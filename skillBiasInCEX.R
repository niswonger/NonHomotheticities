################################################################################
#  WRITTEN BY:    Benji Niswonger - 10 June 2020                               #
#                                                                              #
#  This program uses PUMD from CES Interview to find evidence of skill bias    #
#  income elasticity                                                           #
################################################################################

library(data.table)
library(dplyr)
library(stringr)
library(tidyr)
library(magrittr)
library(readr)
library(ggplot2)
rm(list = ls())

year <- 2007

# Store the directory containing the data files. It is assumed that the FMLI,
# MTBI, and ITBI files are all in the same folder
setwd("~/Documents/Harvard/Research/Dynamic_Inequality_and_Social_Choice/Data/CES/intrvw07")

#################################################################################
# Step 1: Bring in CES DATA
#################################################################################
# Bring in expedniture data
dt.mtbi <- lapply(dir(getwd(), pattern = "^mtbi.*[.]csv$",full.names = TRUE),
                  fread,
                  select = c("NEWID", "COST", "UCC", "REF_YR"),
                  na.strings = c("", ".", "NA")) %>% bind_rows()
setnames(dt.mtbi,old = names(dt.mtbi), new = tolower(names(dt.mtbi)))
dt.mtbi <- dt.mtbi[ref_yr == year, list(cost = sum(cost)), by = list(newid,ucc)]
# Bring in CU data including weights and income rank
# Code from example code 
# Read in and stack the fmli files
fmli <- lapply(
  dir(getwd(), pattern = "^fmli.*[.]csv$", full.names = TRUE),
  fread,
  select = c("NEWID", "FINLWT21", "QINTRVMO", "QINTRVYR", "INCLASS"),
  na.strings = c("", ".", "NA")
) %>% bind_rows() %>%
  setnames(old = names(.), new = tolower(names(.))) %>%
  mutate(qintrvmo = as.numeric(qintrvmo),
    # Generate a calendar-year population weight variable
    popwt = ifelse(
      qintrvmo %in% 1:3 & qintrvyr %in% year,
      (qintrvmo - 1) / 3 * finlwt21 / 4,
      ifelse(
        qintrvyr %in% (year + 1),
        (4 - qintrvmo) / 3 *finlwt21 / 4,
        finlwt21 / 4
      )
    )
  ) %>%
  select(-c(qintrvyr, qintrvmo))
dt.fmli <- data.table(fmli)


dt.mtbi_complete <- dcast(dt.mtbi, newid ~ ucc, value.var = 'cost')
dt.mtbi_complete[is.na(dt.mtbi_complete)]<-0
dt.mtbi_complete <- melt(dt.mtbi_complete, id.vars = 'newid', variable.name = 'ucc', value.name = 'cost')
dt.exp <- merge(dt.fmli,dt.mtbi_complete,by = ('newid'))
  
# Get stub data 
dt.stub <- fread("~/Documents/Harvard/Research/Dynamic_Inequality_and_Social_Choice/Data/CES/stubs/CE-HG-Inter-2017.csv")
dt.stub <- dt.stub[type ==1 & DATA_SECTIONS %in% c('EXPEND','FOOD'), list(level = `Level of Aggregation`, ucc)]
dt.stub[,count := .N, by = list(ucc)]
dt.stub[count>1 ]
# Try looking only at expenditures
dt.exp_2 <- merge(dt.exp,dt.stub, by = 'ucc')
#################################################################################
# Step 2: Get education intensity by NAICS
#################################################################################
dt.acs <- data.table(fread('~/Documents/Harvard/Research/Dynamic_Inequality_and_Social_Choice/Data/ACS/NAICS_EDUC_crosswalk.csv'))
# Focus on employed
dt.acs <- dt.acs[EMPSTAT ==1]
dt.acs[, NAICS := as.numeric(substr(INDNAICS,1,6))]
dt.acs[is.na(NAICS), NAICS := as.numeric(substr(INDNAICS,1,5))]
dt.acs[is.na(NAICS), NAICS := as.numeric(substr(INDNAICS,1,4))]
dt.acs[is.na(NAICS), NAICS := as.numeric(substr(INDNAICS,1,3))]
dt.acs[is.na(NAICS), NAICS := as.numeric(substr(INDNAICS,1,2))]
dt.acs[is.na(NAICS), NAICS := as.numeric(substr(INDNAICS,1,1))]
dt.acs_naics <- dt.acs[, list(college_intensity = sum(PERWT*(EDUC >= 10))/sum(PERWT)), by = list(naics = NAICS)]
#################################################################################
# Step 2.2: Get Lorenz curve data for NAICS
#################################################################################
# Get deciles of income
# dt.acs_per_inc <- dt.acs[,list(INCTOT, PERWT)]
# vec.inc <- unlist(mapply(rep, dt.acs_per_inc$INCTOT,dt.acs_per_inc$PERWT ))
# quant.inc <- quantile(vec.inc, (0:10)/10)
# saveRDS(quant.inc,'~/Documents/Harvard/Research/Dynamic_Inequality_and_Social_Choice/Data/quant_inc.RDS')
quant.inc <- readRDS('~/Documents/Harvard/Research/Dynamic_Inequality_and_Social_Choice/Data/quant_inc.RDS')
# use ugly ifelse equation to get deciles
dt.acs[,decile := cut(INCTOT, breaks = quant.inc, labels = 1:10)]
dt.acs_exp_frac <- dt.acs[, list(exp = sum(PERWT*INCTOT)), by = list(naics = NAICS, decile)]
dt.acs_exp_frac[,frac := exp/sum(exp), by = list(naics)]
# Check Lorenz curve
# ggplot(dt.acs[!is.na(decile), list(exp = sum(PERWT*INCTOT)), 
#               by = list(decile)][order(decile)][,list(decile, curve = cumsum(exp))],
#        aes(decile, curve))+geom_point()
dcast(dt.acs_exp_frac[,list(naics, decile, frac)], )

dt.naics_exp_frac <- dcast(dt.acs_exp_frac[!is.na(decile),list(naics, decile, frac)], naics ~ decile, value.var = 'frac')
# Get total expenditure by NAICS
dt.naics_exp <- dt.acs_exp_frac[,list(exp = sum(exp)), by = list(naics)]

dt.naics_stats <- merge(merge(dt.acs_naics, dt.naics_exp_frac, by = 'naics'),
                        dt.naics_exp, by = 'naics')
#################################################################################
# Step 3: Merge with crosswalk from Xavier Jaravel to get college intensity by UCC
#################################################################################

dt.ucc_naics_cross <- data.table(fread('~/Documents/Harvard/Research/Dynamic_Inequality_and_Social_Choice/Data/CES/ucc_naics_crosswalk.csv'))
# Merge at 6 digit level
dt.ucc_naics_6 <- merge(dt.ucc_naics_cross, dt.naics_stats, by = 'naics', all.x = T)
# Get missing naics at 5 digit level and merge
dt.ucc_naics_5 <- dt.ucc_naics_6[is.na(college_intensity)]
dt.ucc_naics_5[, naics := as.numeric(substr(naics, 1,5))]
dt.ucc_naics_5[,c('college_intensity', 1,2,3,4,5,6,7,8,9,10,'exp') := list(NULL)]
dt.ucc_naics_5 <- merge(dt.ucc_naics_5, dt.naics_stats, by = 'naics', all.x = T)
# Get missing naics at 4 digit level and merge
dt.ucc_naics_4 <- dt.ucc_naics_5[is.na(college_intensity)]
dt.ucc_naics_4[, naics := as.numeric(substr(naics, 1,4))]
dt.ucc_naics_4[,c('college_intensity', 1,2,3,4,5,6,7,8,9,10,'exp') := list(NULL)]
dt.ucc_naics_4 <- merge(dt.ucc_naics_4, dt.naics_stats, by = 'naics', all.x = T)
# Get missing naics at 3 digit level and merge
dt.ucc_naics_3 <- dt.ucc_naics_4[is.na(college_intensity)]
dt.ucc_naics_3[, naics := as.numeric(substr(naics, 1,3))]
dt.ucc_naics_3[,c('college_intensity', 1,2,3,4,5,6,7,8,9,10,'exp') := list(NULL)]
dt.ucc_naics_3 <- merge(dt.ucc_naics_3, dt.naics_stats, by = 'naics', all.x = T)
# Get missing naics at 2 digit level and merge
dt.ucc_naics_2 <- dt.ucc_naics_3[is.na(college_intensity)]
dt.ucc_naics_2[, naics := as.numeric(substr(naics, 1,2))]
dt.ucc_naics_2[,c('college_intensity', 1,2,3,4,5,6,7,8,9,10,'exp') := list(NULL)]
dt.ucc_naics_2 <- merge(dt.ucc_naics_2, dt.naics_stats, by = 'naics', all.x = T)

dt.ucc_ed <- rbind(dt.ucc_naics_6[!is.na(college_intensity)],
                   dt.ucc_naics_5[!is.na(college_intensity)],
                   dt.ucc_naics_4[!is.na(college_intensity)],
                   dt.ucc_naics_3[!is.na(college_intensity)],
                   dt.ucc_naics_2[!is.na(college_intensity)])
# get mean college intensity by ucc treating naics equally
dt.ucc_ed2 <- dt.ucc_ed[,list(college_intensity = sum(college_intensity*exp)/sum(exp),
                              frac_dec_1 = sum(`1`*exp)/sum(exp),
                              frac_dec_2 = sum(`2`*exp)/sum(exp),
                              frac_dec_3 = sum(`3`*exp)/sum(exp),
                              frac_dec_4 = sum(`4`*exp)/sum(exp),
                              frac_dec_5 = sum(`5`*exp)/sum(exp),
                              frac_dec_6 = sum(`6`*exp)/sum(exp),
                              frac_dec_7 = sum(`7`*exp)/sum(exp),
                              frac_dec_8 = sum(`8`*exp)/sum(exp),
                              frac_dec_9 = sum(`9`*exp)/sum(exp),
                              frac_dec_10 = sum(`10`*exp)/sum(exp)), by = list(ucc = as.numeric(ucc), uccname)]
#################################################################################
# Step 4: Get mean expenditure by ucc and income level (inclass) and merge to education data
#################################################################################
dt.exp_inc <- dt.exp_2[,list(exp_ucc = sum(cost*finlwt21)/sum(popwt)), by = list(inclass,ucc = as.numeric(as.character(ucc) ))]
# Check total expenditure by income class
dt.exp_inc[,list(sum(exp_ucc)), by = list(inclass)][order(inclass)]
# Check total expenditure overall
dt.exp_2[,list(exp_ucc = sum(cost*finlwt21)/sum(popwt)), by = list(ucc)][,list(sum(exp_ucc))]
# Check size of different income classes
dt.exp_2[,list(length(unique(newid))), by = list(inclass)][order(inclass)]
# Change groups to balance size 
dt.exp_2[,inclass2 := ifelse(as.numeric(inclass) < 6,1,ifelse(as.numeric(inclass)<9,2,3))]
dt.exp_inc_2 <- dt.exp_2[,list(exp_ucc = sum(cost*finlwt21)/sum(popwt)), by = list(inclass2,ucc = as.numeric(as.character(ucc) ))]

# Get graph with original inclass
dt.inc_ed <- merge(dt.exp_inc, dt.ucc_ed2, by = 'ucc', all.x = T)
dt.results <- dt.inc_ed[exp_ucc >0 & !is.na(college_intensity) ,list( sum(college_intensity*exp_ucc,na.rm = T)/sum(exp_ucc, na.rm = T)), by = list(inclass)][order(inclass)]
ggplot(dt.results, aes(inclass,V1, size = 12))+geom_point() +
  scale_x_discrete('Income Class') +
  scale_y_continuous('Expenditure Weighted Skill Fraction') +
  theme(panel.background = element_blank(),axis.line = element_line(colour = "black"),
        axis.text=element_text(size=15),
        axis.title=element_text(size=20),
        legend.position = 'none')
# Get graph with normalized version
dt.inc_ed2 <- merge(dt.exp_inc_2, dt.ucc_ed2, by = 'ucc', all.x = T)
dt.results <- dt.inc_ed2[exp_ucc >0 & !is.na(college_intensity) ,list( sum(college_intensity*exp_ucc,na.rm = T)/sum(exp_ucc, na.rm = T)), by = list(inclass2)][order(inclass2)]
ggplot(dt.results, aes(inclass2,V1))+geom_point()

# Other miscellaneous information
dt.exp_inc[,list(sum(exp_ucc)/9)]
dt.inc_ed[is.na(uccname), list(sum(exp_ucc)/9), by = list(ucc)][order(V1)]
dt.inc_ed[, list(sum(exp_ucc)/9), by = is.na(uccname)]
dt.inc_ed2[, list(sum(exp_ucc)/3), by = is.na(uccname)]

#################################################################################
# Step 5: Compare Lorenz curves
#################################################################################
dt.exp_dec <- melt(dt.inc_ed, id.vars = c('ucc','inclass','exp_ucc','uccname'), 
                   variable.name = 'decile', value.name = 'frac')[decile != 'college_intensity' & !is.na(frac)]
dt.exp_dec[,decile := as.numeric(substr(decile,10,11))]
dt.exp_dec[,inclass2 := as.factor(ifelse(as.numeric(inclass) < 6,1,ifelse(as.numeric(inclass)<9,2,3)))]
dt.exp_dec_class <- dt.exp_dec[,list(exp = sum(exp_ucc*frac)), by = list(inclass2, decile)]
dt.exp_dec_class[,cumexp := cumsum(exp)/sum(exp), by = list(inclass2)]
ggplot(dt.exp_dec_class, aes(as.factor(decile),cumexp, color = inclass2, size = 10))+geom_point() +
  scale_x_discrete('Decile of Total Income') +
  scale_y_continuous('Cumulative Expenditure Share') + 
  scale_color_manual(values=c('blue','red', 'black'),labels = c("Low Income", "Middle Income",'High Income'), name = NULL) + 
  theme(panel.background = element_blank(),axis.line = element_line(colour = "black"),
        axis.text=element_text(size=15),
        axis.title=element_text(size=20),
        legend.text = element_text(size=15)) +
  guides(size = FALSE)

