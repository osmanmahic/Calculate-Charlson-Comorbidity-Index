###########################################################################
# File name: Calculation.R
# Author: Osman Mahic
# Date: 27-01-2024
# Description: R code for calculating Charlson Comorbidity Index
###########################################################################

library(dplyr)
Final_data1 <- Final_data %>%
group_by(studynr) %>% mutate(c_mi = ifelse(IHD== 1, 1, ifelse(is.na(IHD),
NA, 0)),
c_chf = ifelse(LVD== 1, 1, ifelse(is.na(LVD), NA, 0)),
c_dm = ifelse(DM== 1, 1, ifelse(is.na(DM), NA, 0)),
c_hem = ifelse(PVD4== 1, 2, ifelse(is.na(PVD4), NA, 0)),
c_dm_o = ifelse(DM== 2, 2, ifelse(is.na(DM), NA, 0)),
c_liv = ifelse(liver_modsev== 1, 3, ifelse(is.na(liver_modsev), NA, 0)),
c_meta = ifelse(malign_meta== 1, 6, ifelse(is.na(malign_meta), NA, 0)),
c_aids = ifelse(AIDS== 1, 6, ifelse(is.na(malign_meta), NA, 0)),
c_malign = ifelse(malign== 1, 2, ifelse(is.na(malign), NA, 0)))
Final_data2 <- Final_data1 %>% group_by(studynr) %>% mutate(cci2 = pulm +c_mi +
c_chf + PVD2 + PVD3 + Dementia + SCVD + ulcer + liver_mild + c_dm + c_hem +
c_dm_o + c_malign + c_liv + c_meta + c_aids) %>% mutate(cci1=cci2+2) %>% #Add 2 points to everyone by default
mutate(cci=case_when(cci1<3~ 0, cci1>=3 & cci1 <=4~ 1, cci1 >=5~2 )) #Classify as 'no comorbidity', 'intermediate comorbidity' and 'severe comorbidity'
