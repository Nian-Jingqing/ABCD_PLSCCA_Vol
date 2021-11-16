#This script residualizes the data to prepare it for various iterations of analyses

library(stats)
library(na.tools)
library(modelr)
library(dplyr)

#LOAD ABCD RDS FILES 
ABCD_PLSCCA_Group1 <- readRDS("ABCD_PLSCCA_fam1_Group1_trimmed.rds")
ABCD_PLSCCA_Group2 <- readRDS("ABCD_PLSCCA_fam1_Group2_trimmed.rds")

#VERIFY VARIABLE TYPES

attach(ABCD_PLSCCA_Group1)
is.factor(FEMALE)
is.factor(NHWHITE)
is.factor(HISPANIC)
is.factor(AFRICAN)
is.factor(medication)
is.factor(COIL2)
is.factor(COIL3)
is.factor(COIL4)
is.factor(COIL5)
is.numeric(income)
is.numeric(parent_education)
is.numeric(age)
is.numeric(smri_vol_cdk_banksstslh)
is.numeric(GENERAL)
is.numeric(EXT)
is.numeric(INT)
is.numeric(ADHD)

attach(ABCD_PLSCCA_Group2)
is.factor(FEMALE)
is.factor(NHWHITE)
is.factor(HISPANIC)
is.factor(AFRICAN)
is.factor(medication)
is.factor(COIL2)
is.factor(COIL3)
is.factor(COIL4)
is.factor(COIL5)
is.numeric(income)
is.numeric(parent_education)
is.numeric(age)
is.numeric(smri_vol_cdk_banksstslh)
is.numeric(GENERAL)
is.numeric(EXT)
is.numeric(INT)
is.numeric(ADHD)


#CHECK COLUMN NUMBERS FOR BRAIN REGIONS AND FACTOR SCORES

#cortical column numbers (all 68 regions in a row with the start and end points in the two lines below)
which(colnames(ABCD_PLSCCA_Group1)=="smri_vol_cdk_banksstslh")
which(colnames(ABCD_PLSCCA_Group1)=="smri_vol_cdk_insularh")

#subcortical column numbers for the particular 19 regions we include
which(colnames(ABCD_PLSCCA_Group1)=="smri_vol_scs_crbcortexlh")
which(colnames(ABCD_PLSCCA_Group1)=="smri_vol_scs_tplh")
which(colnames(ABCD_PLSCCA_Group1)=="smri_vol_scs_caudatelh")
which(colnames(ABCD_PLSCCA_Group1)=="smri_vol_scs_putamenlh")
which(colnames(ABCD_PLSCCA_Group1)=="smri_vol_scs_pallidumlh")
which(colnames(ABCD_PLSCCA_Group1)=="smri_vol_scs_bstem")
which(colnames(ABCD_PLSCCA_Group1)=="smri_vol_scs_hpuslh")
which(colnames(ABCD_PLSCCA_Group1)=="smri_vol_scs_amygdalalh")
which(colnames(ABCD_PLSCCA_Group1)=="smri_vol_scs_aal")
which(colnames(ABCD_PLSCCA_Group1)=="smri_vol_scs_vedclh")
which(colnames(ABCD_PLSCCA_Group1)=="smri_vol_scs_crbcortexrh")
which(colnames(ABCD_PLSCCA_Group1)=="smri_vol_scs_tprh")
which(colnames(ABCD_PLSCCA_Group1)=="smri_vol_scs_caudaterh")
which(colnames(ABCD_PLSCCA_Group1)=="smri_vol_scs_putamenrh")
which(colnames(ABCD_PLSCCA_Group1)=="smri_vol_scs_pallidumrh")
which(colnames(ABCD_PLSCCA_Group1)=="smri_vol_scs_hpusrh")
which(colnames(ABCD_PLSCCA_Group1)=="smri_vol_scs_amygdalarh")
which(colnames(ABCD_PLSCCA_Group1)=="smri_vol_scs_aar")
which(colnames(ABCD_PLSCCA_Group1)=="smri_vol_scs_vedcrh")

#Factor score column numbers for GEN, EXT, ADHD, INT
which(colnames(ABCD_PLSCCA_Group1)=="GENERAL")
which(colnames(ABCD_PLSCCA_Group1)=="INT")



#CREATE RESIDUALS FOR PRIMARY ANALYSIS


#Group 1
#run residualization
Y.primarygroup1 <- as.matrix(ABCD_PLSCCA_Group1[c(17:107)])
lm.tores.primarygroup1 <- lm(Y.primarygroup1 ~ age + FEMALE + NHWHITE + AFRICAN + HISPANIC + 
                                COIL2 + COIL3 + COIL4 + COIL5, data = ABCD_PLSCCA_Group1)
lm.residuals.primarygroup1 <- lm.tores.primarygroup1$residuals
#add residuals matrix to the data frame and rename the data frame
ABCD_PLSCCA_Group1_wPrimaryRes <- ABCD_PLSCCA_Group1 %>% spread_residuals(lm.tores.primarygroup1)
#condense new data frame to just sub_num, weight, and residual column
ABCD_PLSCCA_Group1_wPrimaryRes <- ABCD_PLSCCA_Group1_wPrimaryRes[c(grep("subnum_char|wt_NR_mwacs|lm.tores.primarygroup1", names(ABCD_PLSCCA_Group1_wPrimaryRes)))]
#omit cases with NAs in row
ABCD_PLSCCA_Group1_wPrimaryRes <- na.omit(ABCD_PLSCCA_Group1_wPrimaryRes)
#save csv file with sub ID, weight, and all residuals
write.csv(ABCD_PLSCCA_Group1_wPrimaryRes,"ABCD_PLSCCA_Residuals_PrimaryGroup1.csv", row.names = FALSE)


#Group 2
Y.primarygroup2 <- as.matrix(ABCD_PLSCCA_Group2[c(17:107)])
lm.tores.primarygroup2 <- lm( Y.primarygroup2 ~ age + FEMALE + NHWHITE + AFRICAN + HISPANIC + 
                                COIL2 + COIL3 + COIL4 + COIL5, data = ABCD_PLSCCA_Group2)
lm.residuals.primarygroup2 <- lm.tores.primarygroup2$residuals
#add residuals matrix to the data frame and rename the data frame
ABCD_PLSCCA_Group2_wPrimaryRes <- ABCD_PLSCCA_Group2 %>% spread_residuals(lm.tores.primarygroup2)
#condense new data frame to just sub_num, weight, and residual column
ABCD_PLSCCA_Group2_wPrimaryRes <- ABCD_PLSCCA_Group2_wPrimaryRes[c(grep("subnum_char|wt_NR_mwacs|lm.tores.primarygroup2", names(ABCD_PLSCCA_Group2_wPrimaryRes)))]
#omit cases with NAs in row
ABCD_PLSCCA_Group2_wPrimaryRes <- na.omit(ABCD_PLSCCA_Group2_wPrimaryRes)
#save csv file with sub ID, weight, and all residuals
write.csv(ABCD_PLSCCA_Group2_wPrimaryRes,"ABCD_PLSCCA_Residuals_PrimaryGroup2.csv", row.names = FALSE)




#CREATE RESIDUALS FOR SENSITIVITY ANALYSES


#Add income, parent education, and medication in Group 1
Y.sensgroup1 <- as.matrix(ABCD_PLSCCA_Group1[c(17:107)])
lm.tores.sensgroup1 <- lm( Y.sensgroup1 ~ age + FEMALE + NHWHITE + AFRICAN + HISPANIC + 
                             COIL2 + COIL3 + COIL4 + COIL5 + income + parent_education + medication, 
                           data = ABCD_PLSCCA_Group1)
lm.residuals.sensgroup1 <- lm.tores.sensgroup1$residuals
#add residuals matrix to the data frame and rename the data frame
ABCD_PLSCCA_wSensRes <- ABCD_PLSCCA_Group1 %>% spread_residuals(lm.tores.sensgroup1)
#condense new data frame to just sub_num, weight, and residual column
ABCD_PLSCCA_wSensRes <- ABCD_PLSCCA_wSensRes[c(grep("subnum_char|wt_NR_mwacs|lm.tores.sensgroup1", names(ABCD_PLSCCA_wSensRes)))]
#omit cases with NAs in row
ABCD_PLSCCA_wSensRes <- na.omit(ABCD_PLSCCA_wSensRes)
#save csv file with sub ID, weight, and all residuals
write.csv(ABCD_PLSCCA_wSensRes,"ABCD_PLSCCA_Residuals_SensitivityGroup1.csv", row.names = FALSE)


