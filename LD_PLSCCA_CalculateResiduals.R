#This script residualizes the data to prepare it for various iterations of analyses

library(stats)
library(na.tools)
library(modelr)
library(dplyr)

#LOAD ABCD DATA FILE 
abcd_data_PLSCCA <- readRDS("abcd_data_PLSCCA_trimmed.rds")


#VERIFY VARIABLE TYPES

attach(abcd_data_PLSCCA)
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
which(colnames(abcd_data_PLSCCA)=="smri_vol_cdk_banksstslh")
which(colnames(abcd_data_PLSCCA)=="smri_vol_cdk_insularh")

#subcortical column numbers for the particular 19 regions we include
which(colnames(abcd_data_PLSCCA)=="smri_vol_scs_crbcortexlh")
which(colnames(abcd_data_PLSCCA)=="smri_vol_scs_tplh")
which(colnames(abcd_data_PLSCCA)=="smri_vol_scs_caudatelh")
which(colnames(abcd_data_PLSCCA)=="smri_vol_scs_putamenlh")
which(colnames(abcd_data_PLSCCA)=="smri_vol_scs_pallidumlh")
which(colnames(abcd_data_PLSCCA)=="smri_vol_scs_bstem")
which(colnames(abcd_data_PLSCCA)=="smri_vol_scs_hpuslh")
which(colnames(abcd_data_PLSCCA)=="smri_vol_scs_amygdalalh")
which(colnames(abcd_data_PLSCCA)=="smri_vol_scs_aal")
which(colnames(abcd_data_PLSCCA)=="smri_vol_scs_vedclh")
which(colnames(abcd_data_PLSCCA)=="smri_vol_scs_crbcortexrh")
which(colnames(abcd_data_PLSCCA)=="smri_vol_scs_tprh")
which(colnames(abcd_data_PLSCCA)=="smri_vol_scs_caudaterh")
which(colnames(abcd_data_PLSCCA)=="smri_vol_scs_putamenrh")
which(colnames(abcd_data_PLSCCA)=="smri_vol_scs_pallidumrh")
which(colnames(abcd_data_PLSCCA)=="smri_vol_scs_hpusrh")
which(colnames(abcd_data_PLSCCA)=="smri_vol_scs_amygdalarh")
which(colnames(abcd_data_PLSCCA)=="smri_vol_scs_aar")
which(colnames(abcd_data_PLSCCA)=="smri_vol_scs_vedcrh")

#Factor score column numbers for GEN, EXT, ADHD, INT
which(colnames(abcd_data_PLSCCA)=="GENERAL")
which(colnames(abcd_data_PLSCCA)=="INT")


#CREATE RESIDUALS FOR PRIMARY ANALYSIS

#run residualization
Y.primary <- as.matrix(abcd_data_PLSCCA[c(17:107)])
lm.tores.primary <- lm(Y.primary ~ age + FEMALE + NHWHITE + AFRICAN + HISPANIC + 
                                COIL2 + COIL3 + COIL4 + COIL5, data = abcd_data_PLSCCA)
lm.residuals.primary <- lm.tores.primary$residuals
#add residuals matrix to the data frame and rename the data frame
abcd_data_PLSCCA_wPrimaryRes <- abcd_data_PLSCCA %>% spread_residuals(lm.tores.primary)
#condense new data frame to just sub_num, weight, and residual column
abcd_data_PLSCCA_wPrimaryRes <- abcd_data_PLSCCA_wPrimaryRes[c(grep("subnum_char|wt_NR_mwacs|lm.tores.primary", names(abcd_data_PLSCCA_wPrimaryRes)))]
#omit cases with NAs in row
abcd_data_PLSCCA_wPrimaryRes <- na.omit(abcd_data_PLSCCA_wPrimaryRes)
#save csv file with sub ID, weight, and all residuals
write.csv(abcd_data_PLSCCA_wPrimaryRes,"ABCD_PLSCCA_Residuals_Primary.csv", row.names = FALSE)



#CREATE RESIDUALS FOR SENSITIVITY ANALYSIS (sensitivity analysis is same as primary but regresses some additional covariates)

#run residualization
Y.sensitivity <- as.matrix(abcd_data_PLSCCA[c(17:107)])
lm.tores.sensitivity <- lm(Y.sensitivity ~ age + FEMALE + NHWHITE + AFRICAN + HISPANIC + income + 
                             parent_education + medication +
                         COIL2 + COIL3 + COIL4 + COIL5, data = abcd_data_PLSCCA)
lm.residuals.sensitivity <- lm.tores.sensitivity$residuals
#add residuals matrix to the data frame and rename the data frame
abcd_data_PLSCCA_wSensRes <- abcd_data_PLSCCA %>% spread_residuals(lm.tores.sensitivity)
#condense new data frame to just sub_num, weight, and residual column
abcd_data_PLSCCA_wSensRes <- abcd_data_PLSCCA_wSensRes[c(grep("subnum_char|wt_NR_mwacs|lm.tores.sensitivity", names(abcd_data_PLSCCA_wSensRes)))]
#omit cases with NAs in row
abcd_data_PLSCCA_wSensRes <- na.omit(abcd_data_PLSCCA_wSensRes)
#save csv file with sub ID, weight, and all residuals
write.csv(abcd_data_PLSCCA_wSensRes,"ABCD_PLSCCA_Residuals_Sensitivity.csv", row.names = FALSE)

