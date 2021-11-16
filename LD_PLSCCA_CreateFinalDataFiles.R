#This script will create two matched groups for analysis and exclude family members

########### FIRST SAMPLE #############

#LOAD ABCD RDS FILES
abcd_data_match1 <- readRDS("Cortical_Volume_match_1.rds")


#LOAD MPLUS DAT FILE WITH EXTRACTED FACTORS
bifactor_variables <- c("CBCL01", "CBCL03", "CBCL04", "CBCL07", "CBCL09", "CBCL10", "CBCL13", "CBCL15", "CBCL16", "CBCL17", 
                        "CBCL19", "CBCL22", "CBCL23", "CBCL25", "CBCL26", "CBCL27", "CBCL28", "CBCL30", "CBCL31", "CBCL32", 
                        "CBCL33", "CBCL34", "CBCL35", "CBCL37", "CBCL39", "CBCL41", "CBCL43", "CBCL46", "CBCL50", "CBCL51", 
                        "CBCL52", "CBCL56A", "CBCL56B", "CBCL56C", "CBCL56F", "CBCL56G", "CBCL57", "CBCL61", "CBCL62", 
                        "CBCL66", "CBCL68", "CBCL71", "CBCL72", "CBCL74", "CBCL75", "CBCL80", "CBCL81", "CBCL82", "CBCL84", 
                        "CBCL85", "CBCL86", "CBCL87", "CBCL88", "CBCL89", "CBCL90", "CBCL94", "CBCL95", "CBCL97", "CBCL102", 
                        "CBCL103", "CBCL106", "CBCL109", "CBCL111", "CBCL112", "ATTEND", "DESTROY", "GENERAL", "GENERAL_SE", 
                        "EXT", "EXT_SE", "ADHD", "ADHD_SE", "INT", "INT_SE", "PSWEIGHT", "subnum_char", "SITEN", "FAMID")
bifactor_data <- read.table("BifactorScores_PLSCCA.dat", header = FALSE)
colnames(bifactor_data) = bifactor_variables

#CONDENSE MPLUS DAT FILE: delete all data except for subject number and factor scores 
factorscores <- bifactor_data[c(grep("subnum_char|^GENERAL$|^EXT$|^ADHD$|^INT$", names(bifactor_data)))]

#MERGE THE FACTOR SCORES WITH EACH RDS FILE
abcd_data_match1_merged <-merge(abcd_data_match1, factorscores, by="subnum_char") 

#SAVE RDS IF NEEDED
#saveRDS(abcd_data_match1_merged, file = "abcd_data_match1_wfactors.rds")


########### SECOND SAMPLE #############

#LOAD ABCD RDS FILES
abcd_data_match2 <- readRDS("Cortical_Volume_match_2.rds")

#MERGE THE FACTOR SCORES
abcd_data_match2_merged <-merge(abcd_data_match2, factorscores, by="subnum_char") 

#SAVE RDS IF NEEDED
#saveRDS(abcd_data_match2_merged, file = "abcd_data_match2_wfactors.rds")


########### THIRD SAMPLE #############

#LOAD ABCD RDS FILES
abcd_data_match3 <- readRDS("Cortical_Volume_match_3.rds")

#MERGE THE FACTOR SCORES WITH EACH RDS FILE
abcd_data_match3_merged <-merge(abcd_data_match3, factorscores, by="subnum_char") 


#SAVE RDS IF NEEDED
#saveRDS(abcd_data_match3_merged, file = "abcd_data_match3_wfactors.rds")

##SPLIT MATCHED GROUP 3 INTO TWO SUBSETS, RANDOMLY##

# Split Data into Training and Test samples in R
sample_size = floor(0.5*nrow(abcd_data_match3_merged))
set.seed(777)

# randomly split data in r
picked = sample(seq_len(nrow(abcd_data_match3_merged)),size = sample_size)
development =abcd_data_match3_merged[picked,]
holdout =abcd_data_match3_merged[-picked,]

#Check split and make sure it looks correct
#View(development)
#View(holdout)
#View(abcd_data_match3_merged)

#MERGE EACH HALF WITH GROUPS 1 AND 2
ABCD_CCA_Group1 = merge(abcd_data_match1_merged, development, all=TRUE)
ABCD_CCA_Group2 = merge(abcd_data_match2_merged, holdout, all=TRUE)

#Create variable for group number in each file
ABCD_CCA_Group1$MatchGroupNum <- as.integer("0")
ABCD_CCA_Group2$MatchGroupNum <- as.integer("1")

#EXCLUDE SAME FAMILY MEMBERS

library(dplyr)

attach(ABCD_CCA_Group1)
ABCD_CCA_fam1_Group1 <- ABCD_CCA_Group1 %>%
  group_by(FAMID) %>% sample_n(size = 1) 

attach(ABCD_CCA_Group2)
ABCD_CCA_fam1_Group2 <- ABCD_CCA_Group2 %>%
  group_by(FAMID) %>% sample_n(size = 1) 


#VERIFY VARIABLE TYPE 
attach(ABCD_CCA_fam1_Group1)
is.factor(FEMALE)
is.factor(NHWHITE)
is.factor(HISPANIC)
is.factor(AFRICAN)
is.factor(medication)
is.factor(COIL2)
is.factor(COIL3)
is.factor(COIL4)
is.factor(COIL5)
is.numeric(cbcl_q01_p)
is.numeric(income)
is.numeric(parent_education)
is.numeric(age)
is.numeric(smri_vol_cdk_banksstslh)
is.numeric(GENERAL)
is.numeric(EXT)
is.numeric(INT)
is.numeric(ADHD)

attach(ABCD_CCA_fam1_Group2)
is.factor(FEMALE)
is.factor(NHWHITE)
is.factor(HISPANIC)
is.factor(AFRICAN)
is.factor(medication)
is.factor(COIL2)
is.factor(COIL3)
is.factor(COIL4)
is.factor(COIL5)
is.numeric(cbcl_q01_p)
is.numeric(income)
is.numeric(parent_education)
is.numeric(age)
is.numeric(smri_vol_cdk_banksstslh)
is.numeric(GENERAL)
is.numeric(EXT)
is.numeric(INT)
is.numeric(ADHD)

#MAKE CBCL ITEMS NUMERIC AND MAKE COIL VARIABLES AND MEDICATION VARIABLE FACTORS

ABCD_CCA_fam1_Group1[which(colnames(ABCD_CCA_fam1_Group1)=="cbcl_q01_p"):
                             which(colnames(ABCD_CCA_fam1_Group1)=="DESTROY")] %>% mutate_if(is.character, as.numeric) -> 
ABCD_CCA_fam1_Group1[which(colnames(ABCD_CCA_fam1_Group1)=="cbcl_q01_p"):
                         which(colnames(ABCD_CCA_fam1_Group1)=="DESTROY")]

ABCD_CCA_fam1_Group2[which(colnames(ABCD_CCA_fam1_Group2)=="cbcl_q01_p"):
                       which(colnames(ABCD_CCA_fam1_Group2)=="DESTROY")] %>% mutate_if(is.character, as.numeric) -> 
ABCD_CCA_fam1_Group2[which(colnames(ABCD_CCA_fam1_Group2)=="cbcl_q01_p"):
                         which(colnames(ABCD_CCA_fam1_Group2)=="DESTROY")]

ABCD_CCA_fam1_Group1[which(colnames(ABCD_CCA_fam1_Group1)=="COIL2"):
                       which(colnames(ABCD_CCA_fam1_Group1)=="COIL5")] %>% mutate_if(is.character, as.factor) -> 
  ABCD_CCA_fam1_Group1[which(colnames(ABCD_CCA_fam1_Group1)=="COIL2"):
                         which(colnames(ABCD_CCA_fam1_Group1)=="COIL5")]

ABCD_CCA_fam1_Group2[which(colnames(ABCD_CCA_fam1_Group2)=="COIL2"):
                       which(colnames(ABCD_CCA_fam1_Group2)=="COIL5")] %>% mutate_if(is.character, as.factor) -> 
  ABCD_CCA_fam1_Group2[which(colnames(ABCD_CCA_fam1_Group2)=="COIL2"):
                         which(colnames(ABCD_CCA_fam1_Group2)=="COIL5")]

ABCD_CCA_fam1_Group1[which(colnames(ABCD_CCA_fam1_Group1)=="medication")] %>% mutate_if(is.character, as.factor) -> 
  ABCD_CCA_fam1_Group1[which(colnames(ABCD_CCA_fam1_Group1)=="medication")]

ABCD_CCA_fam1_Group2[which(colnames(ABCD_CCA_fam1_Group2)=="medication")] %>% mutate_if(is.character, as.factor) -> 
  ABCD_CCA_fam1_Group2[which(colnames(ABCD_CCA_fam1_Group2)=="medication")]

#check if cbcl items are numeric and medication a factor
is.numeric(ABCD_CCA_fam1_Group1$cbcl_q01_p)
is.numeric(ABCD_CCA_fam1_Group2$cbcl_q01_p)
is.factor(ABCD_CCA_fam1_Group1$medication)
is.factor(ABCD_CCA_fam1_Group2$medication)
is.factor(ABCD_CCA_fam1_Group1$COIL2)
is.factor(ABCD_CCA_fam1_Group2$COIL2)
is.factor(ABCD_CCA_fam1_Group1$COIL3)
is.factor(ABCD_CCA_fam1_Group2$COIL3)
is.factor(ABCD_CCA_fam1_Group1$COIL4)
is.factor(ABCD_CCA_fam1_Group2$COIL4)
is.factor(ABCD_CCA_fam1_Group1$COIL5)
is.factor(ABCD_CCA_fam1_Group2$COIL5)

#save final two matched groups and the two groups combined into .rds files
saveRDS(ABCD_CCA_fam1_Group1, file = "ABCD_PLSCCA_fam1_Group1.rds")
saveRDS(ABCD_CCA_fam1_Group2, file = "ABCD_PLSCCA_fam1_Group2.rds")
ABCD_CCA_BothGroups= merge(ABCD_CCA_fam1_Group1, ABCD_CCA_fam1_Group2, all=TRUE)
saveRDS(ABCD_CCA_BothGroups, file = "ABCD_PLSCCA_fam1_BothGroups.rds")

#load RDS files
ABCD_CCA_fam1_Group1 <- readRDS("ABCD_PLSCCA_fam1_Group1.rds")
ABCD_CCA_fam1_Group2 <- readRDS("ABCD_PLSCCA_fam1_Group2.rds")
ABCD_CCA_fam1_BothGroups <- readRDS("ABCD_PLSCCA_fam1_BothGroups.rds")

#finding column numbers to create trimmed files
which(colnames(ABCD_CCA_fam1_Group1)=="subnum_char")
which(colnames(ABCD_CCA_fam1_Group1)=="age")
which(colnames(ABCD_CCA_fam1_Group1)=="FEMALE")
which(colnames(ABCD_CCA_fam1_Group1)=="white")
which(colnames(ABCD_CCA_fam1_Group1)=="NHWHITE")
which(colnames(ABCD_CCA_fam1_Group1)=="AFRICAN")
which(colnames(ABCD_CCA_fam1_Group1)=="HISPANIC")
which(colnames(ABCD_CCA_fam1_Group1)=="COIL2")
which(colnames(ABCD_CCA_fam1_Group1)=="COIL3")
which(colnames(ABCD_CCA_fam1_Group1)=="COIL4")
which(colnames(ABCD_CCA_fam1_Group1)=="COIL5")
which(colnames(ABCD_CCA_fam1_Group1)=="income")
which(colnames(ABCD_CCA_fam1_Group1)=="parent_education")
which(colnames(ABCD_CCA_fam1_Group1)=="medication")
which(colnames(ABCD_CCA_fam1_Group1)=="wt_NR_mwacs")
which(colnames(ABCD_CCA_fam1_Group1)=="wt_NR_cmwacs")
which(colnames(ABCD_CCA_fam1_Group1)=="smri_vol_cdk_banksstslh")
which(colnames(ABCD_CCA_fam1_Group1)=="smri_vol_cdk_insularh")
which(colnames(ABCD_CCA_fam1_Group1)=="smri_vol_scs_crbcortexlh")
which(colnames(ABCD_CCA_fam1_Group1)=="smri_vol_scs_tplh")
which(colnames(ABCD_CCA_fam1_Group1)=="smri_vol_scs_caudatelh")
which(colnames(ABCD_CCA_fam1_Group1)=="smri_vol_scs_putamenlh")
which(colnames(ABCD_CCA_fam1_Group1)=="smri_vol_scs_pallidumlh")
which(colnames(ABCD_CCA_fam1_Group1)=="smri_vol_scs_bstem")
which(colnames(ABCD_CCA_fam1_Group1)=="smri_vol_scs_hpuslh")
which(colnames(ABCD_CCA_fam1_Group1)=="smri_vol_scs_amygdalalh")
which(colnames(ABCD_CCA_fam1_Group1)=="smri_vol_scs_aal")
which(colnames(ABCD_CCA_fam1_Group1)=="smri_vol_scs_vedclh")
which(colnames(ABCD_CCA_fam1_Group1)=="smri_vol_scs_crbcortexrh")
which(colnames(ABCD_CCA_fam1_Group1)=="smri_vol_scs_tprh")
which(colnames(ABCD_CCA_fam1_Group1)=="smri_vol_scs_caudaterh")
which(colnames(ABCD_CCA_fam1_Group1)=="smri_vol_scs_putamenrh")
which(colnames(ABCD_CCA_fam1_Group1)=="smri_vol_scs_pallidumrh")
which(colnames(ABCD_CCA_fam1_Group1)=="smri_vol_scs_hpusrh")
which(colnames(ABCD_CCA_fam1_Group1)=="smri_vol_scs_amygdalarh")
which(colnames(ABCD_CCA_fam1_Group1)=="smri_vol_scs_aar")
which(colnames(ABCD_CCA_fam1_Group1)=="smri_vol_scs_vedcrh")
which(colnames(ABCD_CCA_fam1_Group1)=="GENERAL")
which(colnames(ABCD_CCA_fam1_Group1)=="INT")
which(colnames(ABCD_CCA_fam1_Group1)=="MatchGroupNum")

#creating trimmed csv files for each group (age, sex, race, income, parent edu, weights, 87 brain vols, 4 factors, group number)
write.table(ABCD_CCA_fam1_Group1[c(1, 3:4, 11:14, 127:131, 133, 136, 745, 747, 752:819, 863:867, 870:872, 875:876, 881:887, 889:890, 1310:1313, 1314)], 
                                   "ABCD_PLSCCA_fam1_Group1_trimmed.csv", quote = FALSE, sep = ",", row.names=FALSE)
write.table(ABCD_CCA_fam1_Group2[c(1, 3:4, 11:14, 127:131, 133, 136, 745, 747, 752:819, 863:867, 870:872, 875:876, 881:887, 889:890, 1310:1313, 1314)], 
                                 "ABCD_PLSCCA_fam1_Group2_trimmed.csv", quote = FALSE, sep = ",", row.names=FALSE)
write.table(ABCD_CCA_fam1_BothGroups[c(1, 3:4, 11:14, 127:131, 133, 136, 745, 747, 752:819, 863:867, 870:872, 875:876, 881:887, 889:890, 1310:1313, 1314)], 
            "ABCD_PLSCCA_fam1_BothGroups_trimmed.csv", quote = FALSE, sep = ",", row.names=FALSE)

#creating trimmed rds files for each group (age, sex, race, income, parent edu, weights, 87 brain vols, 4 factors, group number)
saveRDS(ABCD_CCA_fam1_Group1[c(1, 3:4, 11:14, 127:131, 133, 136, 745, 747, 752:819, 863:867, 870:872, 875:876, 881:887, 889:890, 1310:1313, 1314)], 
            "ABCD_PLSCCA_fam1_Group1_trimmed.rds")
saveRDS(ABCD_CCA_fam1_Group2[c(1, 3:4, 11:14, 127:131, 133, 136, 745, 747, 752:819, 863:867, 870:872, 875:876, 881:887, 889:890, 1310:1313, 1314)], 
            "ABCD_PLSCCA_fam1_Group2_trimmed.rds")
saveRDS(ABCD_CCA_fam1_BothGroups[c(1, 3:4, 11:14, 127:131, 133, 136, 745, 747, 752:819, 863:867, 870:872, 875:876, 881:887, 889:890, 1310:1313, 1314)], 
            "ABCD_PLSCCA_fam1_BothGroups_trimmed.rds")


