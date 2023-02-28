install.packages("gdata")

#LOAD ABCD RDS FILE
abcd_data <- readRDS("Cortical_Volume_04012022.rds")


#CREATING DAT FILE TO USE IN MPLUS TO EXTRACT FACTOR SCORES 

which(colnames(abcd_data)=="subnum_char")
which(colnames(abcd_data)=="PSWEIGHT")
which(colnames(abcd_data)=="FAMID")
which(colnames(abcd_data)=="siten")
which(colnames(abcd_data)=="cbcl01_y1")
which(colnames(abcd_data)=="cbcl03_y1")
which(colnames(abcd_data)=="cbcl04_y1")
which(colnames(abcd_data)=="cbcl07_y1")
which(colnames(abcd_data)=="cbcl09_y1")
which(colnames(abcd_data)=="cbcl10_y1")
which(colnames(abcd_data)=="cbcl13_y1")
which(colnames(abcd_data)=="cbcl15_y1")
which(colnames(abcd_data)=="cbcl16_y1")
which(colnames(abcd_data)=="cbcl17_y1")
which(colnames(abcd_data)=="cbcl19_y1")
which(colnames(abcd_data)=="cbcl22_y1")
which(colnames(abcd_data)=="cbcl23_y1")
which(colnames(abcd_data)=="cbcl25_y1")
which(colnames(abcd_data)=="cbcl26_y1")
which(colnames(abcd_data)=="cbcl27_y1")
which(colnames(abcd_data)=="cbcl28_y1")
which(colnames(abcd_data)=="cbcl30_y1")
which(colnames(abcd_data)=="cbcl31_y1")
which(colnames(abcd_data)=="cbcl32_y1")
which(colnames(abcd_data)=="cbcl33_y1")
which(colnames(abcd_data)=="cbcl34_y1")
which(colnames(abcd_data)=="cbcl35_y1")
which(colnames(abcd_data)=="cbcl37_y1")
which(colnames(abcd_data)=="cbcl39_y1")
which(colnames(abcd_data)=="cbcl41_y1")
which(colnames(abcd_data)=="cbcl43_y1")
which(colnames(abcd_data)=="cbcl46_y1")
which(colnames(abcd_data)=="cbcl50_y1")
which(colnames(abcd_data)=="cbcl51_y1")
which(colnames(abcd_data)=="cbcl52_y1")
which(colnames(abcd_data)=="cbcl56a_y1")
which(colnames(abcd_data)=="cbcl56b_y1")
which(colnames(abcd_data)=="cbcl56c_y1")
which(colnames(abcd_data)=="cbcl56f_y1")
which(colnames(abcd_data)=="cbcl56g_y1")
which(colnames(abcd_data)=="cbcl57_y1")
which(colnames(abcd_data)=="cbcl61_y1")
which(colnames(abcd_data)=="cbcl62_y1")
which(colnames(abcd_data)=="cbcl66_y1")
which(colnames(abcd_data)=="cbcl68_y1")
which(colnames(abcd_data)=="cbcl71_y1")
which(colnames(abcd_data)=="cbcl72_y1")
which(colnames(abcd_data)=="cbcl74_y1")
which(colnames(abcd_data)=="cbcl75_y1")
which(colnames(abcd_data)=="cbcl80_y1")
which(colnames(abcd_data)=="cbcl81_y1")
which(colnames(abcd_data)=="cbcl82_y1")
which(colnames(abcd_data)=="cbcl84_y1")
which(colnames(abcd_data)=="cbcl85_y1")
which(colnames(abcd_data)=="cbcl86_y1")
which(colnames(abcd_data)=="cbcl87_y1")
which(colnames(abcd_data)=="cbcl88_y1")
which(colnames(abcd_data)=="cbcl89_y1")
which(colnames(abcd_data)=="cbcl90_y1")
which(colnames(abcd_data)=="cbcl94_y1")
which(colnames(abcd_data)=="cbcl95_y1")
which(colnames(abcd_data)=="cbcl97_y1")
which(colnames(abcd_data)=="cbcl102_y1")
which(colnames(abcd_data)=="cbcl103_y1")
which(colnames(abcd_data)=="cbcl106_y1")
which(colnames(abcd_data)=="cbcl109_y1")
which(colnames(abcd_data)=="cbcl111_y1")
which(colnames(abcd_data)=="cbcl112_y1")
which(colnames(abcd_data)=="ATTEND_y1")
which(colnames(abcd_data)=="DESTROY_y1")

bifactor_vars <- abcd_data[c(1, 6:7, 10, 51, 53, 54, 57, 59, 60, 63, 65:67, 69, 72, 73, 75:78,
                        80:85, 87, 89, 91, 93, 96, 100:102, 106:108, 111:112, 114, 118:119,
                        123, 125, 128, 129, 131, 132, 137:139, 141:147, 151, 152, 154, 159,
                        160, 163, 166, 168:171)]
write.table(bifactor_vars, "bifactor_variables.dat", row.names = FALSE, col.names = FALSE, na = ".")
bifactor_vars <- read.table("bifactor_variables.dat")
write(colnames(bifactor_vars), "bifactor_variable_names.txt")

#after completing the code above, use the output .dat and .txt file to run the bifactor model mplus script and extract factor scores
#then, compare the extracted scores with a prior version to make sure it ran properly, scores should be similar 

bifactor_variable_names <- c("CBCL01", "CBCL03", "CBCL04", "CBCL07", "CBCL09", "CBCL10", "CBCL13", "CBCL15", "CBCL16", "CBCL17", 
                        "CBCL19", "CBCL22", "CBCL23", "CBCL25", "CBCL26", "CBCL27", "CBCL28", "CBCL30", "CBCL31", "CBCL32", 
                        "CBCL33", "CBCL34", "CBCL35", "CBCL37", "CBCL39", "CBCL41", "CBCL43", "CBCL46", "CBCL50", "CBCL51", 
                        "CBCL52", "CBCL56A", "CBCL56B", "CBCL56C", "CBCL56F", "CBCL56G", "CBCL57", "CBCL61", "CBCL62", 
                        "CBCL66", "CBCL68", "CBCL71", "CBCL72", "CBCL74", "CBCL75", "CBCL80", "CBCL81", "CBCL82", "CBCL84", 
                        "CBCL85", "CBCL86", "CBCL87", "CBCL88", "CBCL89", "CBCL90", "CBCL94", "CBCL95", "CBCL97", "CBCL102", 
                        "CBCL103", "CBCL106", "CBCL109", "CBCL111", "CBCL112", "ATTEND", "DESTROY", "GENERAL", "GENERAL_SE", 
                        "EXT", "EXT_SE", "ADHD", "ADHD_SE", "INT", "INT_SE", "PSWEIGHT", "subnum_char", "SITEN", "FAMID")

bifactor_data_old <- read.table("BifactorScores_3.0.dat", header = FALSE)
colnames(bifactor_data_old) = bifactor_variable_names
View(bifactor_data_old)

bifactor_data_new<- read.table("BifactorScores_PLSCCA.dat", header = FALSE)
colnames(bifactor_data_new) = bifactor_variable_names
View(bifactor_data_new)

mean(bifactor_data_new$GENERAL)
mean(bifactor_data_old$GENERAL)
mean(bifactor_data_new$EXT)
mean(bifactor_data_old$EXT)
mean(bifactor_data_new$ADHD)
mean(bifactor_data_old$ADHD)
mean(bifactor_data_new$INT)
mean(bifactor_data_old$INT)

#CONDENSE MPLUS DAT FILE: delete all data except for subject number and factor scores 
factorscores <- bifactor_data_new[c(grep("subnum_char|^GENERAL$|^EXT$|^ADHD$|^INT$", names(bifactor_data_new)))]

#MERGE THE FACTOR SCORES WITH EACH RDS FILE
abcd_data_merged <- merge(abcd_data, factorscores, by="subnum_char")

#EXCLUDE SAME FAMILY MEMBERS

library(dplyr)

attach(abcd_data_merged)
abcd_data_merged_fam1 <- abcd_data_merged %>%
  group_by(FAMID) %>% sample_n(size = 1) 
#check for duplicate family ids, make sure returns 0
anyDuplicated(abcd_data_merged_fam1$FAMID)

#VERIFY VARIABLE TYPE 
attach(abcd_data_merged_fam1)
is.factor(FEMALE)
is.factor(NHWHITE)
is.factor(HISPANIC)
is.factor(AFRICAN)
is.factor(medication)
is.factor(COIL2)
is.factor(COIL3)
is.factor(COIL4)
is.factor(COIL5)
is.numeric(cbcl01_y1)
is.numeric(income)
is.numeric(parent_education)
is.numeric(age)
is.numeric(smri_vol_cdk_banksstslh)
is.numeric(GENERAL)
is.numeric(EXT)
is.numeric(INT)
is.numeric(ADHD)


#CHANGE VARIABLE TYPES IF NEEDED

abcd_data_merged_fam1[which(colnames(abcd_data_merged_fam1)=="cbcl01_y1"):
                             which(colnames(abcd_data_merged_fam1)=="DESTROY_y1")] %>% mutate_if(is.character, as.numeric) -> 
abcd_data_merged_fam1[which(colnames(abcd_data_merged_fam1)=="cbcl01_y1"):
                         which(colnames(abcd_data_merged_fam1)=="DESTROY_y1")]

abcd_data_merged_fam1[which(colnames(abcd_data_merged_fam1)=="COIL2"):
                       which(colnames(abcd_data_merged_fam1)=="COIL5")] %>% mutate_if(is.character, as.factor) -> 
  abcd_data_merged_fam1[which(colnames(abcd_data_merged_fam1)=="COIL2"):
                         which(colnames(abcd_data_merged_fam1)=="COIL5")]

abcd_data_merged_fam1[which(colnames(abcd_data_merged_fam1)=="medication")] %>% mutate_if(is.character, as.factor) -> 
  abcd_data_merged_fam1[which(colnames(abcd_data_merged_fam1)=="medication")]

abcd_data_merged_fam1[which(colnames(abcd_data_merged_fam1)=="income")] %>% mutate_if(is.character, as.numeric) -> 
  abcd_data_merged_fam1[which(colnames(abcd_data_merged_fam1)=="income")]

#check variable types again in the step above

#save .rds file
saveRDS(abcd_data_merged_fam1, file = "abcd_data_PLSCCA.rds")

#finding column numbers to create trimmed files
which(colnames(abcd_data_merged_fam1)=="subnum_char")
which(colnames(abcd_data_merged_fam1)=="age")
which(colnames(abcd_data_merged_fam1)=="FEMALE")
which(colnames(abcd_data_merged_fam1)=="white")
which(colnames(abcd_data_merged_fam1)=="NHWHITE")
which(colnames(abcd_data_merged_fam1)=="AFRICAN")
which(colnames(abcd_data_merged_fam1)=="HISPANIC")
which(colnames(abcd_data_merged_fam1)=="COIL2")
which(colnames(abcd_data_merged_fam1)=="COIL3")
which(colnames(abcd_data_merged_fam1)=="COIL4")
which(colnames(abcd_data_merged_fam1)=="COIL5")
which(colnames(abcd_data_merged_fam1)=="income")
which(colnames(abcd_data_merged_fam1)=="parent_education")
which(colnames(abcd_data_merged_fam1)=="medication")
which(colnames(abcd_data_merged_fam1)=="wt_NR_mwacs")
which(colnames(abcd_data_merged_fam1)=="wt_NR_cmwacs")
which(colnames(abcd_data_merged_fam1)=="smri_vol_cdk_banksstslh")
which(colnames(abcd_data_merged_fam1)=="smri_vol_cdk_insularh")
which(colnames(abcd_data_merged_fam1)=="smri_vol_scs_crbcortexlh")
which(colnames(abcd_data_merged_fam1)=="smri_vol_scs_tplh")
which(colnames(abcd_data_merged_fam1)=="smri_vol_scs_caudatelh")
which(colnames(abcd_data_merged_fam1)=="smri_vol_scs_putamenlh")
which(colnames(abcd_data_merged_fam1)=="smri_vol_scs_pallidumlh")
which(colnames(abcd_data_merged_fam1)=="smri_vol_scs_bstem")
which(colnames(abcd_data_merged_fam1)=="smri_vol_scs_hpuslh")
which(colnames(abcd_data_merged_fam1)=="smri_vol_scs_amygdalalh")
which(colnames(abcd_data_merged_fam1)=="smri_vol_scs_aal")
which(colnames(abcd_data_merged_fam1)=="smri_vol_scs_vedclh")
which(colnames(abcd_data_merged_fam1)=="smri_vol_scs_crbcortexrh")
which(colnames(abcd_data_merged_fam1)=="smri_vol_scs_tprh")
which(colnames(abcd_data_merged_fam1)=="smri_vol_scs_caudaterh")
which(colnames(abcd_data_merged_fam1)=="smri_vol_scs_putamenrh")
which(colnames(abcd_data_merged_fam1)=="smri_vol_scs_pallidumrh")
which(colnames(abcd_data_merged_fam1)=="smri_vol_scs_hpusrh")
which(colnames(abcd_data_merged_fam1)=="smri_vol_scs_amygdalarh")
which(colnames(abcd_data_merged_fam1)=="smri_vol_scs_aar")
which(colnames(abcd_data_merged_fam1)=="smri_vol_scs_vedcrh")
which(colnames(abcd_data_merged_fam1)=="GENERAL")
which(colnames(abcd_data_merged_fam1)=="INT")

#creating trimmed csv file (age, sex, race, income, parent edu, weights, 87 brain vols, 4 factors)
write.table(abcd_data_merged_fam1[c(1, 3:4, 11:14, 715:719, 721, 724, 1430, 1432, 1437:1504, 1548:1552, 1555:1557, 1560:1561, 1566:1572, 1574:1575, 1995:1998)], 
            "abcd_data_PLSCCA_trimmed.csv", quote = FALSE, sep = ",", row.names=FALSE)


#creating trimmed rds file (age, sex, race, income, parent edu, weights, 87 brain vols, 4 factors)
saveRDS(abcd_data_merged_fam1[c(1, 3:4, 11:14, 715:719, 721, 724, 1430, 1432, 1437:1504, 1548:1552, 1555:1557, 1560:1561, 1566:1572, 1574:1575, 1995:1998)], 
        "abcd_data_PLSCCA_trimmed.rds")


#creating one data set of males only and one of females only and saving as csv and rds files

abcd_data_PLSCCA_males <- abcd_data_merged_fam1[FEMALE==0,]
abcd_data_PLSCCA_females  <- abcd_data_merged_fam1[FEMALE==1,]

write.table(abcd_data_PLSCCA_males[c(1, 3:4, 11:14, 715:719, 721, 724, 1430, 1432, 1437:1504, 1548:1552, 1555:1557, 1560:1561, 1566:1572, 1574:1575, 1995:1998)], 
            "abcd_data_PLSCCA_males_trimmed.csv", quote = FALSE, sep = ",", row.names=FALSE)
saveRDS(abcd_data_PLSCCA_males[c(1, 3:4, 11:14, 715:719, 721, 724, 1430, 1432, 1437:1504, 1548:1552, 1555:1557, 1560:1561, 1566:1572, 1574:1575, 1995:1998)], 
        "abcd_data_PLSCCA_males_trimmed.rds")
write.table(abcd_data_PLSCCA_females[c(1, 3:4, 11:14, 715:719, 721, 724, 1430, 1432, 1437:1504, 1548:1552, 1555:1557, 1560:1561, 1566:1572, 1574:1575, 1995:1998)], 
            "abcd_data_PLSCCA_females_trimmed.csv", quote = FALSE, sep = ",", row.names=FALSE)
saveRDS(abcd_data_PLSCCA_females[c(1, 3:4, 11:14, 715:719, 721, 724, 1430, 1432, 1437:1504, 1548:1552, 1555:1557, 1560:1561, 1566:1572, 1574:1575, 1995:1998)], 
        "abcd_data_PLSCCA_females_trimmed.rds")


