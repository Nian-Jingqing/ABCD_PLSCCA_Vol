#Evaluating demographic breakdowns and group differences across ABCD matched samples

#LOAD ABCD RDS FILES (THESE ARE TWO MATCHED GROUPS WITH FAMILY MEMBERS EXCLUDED)
ABCD_CCA_Group1 <- readRDS("ABCD_PLSCCA_fam1_Group1_trimmed.rds")
ABCD_CCA_Group2 <- readRDS("ABCD_PLSCCA_fam1_Group2_trimmed.rds")
full_data <- readRDS("ABCD_PLSCCA_fam1_BothGroups_trimmed.rds")

#verify N for each group
match1_number <- length(which(full_data$MatchGroupNum==0))
match1_number
match2_number <- length(which(full_data$MatchGroupNum==1))
match2_number

#VERIFY VARIABLE TYPES

attach(full_data)

is.factor(FEMALE)
is.factor(NHWHITE)
is.factor(HISPANIC)
is.factor(AFRICAN)
is.numeric(income)
is.numeric(parent_education)
is.numeric(age)
is.numeric(smri_vol_cdk_banksstslh)
is.numeric(GENERAL)
is.numeric(EXT)
is.numeric(INT)
is.numeric(ADHD)

##CHECK GROUP DIFFERENCES ON DEM VARS AND FACTOR SCORES##

#logistic regression for binary outcome (sex, race)
sexLm <- glm(FEMALE ~ MatchGroupNum, data=full_data, family=binomial)
sexLmSumm <- summary(sexLm)
sexLmSumm

raceLm <- glm(white ~ MatchGroupNum, data=full_data, family=binomial)
raceLmSumm <- summary(raceLm)
raceLmSumm

#regression for continuous outcome (age, income, parent_education)
ageLm <- lm(age ~ MatchGroupNum, data=full_data)
ageLmSumm <- summary(ageLm)
ageLmSumm

incomeLm <- lm(income ~ MatchGroupNum, data=full_data)
incomeLmSumm <- summary(incomeLm)
incomeLmSumm

parent_educationLm <- lm(parent_education ~ MatchGroupNum, data=full_data)
parent_educationLmSumm <- summary(parent_educationLm)
parent_educationLmSumm

GENERALLm <- lm(GENERAL ~ MatchGroupNum, data=full_data)
GENERALLmSumm <- summary(GENERALLm)
GENERALLmSumm

EXTLm <- lm(EXT ~ MatchGroupNum, data=full_data)
EXTLmSumm <- summary(EXTLm)
EXTLmSumm

ADHDLm <- lm(ADHD ~ MatchGroupNum, data=full_data)
ADHDLmSumm <- summary(ADHDLm)
ADHDLmSumm

INTLm <- lm(INT ~ MatchGroupNum, data=full_data)
INTLmSumm <- summary(INTLm)
INTLmSumm


########################################################
#################FULL DATA DEMOGRAPHICS#################
########################################################

#Total sample means
meanAge_total <- mean(full_data$age)
meanAge_total

#Total sample sd
sdAge_total <- sd(full_data$age)
sdAge_total

#Total number of males and females (0=male, 1=female)
sexTable_total <- table(full_data$FEMALE)
sexTable_total

#Percentage of females
female_number <-length(which(full_data$FEMALE==1))
percentfemale <- female_number/8362
female_number
percentfemale
male_number <-length(which(full_data$FEMALE==0))
percentmale <- male_number/8362
male_number
percentmale

#Total number of whites (1=non-hispanic white, 0=not)
whiteTable_total <- table(full_data$NHWHITE)
whiteTable_total

#Total number of hispanic (1=hispanic, 0=not)
hispanicTable_total <- table(full_data$HISPANIC)
hispanicTable_total

#Total number of african (1=african, 0=not)
africanTable_total <- table(full_data$AFRICAN)
africanTable_total

#Total number of other (1=other, 0=not)
otherraceTable_total <- table(full_data$OTHER)
otherraceTable_total

#Percentage of each race group
white_number <-length(which(full_data$NHWHITE==1))
percentwhite <- white_number/8362
percentwhite
hispanic_number <-length(which(full_data$HISPANIC==1))
percenthispanic <- hispanic_number/8362
percenthispanic
african_number <-length(which(full_data$AFRICAN==1))
percentafrican <- african_number/8362
percentafrican
otherrace_number <-length(which(full_data$OTHER==1))
percentotherrace <- otherrace_number/8362
percentotherrace

#Income Summary table
income_total<-table(full_data$income)
income_total

#Percentage of each income response
lessthanfivek_number <-length(which(full_data$income==1))
lessthanfivek_number
percentlessthanfivek <- lessthanfivek_number/8362
percentlessthanfivek
fivektotwelvek_number <-length(which(full_data$income==2))
fivektotwelvek_number
percentfivektotwelvek <- fivektotwelvek_number/8362
percentfivektotwelvek
twelvektosixteenk_number <-length(which(full_data$income==3))
twelvektosixteenk_number
percenttwelvektosixteenk <- twelvektosixteenk_number/8362
percenttwelvektosixteenk
sixteenktotwentyfivek_number <-length(which(full_data$income==4))
sixteenktotwentyfivek_number
percentsixteenktotwentyfivek <- sixteenktotwentyfivek_number/8362
percentsixteenktotwentyfivek
twentyfivektothirtyfivek_number <-length(which(full_data$income==5))
twentyfivektothirtyfivek_number
percenttwentyfivektothirtyfivek <- twentyfivektothirtyfivek_number/8362
percenttwentyfivektothirtyfivek
thirtyfivektofiftyk_number <-length(which(full_data$income==6))
thirtyfivektofiftyk_number
percentthirtyfivektofiftyk <- thirtyfivektofiftyk_number/8362
percentthirtyfivektofiftyk 
fiftyktoseventyfivek_number <-length(which(full_data$income==7))
fiftyktoseventyfivek_number
percentfiftyktoseventyfivek <- fiftyktoseventyfivek_number/8362
percentfiftyktoseventyfivek
seventyfivektohundredk_number <-length(which(full_data$income==8))
seventyfivektohundredk_number
percentseventyfivektohundredk <- seventyfivektohundredk_number/8362
percentseventyfivektohundredk
hundredktotwohundrek_number <-length(which(full_data$income==9))
hundredktotwohundrek_number
percenthundredktotwohundrek <- hundredktotwohundrek_number/8362
percenthundredktotwohundrek
overtwohundredk_number <-length(which(full_data$income==10))
overtwohundredk_number
percentovertwohundredk <- overtwohundredk_number/8362
percentovertwohundredk
missingincome_number <-length(which(is.na(full_data$income)))
missingincome_number
percentmissingincome <- missingincome_number/8362
percentmissingincome



attach(full_data)

#Parental Education Summary table
parentaledu_total<-table(parent_education)
parentaledu_total

#Percentage of each parental education response
neverorkindergarten_number <-length(which(parent_education==0))
neverorkindergarten_number

first_number <-length(which(parent_education==1))
first_number

second_number <-length(which(parent_education==2))
second_number

third_number <-length(which(parent_education==3))
third_number

fourth_number <-length(which(parent_education==4))
fourth_number

fifth_number <-length(which(parent_education==5))
fifth_number

sixth_number <-length(which(parent_education==6))
sixth_number

seventh_number <-length(which(parent_education==7))
seventh_number

eighth_number <-length(which(parent_education==8))
eighth_number

ninth_number <-length(which(parent_education==9))
ninth_number

tenth_number <-length(which(parent_education==10))
tenth_number

eleventh_number <-length(which(parent_education==11))
eleventh_number


nodegree_number <- (neverorkindergarten_number + first_number + second_number + third_number +
                      fourth_number + fifth_number + sixth_number + seventh_number + eighth_number +
                      ninth_number + tenth_number + eleventh_number)
nodegree_number
percentnodegree <- (neverorkindergarten_number + first_number + second_number + third_number +
                      fourth_number + fifth_number + sixth_number + seventh_number + eighth_number +
                      ninth_number + tenth_number + eleventh_number)/8362
percentnodegree

highschoolgradGED_number <-length(which(parent_education==12))
highschoolgradGED_number
percenthighschoolgradGED <- highschoolgradGED_number/8362
percenthighschoolgradGED

somecollege_number <-length(which(parent_education==13))
somecollege_number
percentsomecoll <- somecollege_number/8362
percentsomecoll

associate_number <-length(which(parent_education==14))
associate_number
percentassociate <- associate_number/8362
percentassociate

bachelors_number <-length(which(parent_education==16))
bachelors_number
percentbach <- bachelors_number/8362
percentbach

masters_number <-length(which(parent_education==18))
masters_number
percentmasters <- masters_number/8362
percentmasters

profdocschool_number <-length(which(parent_education==20))
profdocschool_number
percentprofdocschool <- profdocschool_number/8362
percentprofdocschool

missingedu_number <-length(which(is.na(parent_education)))
missingedu_number
percentmissingedu <- missingedu_number/8362
percentmissingedu

########################################################
############TRAINING GROUP DEMOGRAPHICS#################
########################################################


#Total sample means
meanAge_total <- mean(ABCD_CCA_Group1$age)
meanAge_total

#Total sample sd
sdAge_total <- sd(ABCD_CCA_Group1$age)
sdAge_total

#Total number of males and females (0=male, 1=female)
sexTable_total <- table(ABCD_CCA_Group1$FEMALE)
sexTable_total

#Percentage of females
female_number <-length(which(ABCD_CCA_Group1$FEMALE==1))
percentfemale <- female_number/4196
female_number
percentfemale
male_number <-length(which(ABCD_CCA_Group1$FEMALE==0))
percentmale <- male_number/4196
male_number
percentmale

#Total number of whites (1=non-hispanic white, 0=not)
whiteTable_total <- table(ABCD_CCA_Group1$NHWHITE)
whiteTable_total

#Total number of hispanic (1=hispanic, 0=not)
hispanicTable_total <- table(ABCD_CCA_Group1$HISPANIC)
hispanicTable_total

#Total number of african (1=african, 0=not)
africanTable_total <- table(ABCD_CCA_Group1$AFRICAN)
africanTable_total

#Total number of other (1=other, 0=not)
otherraceTable_total <- table(ABCD_CCA_Group1$OTHER)
otherraceTable_total

#Percentage of each race group
white_number <-length(which(ABCD_CCA_Group1$NHWHITE==1))
percentwhite <- white_number/4196
percentwhite
hispanic_number <-length(which(ABCD_CCA_Group1$HISPANIC==1))
percenthispanic <- hispanic_number/4196
percenthispanic
african_number <-length(which(ABCD_CCA_Group1$AFRICAN==1))
percentafrican <- african_number/4196
percentafrican
otherrace_number <-length(which(ABCD_CCA_Group1$OTHER==1))
percentotherrace <- otherrace_number/4196
percentotherrace

#Income Summary table
income_total<-table(ABCD_CCA_Group1$income)
income_total

#Percentage of each income response
lessthanfivek_number <-length(which(ABCD_CCA_Group1$income==1))
lessthanfivek_number
percentlessthanfivek <- lessthanfivek_number/4196
percentlessthanfivek
fivektotwelvek_number <-length(which(ABCD_CCA_Group1$income==2))
fivektotwelvek_number
percentfivektotwelvek <- fivektotwelvek_number/4196
percentfivektotwelvek
twelvektosixteenk_number <-length(which(ABCD_CCA_Group1$income==3))
twelvektosixteenk_number
percenttwelvektosixteenk <- twelvektosixteenk_number/4196
percenttwelvektosixteenk
sixteenktotwentyfivek_number <-length(which(ABCD_CCA_Group1$income==4))
sixteenktotwentyfivek_number
percentsixteenktotwentyfivek <- sixteenktotwentyfivek_number/4196
percentsixteenktotwentyfivek
twentyfivektothirtyfivek_number <-length(which(ABCD_CCA_Group1$income==5))
twentyfivektothirtyfivek_number
percenttwentyfivektothirtyfivek <- twentyfivektothirtyfivek_number/4196
percenttwentyfivektothirtyfivek
thirtyfivektofiftyk_number <-length(which(ABCD_CCA_Group1$income==6))
thirtyfivektofiftyk_number
percentthirtyfivektofiftyk <- thirtyfivektofiftyk_number/4196
percentthirtyfivektofiftyk 
fiftyktoseventyfivek_number <-length(which(ABCD_CCA_Group1$income==7))
fiftyktoseventyfivek_number
percentfiftyktoseventyfivek <- fiftyktoseventyfivek_number/4196
percentfiftyktoseventyfivek
seventyfivektohundredk_number <-length(which(ABCD_CCA_Group1$income==8))
seventyfivektohundredk_number
percentseventyfivektohundredk <- seventyfivektohundredk_number/4196
percentseventyfivektohundredk
hundredktotwohundrek_number <-length(which(ABCD_CCA_Group1$income==9))
hundredktotwohundrek_number
percenthundredktotwohundrek <- hundredktotwohundrek_number/4196
percenthundredktotwohundrek
overtwohundredk_number <-length(which(ABCD_CCA_Group1$income==10))
overtwohundredk_number
percentovertwohundredk <- overtwohundredk_number/4196
percentovertwohundredk
missingincome_number <-length(which(is.na(ABCD_CCA_Group1$income)))
missingincome_number
percentmissingincome <- missingincome_number/4196
percentmissingincome



attach(ABCD_CCA_Group1)

#Parental Education Summary table
parentaledu_total<-table(parent_education)
parentaledu_total

#Percentage of each parental education response
neverorkindergarten_number <-length(which(parent_education==0))
neverorkindergarten_number

first_number <-length(which(parent_education==1))
first_number

second_number <-length(which(parent_education==2))
second_number

third_number <-length(which(parent_education==3))
third_number

fourth_number <-length(which(parent_education==4))
fourth_number

fifth_number <-length(which(parent_education==5))
fifth_number

sixth_number <-length(which(parent_education==6))
sixth_number

seventh_number <-length(which(parent_education==7))
seventh_number

eighth_number <-length(which(parent_education==8))
eighth_number

ninth_number <-length(which(parent_education==9))
ninth_number

tenth_number <-length(which(parent_education==10))
tenth_number

eleventh_number <-length(which(parent_education==11))
eleventh_number


nodegree_number <- (neverorkindergarten_number + first_number + second_number + third_number +
                      fourth_number + fifth_number + sixth_number + seventh_number + eighth_number +
                      ninth_number + tenth_number + eleventh_number)
nodegree_number
percentnodegree <- (neverorkindergarten_number + first_number + second_number + third_number +
                      fourth_number + fifth_number + sixth_number + seventh_number + eighth_number +
                      ninth_number + tenth_number + eleventh_number)/4196
percentnodegree

highschoolgradGED_number <-length(which(parent_education==12))
highschoolgradGED_number
percenthighschoolgradGED <- highschoolgradGED_number/4196
percenthighschoolgradGED

somecollege_number <-length(which(parent_education==13))
somecollege_number
percentsomecoll <- somecollege_number/4196
percentsomecoll

associate_number <-length(which(parent_education==14))
associate_number
percentassociate <- associate_number/4196
percentassociate

bachelors_number <-length(which(parent_education==16))
bachelors_number
percentbach <- bachelors_number/4196
percentbach

masters_number <-length(which(parent_education==18))
masters_number
percentmasters <- masters_number/4196
percentmasters

profdocschool_number <-length(which(parent_education==20))
profdocschool_number
percentprofdocschool <- profdocschool_number/4196
percentprofdocschool

missingedu_number <-length(which(is.na(parent_education)))
missingedu_number
percentmissingedu <- missingedu_number/4196
percentmissingedu


########################################################
################TEST GROUP DEMOGRAPHICS#################
########################################################


#Total sample means
meanAge_total <- mean(ABCD_CCA_Group2$age)
meanAge_total

#Total sample sd
sdAge_total <- sd(ABCD_CCA_Group2$age)
sdAge_total

#Total number of males and females (0=male, 1=female)
sexTable_total <- table(ABCD_CCA_Group2$FEMALE)
sexTable_total

#Percentage of females
female_number <-length(which(ABCD_CCA_Group2$FEMALE==1))
percentfemale <- female_number/4166
female_number
percentfemale
male_number <-length(which(ABCD_CCA_Group2$FEMALE==0))
percentmale <- male_number/4166
male_number
percentmale

#Total number of whites (1=non-hispanic white, 0=not)
whiteTable_total <- table(ABCD_CCA_Group2$NHWHITE)
whiteTable_total

#Total number of hispanic (1=hispanic, 0=not)
hispanicTable_total <- table(ABCD_CCA_Group2$HISPANIC)
hispanicTable_total

#Total number of african (1=african, 0=not)
africanTable_total <- table(ABCD_CCA_Group2$AFRICAN)
africanTable_total

#Total number of other (1=other, 0=not)
otherraceTable_total <- table(ABCD_CCA_Group2$OTHER)
otherraceTable_total

#Percentage of each race group
white_number <-length(which(ABCD_CCA_Group2$NHWHITE==1))
percentwhite <- white_number/4166
percentwhite
hispanic_number <-length(which(ABCD_CCA_Group2$HISPANIC==1))
percenthispanic <- hispanic_number/4166
percenthispanic
african_number <-length(which(ABCD_CCA_Group2$AFRICAN==1))
percentafrican <- african_number/4166
percentafrican
otherrace_number <-length(which(ABCD_CCA_Group2$OTHER==1))
percentotherrace <- otherrace_number/4166
percentotherrace

#Income Summary table
income_total<-table(ABCD_CCA_Group2$income)
income_total

#Percentage of each income response
lessthanfivek_number <-length(which(ABCD_CCA_Group2$income==1))
lessthanfivek_number
percentlessthanfivek <- lessthanfivek_number/4166
percentlessthanfivek
fivektotwelvek_number <-length(which(ABCD_CCA_Group2$income==2))
fivektotwelvek_number
percentfivektotwelvek <- fivektotwelvek_number/4166
percentfivektotwelvek
twelvektosixteenk_number <-length(which(ABCD_CCA_Group2$income==3))
twelvektosixteenk_number
percenttwelvektosixteenk <- twelvektosixteenk_number/4166
percenttwelvektosixteenk
sixteenktotwentyfivek_number <-length(which(ABCD_CCA_Group2$income==4))
sixteenktotwentyfivek_number
percentsixteenktotwentyfivek <- sixteenktotwentyfivek_number/4166
percentsixteenktotwentyfivek
twentyfivektothirtyfivek_number <-length(which(ABCD_CCA_Group2$income==5))
twentyfivektothirtyfivek_number
percenttwentyfivektothirtyfivek <- twentyfivektothirtyfivek_number/4166
percenttwentyfivektothirtyfivek
thirtyfivektofiftyk_number <-length(which(ABCD_CCA_Group2$income==6))
thirtyfivektofiftyk_number
percentthirtyfivektofiftyk <- thirtyfivektofiftyk_number/4166
percentthirtyfivektofiftyk 
fiftyktoseventyfivek_number <-length(which(ABCD_CCA_Group2$income==7))
fiftyktoseventyfivek_number
percentfiftyktoseventyfivek <- fiftyktoseventyfivek_number/4166
percentfiftyktoseventyfivek
seventyfivektohundredk_number <-length(which(ABCD_CCA_Group2$income==8))
seventyfivektohundredk_number
percentseventyfivektohundredk <- seventyfivektohundredk_number/4166
percentseventyfivektohundredk
hundredktotwohundrek_number <-length(which(ABCD_CCA_Group2$income==9))
hundredktotwohundrek_number
percenthundredktotwohundrek <- hundredktotwohundrek_number/4166
percenthundredktotwohundrek
overtwohundredk_number <-length(which(ABCD_CCA_Group2$income==10))
overtwohundredk_number
percentovertwohundredk <- overtwohundredk_number/4166
percentovertwohundredk
missingincome_number <-length(which(is.na(ABCD_CCA_Group2$income)))
missingincome_number
percentmissingincome <- missingincome_number/4166
percentmissingincome



attach(ABCD_CCA_Group2)

#Parental Education Summary table
parentaledu_total<-table(parent_education)
parentaledu_total

#Percentage of each parental education response
neverorkindergarten_number <-length(which(parent_education==0))
neverorkindergarten_number

first_number <-length(which(parent_education==1))
first_number

second_number <-length(which(parent_education==2))
second_number

third_number <-length(which(parent_education==3))
third_number

fourth_number <-length(which(parent_education==4))
fourth_number

fifth_number <-length(which(parent_education==5))
fifth_number

sixth_number <-length(which(parent_education==6))
sixth_number

seventh_number <-length(which(parent_education==7))
seventh_number

eighth_number <-length(which(parent_education==8))
eighth_number

ninth_number <-length(which(parent_education==9))
ninth_number

tenth_number <-length(which(parent_education==10))
tenth_number

eleventh_number <-length(which(parent_education==11))
eleventh_number


nodegree_number <- (neverorkindergarten_number + first_number + second_number + third_number +
                      fourth_number + fifth_number + sixth_number + seventh_number + eighth_number +
                      ninth_number + tenth_number + eleventh_number)
nodegree_number
percentnodegree <- (neverorkindergarten_number + first_number + second_number + third_number +
                      fourth_number + fifth_number + sixth_number + seventh_number + eighth_number +
                      ninth_number + tenth_number + eleventh_number)/4166
percentnodegree

highschoolgradGED_number <-length(which(parent_education==12))
highschoolgradGED_number
percenthighschoolgradGED <- highschoolgradGED_number/4166
percenthighschoolgradGED

somecollege_number <-length(which(parent_education==13))
somecollege_number
percentsomecoll <- somecollege_number/4166
percentsomecoll

associate_number <-length(which(parent_education==14))
associate_number
percentassociate <- associate_number/4166
percentassociate

bachelors_number <-length(which(parent_education==16))
bachelors_number
percentbach <- bachelors_number/4166
percentbach

masters_number <-length(which(parent_education==18))
masters_number
percentmasters <- masters_number/4166
percentmasters

profdocschool_number <-length(which(parent_education==20))
profdocschool_number
percentprofdocschool <- profdocschool_number/4166
percentprofdocschool

missingedu_number <-length(which(is.na(parent_education)))
missingedu_number
percentmissingedu <- missingedu_number/4166
percentmissingedu


