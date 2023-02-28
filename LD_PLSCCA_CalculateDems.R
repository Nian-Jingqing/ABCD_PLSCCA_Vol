#Evaluating demographic breakdowns

#LOAD DATA
full_data <- readRDS("abcd_data_PLSCCA.rds")


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
percentfemale <- female_number/9027
female_number
percentfemale
male_number <-length(which(full_data$FEMALE==0))
percentmale <- male_number/9027
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
percentwhite <- white_number/9027
percentwhite
hispanic_number <-length(which(full_data$HISPANIC==1))
percenthispanic <- hispanic_number/9027
percenthispanic
african_number <-length(which(full_data$AFRICAN==1))
percentafrican <- african_number/9027
percentafrican
otherrace_number <-length(which(full_data$OTHER==1))
percentotherrace <- otherrace_number/9027
percentotherrace

#Income Summary table
income_total<-table(full_data$income)
income_total

#Percentage of each income response
lessthanfivek_number <-length(which(full_data$income==1))
lessthanfivek_number
percentlessthanfivek <- lessthanfivek_number/9027
percentlessthanfivek
fivektotwelvek_number <-length(which(full_data$income==2))
fivektotwelvek_number
percentfivektotwelvek <- fivektotwelvek_number/9027
percentfivektotwelvek
twelvektosixteenk_number <-length(which(full_data$income==3))
twelvektosixteenk_number
percenttwelvektosixteenk <- twelvektosixteenk_number/9027
percenttwelvektosixteenk
sixteenktotwentyfivek_number <-length(which(full_data$income==4))
sixteenktotwentyfivek_number
percentsixteenktotwentyfivek <- sixteenktotwentyfivek_number/9027
percentsixteenktotwentyfivek
twentyfivektothirtyfivek_number <-length(which(full_data$income==5))
twentyfivektothirtyfivek_number
percenttwentyfivektothirtyfivek <- twentyfivektothirtyfivek_number/9027
percenttwentyfivektothirtyfivek
thirtyfivektofiftyk_number <-length(which(full_data$income==6))
thirtyfivektofiftyk_number
percentthirtyfivektofiftyk <- thirtyfivektofiftyk_number/9027
percentthirtyfivektofiftyk 
fiftyktoseventyfivek_number <-length(which(full_data$income==7))
fiftyktoseventyfivek_number
percentfiftyktoseventyfivek <- fiftyktoseventyfivek_number/9027
percentfiftyktoseventyfivek
seventyfivektohundredk_number <-length(which(full_data$income==8))
seventyfivektohundredk_number
percentseventyfivektohundredk <- seventyfivektohundredk_number/9027
percentseventyfivektohundredk
hundredktotwohundrek_number <-length(which(full_data$income==9))
hundredktotwohundrek_number
percenthundredktotwohundrek <- hundredktotwohundrek_number/9027
percenthundredktotwohundrek
overtwohundredk_number <-length(which(full_data$income==10))
overtwohundredk_number
percentovertwohundredk <- overtwohundredk_number/9027
percentovertwohundredk
missingincome_number <-length(which(is.na(full_data$income)))
missingincome_number
percentmissingincome <- missingincome_number/9027
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
                      ninth_number + tenth_number + eleventh_number)/9027
percentnodegree

highschoolgradGED_number <-length(which(parent_education==12))
highschoolgradGED_number
percenthighschoolgradGED <- highschoolgradGED_number/9027
percenthighschoolgradGED

somecollege_number <-length(which(parent_education==13))
somecollege_number
percentsomecoll <- somecollege_number/9027
percentsomecoll

associate_number <-length(which(parent_education==14))
associate_number
percentassociate <- associate_number/9027
percentassociate

bachelors_number <-length(which(parent_education==16))
bachelors_number
percentbach <- bachelors_number/9027
percentbach

masters_number <-length(which(parent_education==18))
masters_number
percentmasters <- masters_number/9027
percentmasters

profdocschool_number <-length(which(parent_education==20))
profdocschool_number
percentprofdocschool <- profdocschool_number/9027
percentprofdocschool

missingedu_number <-length(which(is.na(parent_education)))
missingedu_number
percentmissingedu <- missingedu_number/9027
percentmissingedu

