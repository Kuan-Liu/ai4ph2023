################################################################################
# A tutorial introduction to heterogeneous treatment effect estimation
# with meta-learners
# 
# Administration and Policy and Mental Health Services Research
#
# Marie Salditt, Theresa Eckes, and Steffen Nestler (University of Muenster)
# e-mail address of corresponding author: msalditt@uni-muenster.de
#
#
# This file explains how to access the Add Health Public Use data
# and how to form the sample used for illustration.
################################################################################


## STEPS TO OBTAIN ADD HEALTH PUBLIC USE DATA
#--------------------------------------------------------------------------------
# As explained on the Add Health website (https://addhealth.cpc.unc.edu/data/),
# the public-use datasets are distributed free of charge by three sources, inter alia,
# by the Inter-university Consortium for Political and Social Research (ICPSR).

# The files are available for download from the ICPSR website:
# https://www.icpsr.umich.edu/web/ICPSR/studies/21600?archive=ICPSR&q=21600#
# To download the data, users are required to agree to the terms of use and to 
# specify the reason for download.

# In our example, we use the in-home questionnaire data from Wave I (21600-0001-Data.rda),
# Wave II (21600-0005-Data.rda), and Wave III (21600-0008-Data.rda).

# See also the codebook explorer to search for specific items:
# https://addhealth.cpc.unc.edu/documentation/codebook-explorer/#/


# PREPARE WORKSPACE AND LOAD DATA
#--------------------------------------------------------------------------------
rm(list = ls())

# load data
setwd("...") # set working directory
load("Wave I/DS0001 - In-Home Questionnaire/21600-0001-Data.rda")
load("Wave II/DS0005 - In-Home Questionnaire/21600-0005-Data.rda")
load("Wave III/DS0008 - In-Home Questionnaire/21600-0008-Data.rda")

waveI <- da21600.0001
waveII <- da21600.0005
waveIII <- da21600.0008


# SELECT COVARIATES FROM WAVE I 
#--------------------------------------------------------------------------------

covariateNamesQuest <- c(
  
                    "AID",  # ID variable 
  
                    # Socio-demographics
                    #------------------------------------------
                    
                    "IMONTH", "IDAY", "IYEAR", # date of the interview
                    "H1GI1M", "H1GI1Y", # birth month and year
                    "BIO_SEX", # biological sex
                     
                    # Ethnicity: Are you of Hispanic or Latino origin?
                    "H1GI4",
                    # Race (multiple choices possible):
                    "H1GI6A", # White
                    "H1GI6B", # Black,
                    "H1GI6C", # Native American/Indian,
                    "H1GI6D", # Asian
                    "H1GI6E", # other
                    
                    # Educational level of resident mother / father
                    "H1RM1", "H1RF1",
                    
                    # Financial situation
                    "PA56", # Parent questionnaire: Do you have enough money to pay your bills? 
                    "PA55", # Parent questionnaire: household's total income
                    
                    
                    # Self-reported general health
                    #------------------------------------------
                    "H1GH1", 
                    
                    
                    # Alcohol use
                    #------------------------------------------
                    
                    "H1TO12", 
                    # Have you had a drink of beer, wine, or liquor—not just a sip or a
                    # taste of someone else’s drink—more than 2 or 3 times in your life?
                    
                    "H1TO15", 
                    # During the past 12 months, on how many days did you drink
                    # alcohol?

                    
                    # Activities and excercise
                    #------------------------------------------
                    "H1DA5", "H1DA6", "H1DA7", 
                    "H1DA8", "H1DA9", "H1DA10",

                    # During the past week, how many times did you play an
                    # active sport, such as baseball, softball, basketball, soccer,
                    # swimming, or football?
                    
                    # During the past week, how many times did you do
                    # exercise, such as jogging, walking, karate, jumping rope,
                    # gymnastics or dancing?
                    
                    # During the past week, how many times did you just hang
                    # out with friends?
                    
                    # How many hours a week do you watch television?
                    
                    # How many hours a week do you watch videos?
                    
                    # How many hours a week do you play video or computer games?
                    
                    
                    # Perceived parental closeness
                    #------------------------------------------
                    "H1WP9", # How close do you feel to your resident mother/ father? 
                    "H1WP13", 
                    
                    "H1WP10", # How much do you think she/he cares about you? 
                    "H1WP14", 
                    
                    "H1PF5", # Overall, you are satisfied with your relationship with your mother/father. 
                    "H1PF25", 
                   
                    "H1PF1", # Most of the time, your mother/father is warm and loving toward you. 
                    "H1PF23", 
                    
                    "H1PF4", # You are satisfied with the way your mother/father and you communicate. 
                    "H1PF24",
                    
                    
                    # Perceived family support
                    #------------------------------------------
                    "H1PR3", # How much do you feel that your parents care about you?
                    "H1PR5", # How much do you feel that people in your family understand you?
                    "H1PR6",  # How much do you feel that you want to leave home?
                    "H1PR7",  # How much do you feel that you and your family have fun together?
                    "H1PR8",  # How much do you feel that your family pays attention to you?
                    
                    
                    # Parental involvement
                    #-----------------------------------------
                    # Which of the things listed on this card have you done with your {MOTHER/ADOPTIVE MOTHER/STEPMOTHER/FOSTER MOTHER/etc.} in the past 4 weeks?
                    paste0("H1WP17", LETTERS[1:10]),
                    
                    # Which of the things listed on this card have you done with your {MOTHER/ADOPTIVE MOTHER/STEPMOTHER/FOSTER MOTHER/etc.} in the past 4 weeks?
                    paste0("H1WP18", LETTERS[1:10]), 
                    
                    
                    # Suicidality
                    #---------------------------
                    "H1SU6", # Have any of your family tried to kill themselves during the past 12 months? 
                    "H1SU7", # Have any of them succeeded? 
                    
                    "H1SU4", # Have any of your friends tried to kill themselves during the past 12 months?
                    "H1SU5", # Have any of them succeeded? 
                    
                    "H1SU1", # During the past 12 months, did you ever seriously think about committing suicide?
                    "H1SU2", # During the past 12 months, how many times did you actually attempt suicide?

                    
                    # Prior treatment
                    #---------------------------
                    "H1HS3",  # In the past year, have you received psychological or emotional counseling?

                    
                    # Center for Epidemiologic Studies Depression Scale (CESD)
                    #-------------------------------------------------------------
                    paste0("H1FS", 1:19),
                    

                    # Self rated intelligence, self-esteem and personality
                    #---------------------------------------------------------
                    "H1SE4", # Compared with other people your age, how intelligent are you?
                    
                    # Self-esteem
                    "H1PF30", # You have a lot of good qualities.
                    "H1PF32", # You have a lot to be proud of.
                    "H1PF33", # You like yourself just the way you are.
                    "H1PF34", # You feel like you are doing everything just about right.
                    "H1PF35", # You feel socially accepted.
                    "H1PF36", # You feel loved and wanted.

                    
                    # Decision making
                    "H1PF18",
                    # When you have a problem to solve, one of the first
                    # things you do is get as many facts about the problem as
                    # possible.
                    
                    "H1PF19",
                    # When you are attempting to find a solution to a
                    # problem, you usually try to think of as many different
                    # ways to approach the problem as possible.
                    
                    "H1PF20",
                    # When making decisions, you generally use a systematic
                    # method for judging and comparing alternatives
                    
                    "H1PF21",
                    # After carrying out a solution to a problem, you usually
                    # try to analyze what went right and what went wrong.

                    
                    # Problem Avoidance
                    "H1PF14" # You usually go out of your way to avoid having to deal with problems in your life.

                   ) 

waveI <- waveI[, covariateNamesQuest]

# tidy up
rm(list = c("da21600.0001", "da21600.0002", "da21600.0005", "da21600.0008"))



#--------------------------------------------------------------------------------
# COVARIATE PREPARATION
#--------------------------------------------------------------------------------
dfs <- data.frame(AID = waveI$AID)


# AGE
#-----------------------------
# Compute age variable as described in https://addhealth.cpc.unc.edu/documentation/frequently-asked-questions/

# transform month and year of interview and birth date to numeric
dput(levels(waveI$IMONTH))
waveI$imonth <- factor(waveI$IMONTH, levels =  c("(01) (1) January", "(04) (4) April", "(05) (5) May", "(06) (6) June", 
                  "(07) (7) July", "(08) (8) August", "(09) (9) September", "(10) (10) October", 
                  "(11) (11) November", "(12) (12) December"),
                  labels = c(1,4:12) )
waveI$imonth <- as.numeric(levels(waveI$imonth))[waveI$imonth] # transform into numeric variable
dput(levels(waveI$IYEAR))
waveI$iyear <- factor(waveI$IYEAR, levels =  c("(94) (94) 1994", "(95) (95) 1995"),
                      labels = c(1994, 1995) )
waveI$iyear <- as.numeric(levels(waveI$iyear ))[waveI$iyear] 

dput(levels(waveI$H1GI1M))
waveI$H1GI1M <- factor(waveI$H1GI1M, levels = c("(01) (1) January", "(02) (2) February", "(03) (3) March", 
                                                "(04) (4) April", "(05) (5) May", "(06) (6) June", "(07) (7) July", 
                                                "(08) (8) August", "(09) (9) September", "(10) (10) October", 
                                                "(11) (11) November", "(12) (12) December"),
                       labels = 1:12)
waveI$H1GI1M  <- as.numeric(levels(waveI$H1GI1M ))[waveI$H1GI1M ]
dput(levels(waveI$H1GI1Y))
waveI$H1GI1Y <- factor(waveI$H1GI1Y, levels = c("(74) (74) 1974", "(75) (75) 1975", "(76) (76) 1976", "(77) (77) 1977", 
                                                "(78) (78) 1978", "(79) (79) 1979", "(80) (80) 1980", "(81) (81) 1981", 
                                                "(82) (82) 1982", "(83) (83) 1983"),
                       labels = c(1974:1983))
waveI$H1GI1Y  <- as.numeric(levels(waveI$H1GI1Y ))[waveI$H1GI1Y ]

# compute interview date
idate <-  with( waveI, ISOdate(month = imonth, day = IDAY, year = iyear) )

# compute birth date (use 15 as day)
bdate <- with( waveI, ISOdate(month = H1GI1M, day = 15, year = H1GI1Y) )

# compute age
dfs$Age <- as.numeric( round( (idate-bdate) / 365.25 ) )



# BIOLOGICAL SEX
#-----------------------------
dput(levels(waveI$BIO_SEX))
dfs$Sex  <- factor( waveI$BIO_SEX , levels = c("(1) (1) Male", "(2) (2) Female"),
                        labels = c(1, 0) ) # 1 for male, 0 for female 
dfs$Sex <- as.numeric(levels(dfs$Sex ))[dfs$Sex]



# RACE
#-----------------------------

# Construct unique race variable as described in 
# https://addhealth.cpc.unc.edu/documentation/frequently-asked-questions/

Race <- NA
Race <- ifelse(waveI$H1GI6B == "(1) (1) Marked", 2, Race) # Black or African American, Non-Hispanic
Race <- ifelse(waveI$H1GI6D == "(1) (1) Marked (If Asian/Pacific Islander among R's answer ask Q", 3,  Race) # Asian or Pacific Islander, Non-Hispanic
Race <- ifelse(waveI$H1GI6C == "(1) (1) Marked", 4,  Race) # American Indian or Native American, Non-Hispanic
Race <- ifelse(waveI$H1GI6E == "(1) (1) Marked", 5,  Race) # Other, Non-Hispanic
Race <- ifelse(waveI$H1GI6A == "(1) (1) Marked", 6,  Race) # White, Non-Hispanic
Race <- ifelse(waveI$H1GI4 == "(1) (1) Yes", 1, Race) # Hispanic or Latino
Race <- factor(Race, levels = c(6, 1:5), labels = c("White", "Hispanic", "Black", "Asian", "Native American", "Other"))
# use hot-one encoding
RaceDummies <- model.matrix(~  Race, model.frame(~  Race, Race, na.action=na.pass))[, -1] # exclude intercept
colnames(RaceDummies) <- c("Hispanic", "Black", "Asian", "Native.American", 
                           "OtherRace")
dfs <- cbind.data.frame(dfs, RaceDummies)



# PARENTAL EDUCATION
#-----------------------------
# H1RM1
# Educational level of resident mother (How far in school did she go?)
dput(levels(waveI$H1RM1))
dfs$EducationMother <- factor(waveI$H1RM1, levels = c("(01) (1) 8th grade or less", "(02) (2) >8th grade/didn't graduate high school", 
                                                      "(03) (3) Business/trade/voc. school instead high school", "(04) (4) High school graduate", 
                                                      "(05) (5) GED", "(06) (6) Business/trade/voc. school after high school", 
                                                      "(07) (7) College/didn't graduate", "(08) (8) Graduated from college/university", 
                                                      "(09) (9) Prof training beyond 4-year college/university", "(10) (10) She never went to school", 
                                                      "(11) (11) Went to school/Resp doesn't know level", "(12) (12) Resp doesn't know if she went to school"),
                              labels = c(1:9, 0, NA, NA)) # larger values indicate higher education
dfs$EducationMother <- as.numeric(levels(dfs$EducationMother))[dfs$EducationMother] 

# H1RF1
# Educational level of resident father (How far in school did he go?)
dput(levels(waveI$H1RF1))
dfs$EducationFather <- factor(waveI$H1RF1, levels = c("(01) (1) 8th grade or less", "(02) (2) >8th grade/didn't graduate high school", 
                                                      "(03) (3) Business/trade/voc. school instead high school", "(04) (4) High school graduate", 
                                                      "(05) (5) GED", "(06) (6) Business/trade/voc. school after high school", 
                                                      "(07) (7) College/didn't graduate", "(08) (8) Graduated from college/university", 
                                                      "(09) (9) Prof training beyond 4-year college/univ", "(10) (10) He never went to school", 
                                                      "(11) (11) Went to school/Resp doesn't know level", "(12) (12) Resp doesn't know if he went to school"),
                              labels = c(1:9, 0, NA, NA)) # larger values indicate higher education
dfs$EducationFather <- as.numeric(levels(dfs$EducationFather))[dfs$EducationFather]

# Create Parental Education variable by taking the mean of educational level of father and mother
dfs$ParentalEducation <- rowMeans(cbind(dfs$EducationMother, dfs$EducationFather), na.rm = T)
# larger values indicate higher educational level of parents

dfs$EducationFather <- dfs$EducationMother <- NULL



# FINANTIAL SITUATION OF PARENTS
#----------------------------------
# PA56: Do you have enough money to pay your bills?
dput(levels(waveI$PA56))
dfs$EnoughMoney  <- factor( waveI$PA56 , levels = c("(0) (0) No", "(1) (1) Yes"),
                            labels = c(0, 1) )
dfs$EnoughMoney <- as.numeric(levels(dfs$EnoughMoney))[dfs$EnoughMoney]

# PA55: Household's income (in 1000 dollar)
dfs$Income <- waveI$PA55



# SELF-RATED HEALTH
#----------------------------------
# H1GH1: In general, how is your health? Would you say...
dput(levels(waveI$H1GH1))
dfs$Health <- factor(waveI$H1GH1, levels = c("(1) (1) Excellent", "(2) (2) Very good", "(3) (3) Good", "(4) (4) Fair", 
                                              "(5) (5) Poor"),
                      labels = 5:1) # higher values indicate better health
dfs$Health <- as.numeric(levels(dfs$Health))[dfs$Health]



# SELF-RATED INTELLIGENCE
#----------------------------------
# H1SE4: Compared with other people your age, how intelligent are you? 
dput(levels(waveI$H1SE4))
dfs$Intelligence <- factor(waveI$H1SE4, levels = c("(1) (1) Moderately below average", "(2) (2) Slightly below average", 
                                                   "(3) (3) About average", "(4) (4) Slightly above average", "(5) (5) Moderately above average", 
                                                   "(6) (6) Extremely above average"),
                           labels = c(1:6)) # the higher, the more intelligent
dfs$Intelligence <- as.numeric(levels(dfs$Intelligence))[dfs$Intelligence]



# SELF-ESTEEM
#----------------------------------
# Please tell me whether you agree or disagree with each of the following statements.
# (5-point Likert scale)
selfEsteemItems  <- waveI[, c("H1PF30", paste0("H1PF3", 2:6))]
# "H1PF30" # You have a lot of good qualities.
# "H1PF32" # You have a lot to be proud of.
# "H1PF33" # You like yourself just the way you are.
# "H1PF34" # You feel like you are doing everything just about right.
# "H1PF35" # You feel socially accepted.
# "H1PF36" # You feel loved and wanted.

apply(selfEsteemItems , 2, function(x) unique(x))  
# all items are on the same scale, so we can refactor them at the same time
dput(levels(selfEsteemItems [, 1]))
selfEsteemItems <- apply(selfEsteemItems , 2, function(x){
  f <- factor(x, levels = c("(1) (1) Strongly agree", "(2) (2) Agree", "(3) (3) Neither agree nor disagree", 
                            "(4) (4) Disagree", "(5) (5) Strongly disagree"),
              labels = c(5:1)) # higher values indicate higher self-esteem
  f <- as.numeric(levels(f ))[f]
  return(f)
})
dfs$SelfEsteem <- rowMeans(selfEsteemItems, na.rm = T)
psych::describe(dfs$SelfEsteem)



# DECISION MAKING
#----------------------------------
# Please tell me whether you agree or disagree with each of the following statements.
# (5-point Likert scale)

decisionMakingItems  <- waveI[, paste0("H1PF", 18:21)]
apply(decisionMakingItems , 2, function(x) unique(x))  
# all items are on the same scale, so we can refactor them at the same time
dput(levels(decisionMakingItems [, 1]))
decisionMakingItems <- apply(decisionMakingItems , 2, function(x){
  f <- factor(x, levels = c("(1) (1) Strongly agree", "(2) (2) Agree", "(3) (3) Neither agree nor disagree", 
                            "(4) (4) Disagree", "(5) (5) Strongly disagree"),
              labels = c(5:1)) # higher values indicate more analytical approach towards decision making
  f <- as.numeric(levels(f ))[f]
  return(f)
})
dfs$DecisionMaking <- rowMeans(decisionMakingItems, na.rm = T)
psych::describe(dfs$DecisionMaking)



# PROBLEM AVOIDANCE
#----------------------------------
# H1PF14: You usually go out of your way to avoid having to deal with problems in your life.
dput(levels(waveI$H1PF14))
dfs$ProblemAvoidance <- factor(waveI$H1PF14, levels = c("(1) (1) Strongly agree", "(2) (2) Agree", "(3) (3) Neither agree nor disagree", 
                                                        "(4) (4) Disagree", "(5) (5) Strongly disagree"),
                               labels = c(5:1)) # the larger, the more dealing with problems is avoided
dfs$ProblemAvoidance <- as.numeric(levels(dfs$ProblemAvoidance))[dfs$ProblemAvoidance] 



# ALCOHOL USE
#------------------------------------------
# Create an alcohol use composite as described in
# https://www.sciencedirect.com/science/article/pii/S1054139X00001555

# Lifetime alcohol use
# Have you had a drink of beer, wine, or liquor—not just a sip or a
# taste of someone else’s drink—more than 2 or 3 times in your life?
dput(levels(waveI$H1TO12))
everAlcohol <- factor(waveI$H1TO12, levels = c("(0) (0) No (skip to Q.29)", "(1) (1) Yes"),
                             labels = c(0, 1)) 
everAlcohol <- as.numeric(levels(everAlcohol))[everAlcohol]

# Alcohol consumption during the past 12 months:
# During the past 12 months, on how many days did you drink
# alcohol?
dput(levels(waveI$H1TO15))
monthsAlcohol <- factor(waveI$H1TO15, levels = c("(1) (1) Every day/almost every day", "(2) (2) 3-5 days/week", 
                                                 "(3) (3) 1 or 2 days/week", "(4) (4) 2 or 3 days/month", "(5) (5) Once a month or less (3-12 times in past 12 months)", 
                                                 "(6) (6) 1 or 2 days in past 12 months", "(7) (7) Never (skip to Q.29)"),
                      labels = c(8, 7, 6, 5, 4, 3, 2)) 
monthsAlcohol <- as.numeric(levels(monthsAlcohol))[monthsAlcohol]

dfs$AlcoholUse <- ifelse(everAlcohol == 0, 1, monthsAlcohol)
# Create eight-level composite:
# 1 = "2–3 drinks lifetime,
# 2 = "more than 2–3 drinks lifetime, no use in past 12 months",
# 3 = "drank alcohol on 1 or 2 days in past 12 months,"
# 4 = "drank once a month or less in the past 12 months,"
# 5 = "drank 2–3 days a month in the past 12 months,"
# 6 = "drank 1–2 days a week in the past 12 months,"
# 7 = "drank 3–5 days a week in the past 12 months,"
# 8 = "drank every day or almost every day in the past 12 months").



# SPORTS AND EXCERCISE
#----------------------------------
# During the past week, how many times did you play an
# active sport, such as baseball, softball, basketball, soccer,
# swimming, or football?
dput(levels(waveI$H1DA5))
dfs$TeamSports <- factor(waveI$H1DA5, levels = c("(0) (0) Not at all", "(1) (1) 1 or 2 times", "(2) (2) 3 or 4 times", 
                                                 "(3) (3) 5 or more times"),
                         labels = c(0:3)) 
dfs$TeamSports <- as.numeric(levels(dfs$TeamSports))[dfs$TeamSports] 


# During the past week, how many times did you do
# exercise, such as jogging, walking, karate, jumping rope,
# gymnastics or dancing?
dput(levels(waveI$H1DA6))
dfs$Excercise <- factor(waveI$H1DA6, levels = c("(0) (0) Not at all", "(1) (1) 1 or 2 times", "(2) (2) 3 or 4 times", 
                                                "(3) (3) 5 or more times"),
                        labels = c(0:3)) # the larger, the more avoidance of having to deal with problems
dfs$Excercise <- as.numeric(levels(dfs$Excercise))[dfs$Excercise]



# TIME SPENT WITH FRIENDS
#----------------------------------
# H1DA7: During the past week, how many times did you just hang out with friends?
dput(levels(waveI$H1DA7))
dfs$TimeWithFriends <- factor(waveI$H1DA7, levels = c("(0) (0) Not at all", "(1) (1) 1 or 2 times", "(2) (2) 3 or 4 times", 
                                                      "(3) (3) 5 or more times"),
                              labels = c(0:3)) # the larger, the more avoidance of having to deal with problems
dfs$TimeWithFriends <- as.numeric(levels(dfs$TimeWithFriends))[dfs$TimeWithFriends] 



# HOURS SPENT WITH TELEVISION, VIDEOS, VIDEO GAMES
#-----------------------------------------------------
# H1DA8: How many hours a week do you watch television?
# H1DA9: How many hours a week do you watch videos?
# H1DA10: How many hours a week do you play video or computer games?

# Take the sum
videoItems <- waveI[, c("H1DA8", "H1DA9", "H1DA10")]
dfs$HoursVideosWeekly <- rowSums(videoItems)



# PARENTAL INVOLVEMENT
#-----------------------------------------------------

# Create a parent–adolescent activities composite reflecting the number of
# shared parent–child activities within the past 4 weeks as described in
# https://www.sciencedirect.com/science/article/pii/S1054139X00001555

activitiesMom <- waveI[, paste0("H1WP17", LETTERS[1:10])]
apply(activitiesMom, 2, function(x) unique(x))  

# all items are on the same scale, so we can refactor them at the same time
dput(levels(activitiesMom[, 1]))
activitiesMom <- apply(activitiesMom, 2, function(x){
  f <- factor(x, levels = c("(0) (0) No", "(1) (1) Yes"),
              labels = c(0,1))
  f <- as.numeric(levels(f ))[f]
  return(f)
})

activitiesDad <- waveI[, paste0("H1WP18", LETTERS[1:10])]
apply(activitiesDad, 2, function(x) unique(x))  

dput(levels(activitiesDad[, 1]))
activitiesDad <- apply(activitiesDad, 2, function(x){
  f <- factor(x, levels = c("(0) (0) No", "(1) (1) Yes"),
              labels = c(0,1))
  f <- as.numeric(levels(f ))[f]
  return(f)
})
# Sum the activities with mother and father
dfs$ActivitiesWithParent <- rowSums(cbind(activitiesMom, activitiesDad), na.rm = T)



# PERCEIVED PARENTAL CLOSENESS
#-------------------------------------------------------------
# Create a perceived parental closeness composite as described in 
# https://link.springer.com/article/10.1007/s10964-012-9865-5

closenessItems1 <- waveI[, paste0("H1WP", c(9,10,13,14))]
closenessItems2 <- waveI[, paste0("H1PF", c(1,4,5,23,24,25))]

dput(levels(closenessItems1 [, 1]))
closenessItems1 <- apply(closenessItems1 , 2, function(x){
  f <- factor(x, levels = c("(1) (1) Not at all", "(2) (2) Very little", "(3) (3) Somewhat", 
                            "(4) (4) Quite a bit", "(5) (5) Very much"),
              labels = c(1:5)) # higher values indicate higher closeness to parent
  f <- as.numeric(levels(f ))[f]
  return(f)
})

dput(levels(closenessItems2 [, 1]))
closenessItems2 <- apply(closenessItems2 , 2, function(x){
  f <- factor(x, levels = c("(1) (1) Strongly agree", "(2) (2) Agree", "(3) (3) Neither agree nor disagree", 
                            "(4) (4) Disagree", "(5) (5) Strongly disagree"),
              labels = c(5:1)) # higher values indicate higher closeness to parent
  f <- as.numeric(levels(f ))[f]
  return(f)
})

# perceived closeness to mother
closenessMomItems <- cbind(closenessItems1[, 1:2], closenessItems2[, 1:3])
closenessMom <- rowMeans(closenessMomItems) 
# perceived closeness to father
closenessDadItems <- cbind(closenessItems1[, 3:4], closenessItems2[, 4:6])
closenessDad <- rowMeans(closenessDadItems) 
# take the mean (for adolescents with one parent, this variable measures the perceived closeness to the respective parent.)
dfs$ParentalCloseness <- rowMeans(cbind(closenessMom,closenessDad), na.rm = T)



# PERCEIVED FAMILY SUPPORT
#------------------------------
# Create a family support composite as described in 
# https://link.springer.com/article/10.1007/s10964-012-9865-5

FamilySupportItems <- waveI[, paste0("H1PR", c(3,5,6:8))]
apply(FamilySupportItems , 2, function(x) sort(unique(x)))  
# all items are on the same scale, so we can refactor them at the same time
dput(levels(FamilySupportItems [, 1]))
FamilySupportItems <- apply(FamilySupportItems , 2, function(x){
  f <- factor(x, levels = c("(1) (1) Not at all", "(2) (2) Very little", "(3) (3) Somewhat", 
                            "(4) (4) Quite a bit", "(5) (5) Very much", "(6) (6) Does not apply"),
              labels = c(1:5, NA)) # higher values indicate higher perceived support
  f <- as.numeric(levels(f ))[f]
  return(f)
})
FamilySupportItems[, "H1PR6"] <- 6-FamilySupportItems[, "H1PR6"] # reverse code
dfs$FamilySupport <- rowMeans(FamilySupportItems, na.rm = T)
psych::describe(dfs$FamilySupport)



# FAMILY SUICIDE COMPOSITE
#-----------------------------------------------------
# Create a three-level family suicide composite as described in
# https://www.sciencedirect.com/science/article/pii/S1054139X00001555
# H1SU6: Have any of your family tried to kill themselves during the past 12 months? 
# H1SU7: Have any of them succeeded? 
unique(waveI$H1SU6); unique(waveI$H1SU7); 
FamilySuicide <- factor( ifelse( waveI$H1SU6 == "(0) (0) No (skip to Q.8)", "No Attempt",
                             ifelse(waveI$H1SU7 == "(0) (0) No", "Family member attempted suicide",
                                    "Family member completed suicide") ) )
FamilySuicide <- relevel(FamilySuicide, ref = "No Attempt")
# use hot-one encoding
FamilySuicideDummies <- model.matrix(~  FamilySuicide, model.frame(~  FamilySuicide, FamilySuicide, na.action=na.pass))[, -1] # exclude intercept
colnames(FamilySuicideDummies) <- c("AttemptedSuicideInFamily", "CompletedSuicideInFamily")
dfs <- cbind.data.frame(dfs, FamilySuicideDummies)


# FRIEND SUICIDE COMPOSITE
#-----------------------------------------------------
# H1SU4: Have any of your friends tried to kill themselves during the past 12 months?
# H1SU5: Have any of them succeeded? 
unique(waveI$H1SU4); unique(waveI$H1SU5)
FriendSuicide <- factor( ifelse( waveI$H1SU4 == "(0) (0) No (skip to Q.6)", "No Attempt",
                                 ifelse(waveI$H1SU7 == "(0) (0) No", "Friend attempted suicide",
                                        "Friend completed suicide") ) )
FriendSuicide <- relevel(FriendSuicide, ref = "No Attempt")
# use hot-one encoding
FriendSuicideDummies <- model.matrix(~  FriendSuicide, model.frame(~  FriendSuicide, FriendSuicide, na.action=na.pass))[, -1] # exclude intercept
colnames(FriendSuicideDummies) <- c("FriendAttemptedSuicide", "FriendCompletedSuicide")
dfs <- cbind.data.frame(dfs, FriendSuicideDummies)



# SUICIDALITY
#-----------------------------------------------------
# H1SU1: During the past 12 months, did you ever seriously think about committing suicide?
# H1SU2: During the past 12 months, how many times did you actually attempt suicide?
unique(waveI$H1SU1)
dfs$SuicidalThoughts <- factor( waveI$H1SU1, levels = c("(0) (0) No (skip to Q.4)", "(1) (1) Yes"),
                            labels = c(0,1) )
dfs$SuicidalThoughts <- as.numeric(levels(dfs$SuicidalThoughts))[dfs$SuicidalThoughts] # transform into numeric variable

unique(waveI$H1SU2); table(waveI$H1SU2)
SuicideAttempts <- factor( waveI$H1SU2, levels = c("(0) (0) 0 times (skip to Q.4)", "(1) (1) 1 times", "(2) (2) 2 or 3 times", 
                                                         "(3) (3) 4 or 5 times", "(4) (4) 6 or more times"),
                                labels = c("No attempt", "One attempt", rep("At least 2 attempts", 3)) )
SuicideAttempts[dfs$SuicidalThoughts == 0 & is.na(SuicideAttempts)] <- "No attempt"
# use hot-one encoding
SuicideAttemptsDummies <- model.matrix(~  SuicideAttempts , model.frame(~  SuicideAttempts, SuicideAttempts, na.action=na.pass))[, -1] # exclude intercept
colnames(SuicideAttemptsDummies) <- c("OneAttemptedSuicide", "AtLeastTwoAttemptedSuicides")
dfs <- cbind.data.frame(dfs, SuicideAttemptsDummies)



# PRIOR TREATMENT
#---------------------------
# H1HS3: In the past year, have you received psychological or emotional counseling?
unique(waveI$H1HS3)
dfs$PriorTreatment <- ifelse(waveI$H1HS3 == "(1) (1) Yes", 1, 0)



# PRIOR OUTCOME [DEPRESSIVE SYMPTOMS MEASURED VIA (CES-D)]
#-------------------------------------------------------------
# You were bothered by things that usually don’t bother you.
# You didn’t feel like eating, your appetite was poor.
# You felt that you could not shake off the blues, even with help from your family and your friends.
# RECODE 4: You felt that you were just as good as other people.
# You had trouble keeping your mind on what you were doing.
# You felt depressed.
# You felt that you were too tired to do things.
# RECODE 8: You felt hopeful about the future.
# You thought your life had been a failure.
# You felt fearful.  
# RECODE 11: You were happy
# You talked less than usual.
# You felt lonely.
# People were unfriendly to you.
# RECODE 15: You enjoyed life.
# You felt sad.
# You felt that people disliked you.
# It was hard to get started doing things.
# You felt life was not worth living.

CESDItems <- waveI[, paste0("H1FS", 1:19)]
apply(CESDItems, 2, function(x) unique(x))  

dput(levels(CESDItems[, 1]))
CESDItems <- apply(CESDItems, 2, function(x){
  f <- factor(x, levels = c("(0) (0) Never/rarely", "(1) (1) Sometimes", "(2) (2) A lot of the time", 
                            "(3) (3) Most/all of the time"),
              labels = 0:3)
  f <- as.numeric(levels(f ))[f]
  return(f)
})
# recode item 4, 8, 11, and 15
CESDItems[, c(4,8,11,15)] <- apply(CESDItems[, c(4,8,11,15)], 2, function(x) 3-x)
# take the sum
dfs$CES_D <- rowSums(CESDItems, na.rm = T) 
psych::describe(dfs$CES_D )



# GET TREATMENT VARIABLE FROM WAVE II
#--------------------------------------------------------------------------------
# H2HS5: In the past year, have you received psychological or emotional counseling?
treat <- data.frame(AID = waveII$AID, A = waveII$H2HS5)
levels(treat$A) 
treat$A <- factor(treat$A, levels = c("(0) (0) No", "(1) (1) Yes"), labels = c(0,1) )
treat$A <- as.numeric(levels(treat$A))[treat$A]
NA_IDX <- which(is.na(treat$A)) # 1677 3707
treat <- treat[-NA_IDX, ]

dfs <- merge(dfs, treat, by = "AID")



# GET OUTCOME VARIABLE FROM WAVE III 
#--------------------------------------------------------------------------------
# Think about the past seven days. How often was each of the following things
# true during the past seven days?
# H3SP5 You were bothered by things that usually don’t bother you. 
# H3SP6 You could not shake off the blues, even with help from your family and your friends
# H3SP7 You felt that you were just as good as other people 
# H3SP8 You had trouble keeping your mind on what you were doing 
# H3SP9 You were depressed 
# H3SP10 You were too tired to do things 
# H3SP11 You enjoyed life 
# H3SP12 You were sad 
# H3SP13 You felt that people disliked you 
# (Wave III did only include these 9 items from the full CES-D scale.)

outcomeItems <- waveIII[, c("AID", paste0("H3SP", 5:13))]
apply(outcomeItems[, -1], 2, function(x) unique(x))
# all items are on the same scale, so we can refactor them at the same time
dput(levels(outcomeItems[, 2]))
outcomeItems[, -1] <- apply(outcomeItems[, -1], 2, function(x){ 
                             f <- factor(x, levels = c("(0) (0) Never/rarely", "(1) (1) Sometimes", "(2) (2) A lot of the time", 
                                                       "(3) (3) Most of the time/all of the time"),
                                         labels = c(0:3))
                             f <- as.numeric(levels(f ))[f]
                             return(f)
                             })
# recode negative items
outcomeItems[, "H3SP7"] <- 3-outcomeItems[, "H3SP7"] 
outcomeItems[, "H3SP11"]  <- 3-outcomeItems[, "H3SP11"] 

# take the sum
outcomeItems <- as.data.frame(outcomeItems)
outcomeItems$Y<- rowSums(outcomeItems[, -1], na.rm = T)
# maximal score: 3*9 = 27
psych::describe(outcomeItems$Y)
#     vars    n mean   sd median trimmed  mad min max range skew kurtosis   se
#  X1    1 4882 4.48 4.05      3     3.9 2.97   0  25    25  1.4     2.26 0.06

outcome <- data.frame(AID = outcomeItems$AID, Y = outcomeItems$Y)
dfs <- merge(dfs, outcome, by = "AID")

dfs$AID <- NULL # ID variable no longer needed after merging



# IMPUTE MISSING VALUES USING BAGGED TREES
#--------------------------------------------------------------------------------
library(caret)
set.seed(1)
dfs_preprocessed <- caret::preProcess(dfs, method=c("bagImpute"))
dfs_imputed <- predict(dfs_preprocessed, dfs)
which(is.na(dfs_imputed))
str(dfs_imputed)
apply(dfs, 2, function(x) sum(is.na(x))) # variables with missing values
apply(dfs_imputed, 2, unique)
# Some dichotomous variables have imputed values between 0 and 1.
# We set imputed values < 0.5 to 0 and else to 1 for the dichotomous variables:
imputedBinary <- c("Hispanic", "Black", "Asian", "Native.American", "OtherRace", "EnoughMoney",   
                  "AttemptedSuicideInFamily", "CompletedSuicideInFamily", "FriendAttemptedSuicide", 
                  "FriendCompletedSuicide", "SuicidalThoughts", "OneAttemptedSuicide", 
                   "AtLeastTwoAttemptedSuicides", "PriorTreatment")
dfs_imputed[, imputedBinary] <- apply(dfs_imputed[, imputedBinary], 2, function(x){
                                      ifelse(x < 0.5, 0, 1)
                                    })

# save final data set
# write.table(dfs_imputed, "AddHealthExample_imputed.txt", row.names = F, col.names = T)
