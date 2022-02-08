library(arules)
library(learningtower)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(factoextra)
library(reactable)
library(arulesViz)


# Loading the PISA 2018 data and assigning it to variable PISA_2018
data("student_subset_2018")
data("school")
data("countrycode")
school_student_subset_2018 <- left_join(
  student_subset_2018, 
  school, 
  by = c("school_id", "country", "year")) 
# Filtering ASEAN countries to be loaded
# Brunei, Cambodia, Indonesia, Laos, Malaysia, Myanmar, Philippines, Singapore, Thailand and, Vietnam
# PISA_2018 <- load_student(2018)
PISA_2018 <- student_subset_2018
# Filtering ASEAN countries and assigning them into their own data-frames
PH <- filter(PISA_2018, country == 'PHL')
BRN <- filter(PISA_2018, country == 'BRN')
IDN <- filter(PISA_2018, country == 'IDN')
KHM <- filter(PISA_2018, country == 'KHM') # Did not participate
LAO <- filter(PISA_2018, country == 'LAO') # Did not participate
MYS <- filter(PISA_2018, country == 'MYS')
THA <- filter(PISA_2018, country == 'THA')
VNM <- filter(PISA_2018, country == 'VNM')
SGP <- filter(PISA_2018, country == 'SGP')
MMR <- filter(PISA_2018, country == 'MMR') # Did not participate

# There are in total 3 countries that did not participate in the PISA 2018 assessment
# NO_OF_SAMPLE <- 50

# Building ASEAN data-frame
ASEAN_PISA_2018 = rbind.data.frame(
  PH,
  BRN,
  IDN,
  MYS,
  THA,
  VNM,
  SGP
)

# Data Cleaning
# Renaming columns to UPPERCASE
CNT <- ASEAN_PISA_2018$country
SCH_ID <- ASEAN_PISA_2018$school_id
STU_ID <- ASEAN_PISA_2018$country
MOTHER_ED <- ASEAN_PISA_2018$mother_educ
FATHER_ED <- ASEAN_PISA_2018$father_educ
GENDER <- ASEAN_PISA_2018$gender
COMPUTER <- ASEAN_PISA_2018$computer
INTERNET <- ASEAN_PISA_2018$internet
MATH <- ASEAN_PISA_2018$math
SCIENCE <- ASEAN_PISA_2018$science
STU_WGT <- ASEAN_PISA_2018$stu_wgt
DESK <- ASEAN_PISA_2018$desk
NO_TV <- ASEAN_PISA_2018$television
NO_COMPUTER <- ASEAN_PISA_2018$computer_n
NO_CAR <- ASEAN_PISA_2018$car
NO_BOOK <- ASEAN_PISA_2018$book
WEALTH <- ASEAN_PISA_2018$wealth
ESCS <- ASEAN_PISA_2018$escs

# Rebuilding new student profile
# Invoking na.omit to remove missing values from the data-set
STUDENT_PROFILE_2018 = na.omit(data.frame(
  CNT,
  SCH_ID,
  STU_ID,
  MOTHER_ED,
  FATHER_ED,
  GENDER,
  COMPUTER,
  INTERNET,
  MATH,
  SCIENCE,
  STU_WGT,
  DESK,
  NO_TV,
  NO_COMPUTER,
  NO_CAR,
  NO_BOOK,
  WEALTH,
  ESCS
))

# Data Processing for Apriori

STUDENT_PROFILE_2018$FATHER_ED <- ifelse(STUDENT_PROFILE_2018$`FATHER_ED`=='-', 'A1',
                                  ifelse(STUDENT_PROFILE_2018$`FATHER_ED`=='less than ISCED1', 'A2',
                                  ifelse(STUDENT_PROFILE_2018$`FATHER_ED`=='ISCED 1', 'A3',
                                  ifelse(STUDENT_PROFILE_2018$`FATHER_ED`=='ISCED 2', 'A4',
                                  ifelse(STUDENT_PROFILE_2018$`FATHER_ED`=='ISCED 3B, C', 'A5',
                                  ifelse(STUDENT_PROFILE_2018$`FATHER_ED`=='ISCED 3A', 'A6',NA))))))

STUDENT_PROFILE_2018$MOTHER_ED <- ifelse(STUDENT_PROFILE_2018$`MOTHER_ED`=='-', 'B1',
                                  ifelse(STUDENT_PROFILE_2018$`MOTHER_ED`=='less than ISCED1', 'B2',
                                  ifelse(STUDENT_PROFILE_2018$`MOTHER_ED`=='ISCED 1', 'B3',
                                  ifelse(STUDENT_PROFILE_2018$`MOTHER_ED`=='ISCED 2', 'B4',
                                  ifelse(STUDENT_PROFILE_2018$`MOTHER_ED`=='ISCED 3B, C', 'B5',
                                  ifelse(STUDENT_PROFILE_2018$`MOTHER_ED`=='ISCED 3A', 'B6',NA))))))


STUDENT_PROFILE_2018$NO_COMPUTER <- ifelse(STUDENT_PROFILE_2018$`NO_COMPUTER`=='-', 'C1',
                                    ifelse(STUDENT_PROFILE_2018$`NO_COMPUTER`=='0', 'C2',
                                    ifelse(STUDENT_PROFILE_2018$`NO_COMPUTER`=='1', 'C3',
                                    ifelse(STUDENT_PROFILE_2018$`NO_COMPUTER`=='2', 'C4',
                                    ifelse(STUDENT_PROFILE_2018$`NO_COMPUTER`=='3+', 'C5',NA)))))


STUDENT_PROFILE_2018$NO_TV <- ifelse(STUDENT_PROFILE_2018$`NO_TV`=='-', 'D1',
                              ifelse(STUDENT_PROFILE_2018$`NO_TV`=='0', 'D2',
                              ifelse(STUDENT_PROFILE_2018$`NO_TV`=='1', 'D3',
                              ifelse(STUDENT_PROFILE_2018$`NO_TV`=='2', 'D4',
                              ifelse(STUDENT_PROFILE_2018$`NO_TV`=='3+', 'D5',NA)))))


STUDENT_PROFILE_2018$NO_CAR <- ifelse(STUDENT_PROFILE_2018$`NO_CAR`=='-', 'E1',
                               ifelse(STUDENT_PROFILE_2018$`NO_CAR`=='0', 'E2',
                               ifelse(STUDENT_PROFILE_2018$`NO_CAR`=='1', 'E3',
                               ifelse(STUDENT_PROFILE_2018$`NO_CAR`=='2', 'E4',
                               ifelse(STUDENT_PROFILE_2018$`NO_CAR`=='3+', 'E5',NA)))))


STUDENT_PROFILE_2018$NO_BOOK <- ifelse(STUDENT_PROFILE_2018$`NO_BOOK`=='-', 'F1',
                                ifelse(STUDENT_PROFILE_2018$`NO_BOOK`=='0-10', 'F2',
                                ifelse(STUDENT_PROFILE_2018$`NO_BOOK`=='11-25', 'F3',
                                ifelse(STUDENT_PROFILE_2018$`NO_BOOK`=='26-100', 'F4',
                                ifelse(STUDENT_PROFILE_2018$`NO_BOOK`=='101-200', 'F5',
                                ifelse(STUDENT_PROFILE_2018$`NO_BOOK`=='201-500', 'F6',
                                ifelse(STUDENT_PROFILE_2018$`NO_BOOK`=='more than 500', 'F7',NA)))))))

MATH_PERF <- ifelse(STUDENT_PROFILE_2018$`MATH`=='-', 0,
             ifelse(STUDENT_PROFILE_2018$`MATH`<= 300, 'Low',
             ifelse(STUDENT_PROFILE_2018$`MATH`< 499, 'Medium',
             ifelse(STUDENT_PROFILE_2018$`MATH`>= 500 , 'High', NA))))

SCIENCE_PERF <- ifelse(STUDENT_PROFILE_2018$`SCIENCE`=='-', 0,
                ifelse(STUDENT_PROFILE_2018$`SCIENCE`<= 300, 'Low',
                ifelse(STUDENT_PROFILE_2018$`SCIENCE`< 499, 'Medium',
                ifelse(STUDENT_PROFILE_2018$`SCIENCE`>= 500 , 'High', NA))))

FATHER_ED <- as.factor(STUDENT_PROFILE_2018$FATHER_ED)
MOTHER_ED <- as.factor(STUDENT_PROFILE_2018$MOTHER_ED)
NO_COMPUTER <- as.factor(STUDENT_PROFILE_2018$NO_COMPUTER)
NO_CAR <- as.factor(STUDENT_PROFILE_2018$NO_CAR)
NO_TV <- as.factor(STUDENT_PROFILE_2018$NO_TV)
NO_BOOK <- as.factor(STUDENT_PROFILE_2018$NO_BOOK)
MATH_SCORE_PERF <- as.factor(MATH_PERF)
SCIENCE_SCORE_PERF <- as.factor(SCIENCE_PERF)


# Association Rule with Apriori Algorithm
ASSOC_DATA = data.frame(
  FATHER_ED,
  MOTHER_ED,
  NO_COMPUTER,
  NO_CAR,
  NO_TV,
  NO_BOOK,
  MATH_SCORE_PERF,
  SCIENCE_SCORE_PERF
)

Model = apriori(data = ASSOC_DATA, parameter = list(
                confidence = 0.5
))

# Visualize
rules.all <- sort(Model, by = 'lift')
inspect(rules.all)
plot(rules.all, method = "graph")
