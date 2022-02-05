library(learningtower)
library(dplyr)
library(tidyr)
library(factoextra)
library(reactable)


# Loading the PISA 2018 data and assigning it to variable PISA_2018
# Filtering ASEAN countries to be loaded
# Brunei, Cambodia, Indonesia, Laos, Malaysia, Myanmar, Philippines, Singapore, Thailand and, Vietnam

PISA_2018 <- load_student(2018)

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

NO_OF_SAMPLE <- 50

# Building ASEAN data-frame
ASEAN_PISA_2018 = rbind.data.frame(
  sample_n(PH, NO_OF_SAMPLE),
  sample_n(BRN, NO_OF_SAMPLE),
  sample_n(IDN, NO_OF_SAMPLE),
  sample_n(MYS, NO_OF_SAMPLE),
  sample_n(THA, NO_OF_SAMPLE),
  sample_n(VNM, NO_OF_SAMPLE),
  sample_n(SGP, NO_OF_SAMPLE)
)

# Output of the sample
reactable(head(PISA_2018), 
          searchable = TRUE,
          striped = TRUE,
          highlight = TRUE,
          bordered = TRUE,
          theme = reactableTheme(
            borderColor = "#dfe2e5",
            stripedColor = "#f6f8fa",
            highlightColor = "#f0f5f9",
            cellPadding = "8px 12px"))

# Class of the data
class(ASEAN_PISA_2018)

# Total number of rows and columns in the data-set
dim(ASEAN_PISA_2018)

# Mathematics summary Stats
MATH_STAT_2018 <- summary(ASEAN_PISA_2018$math)

# Science summary Stats
SCI_STAT_2018 <- summary(ASEAN_PISA_2018$science)

# Histograms of both Math and Science Performances
PISA_MATH_2018 <- ASEAN_PISA_2018$math
hist(PISA_MATH_2018, 
     main = paste("Histogram of", "PISA 2018 Mathematics Performance"), 
     col="#3e3f3a")

PISA_SCI_2018 <- ASEAN_PISA_2018$science
hist(PISA_SCI_2018, 
     main = paste("Histogram of", "PISA 2018 Science Performance"),
     col = "#3e3f3a")

# Boxplots of both Math and Science Performances
boxplot(na.omit(PISA_MATH_2018), col="#3e3f3a")

boxplot(na.omit(PISA_SCI_2018), col="#3e3f3a")

# Overalls summary statistics of all variables in the data-set
OVERALL_STAT <- summary(ASEAN_PISA_2018)
OVERALL_STAT

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

# Summarizing new student profile data-set
summary(STUDENT_PROFILE_2018)

# New student profile
reactable(head(STUDENT_PROFILE_2018))

# Mapping chosen Categorical data

STUDENT_PROFILE_2018$FATHER_ED <- ifelse(STUDENT_PROFILE_2018$`FATHER_ED`=='-', 0,
                                         ifelse(STUDENT_PROFILE_2018$`FATHER_ED`=='less than ISCED1', 2,
                                                ifelse(STUDENT_PROFILE_2018$`FATHER_ED`=='ISCED 3B, C', 4,
                                                       ifelse(STUDENT_PROFILE_2018$`FATHER_ED`=='ISCED 3A', 6,
                                                              ifelse(STUDENT_PROFILE_2018$`FATHER_ED`=='ISCED 2', 8,
                                                                     ifelse(STUDENT_PROFILE_2018$`FATHER_ED`=='ISCED 1', 10,
                                                                            NA))))))

# Transforming FATHER_ED variable to omit duplicates and reduce data-set
unique(STUDENT_PROFILE_2018[, "FATHER_ED"])

STUDENT_PROFILE_2018$MOTHER_ED <- ifelse(STUDENT_PROFILE_2018$`MOTHER_ED`=='-', 0,
                                  ifelse(STUDENT_PROFILE_2018$`MOTHER_ED`=='less than ISCED1', 2,
                                  ifelse(STUDENT_PROFILE_2018$`MOTHER_ED`=='ISCED 3B, C', 4,
                                  ifelse(STUDENT_PROFILE_2018$`MOTHER_ED`=='ISCED 3A', 6,
                                  ifelse(STUDENT_PROFILE_2018$`MOTHER_ED`=='ISCED 2', 8,
                                  ifelse(STUDENT_PROFILE_2018$`MOTHER_ED`=='ISCED 1', 10,
                                                                            NA))))))

# Transforming MOTHER_ED variable to omit duplicates and reduce data-set
unique(STUDENT_PROFILE_2018[, "MOTHER_ED"])

STUDENT_PROFILE_2018$NO_COMPUTER <- ifelse(STUDENT_PROFILE_2018$`NO_COMPUTER`=='-', 0,
                                    ifelse(STUDENT_PROFILE_2018$`NO_COMPUTER`=='0', 0,
                                    ifelse(STUDENT_PROFILE_2018$`NO_COMPUTER`=='1', 5,
                                    ifelse(STUDENT_PROFILE_2018$`NO_COMPUTER`=='2', 8,
                                    ifelse(STUDENT_PROFILE_2018$`NO_COMPUTER`=='3+', 10,
                                                                       NA)))))

# Transforming NO_COMPUTER variable to omit duplicates and reduce data-set
unique(STUDENT_PROFILE_2018[, "NO_COMPUTER"])

STUDENT_PROFILE_2018$NO_TV <- ifelse(STUDENT_PROFILE_2018$`NO_TV`=='-', 0,
                              ifelse(STUDENT_PROFILE_2018$`NO_TV`=='0', 0,
                              ifelse(STUDENT_PROFILE_2018$`NO_TV`=='1', 6,
                              ifelse(STUDENT_PROFILE_2018$`NO_TV`=='2', 7,
                              ifelse(STUDENT_PROFILE_2018$`NO_TV`=='3+', 10,
                                                                 NA)))))

# Transforming NO_TV variable to omit duplicates and reduce data-set
unique(STUDENT_PROFILE_2018[, "NO_TV"])

STUDENT_PROFILE_2018$NO_CAR <- ifelse(STUDENT_PROFILE_2018$`NO_CAR`=='-', 0,
                               ifelse(STUDENT_PROFILE_2018$`NO_CAR`=='0', 0,
                               ifelse(STUDENT_PROFILE_2018$`NO_CAR`=='1', 6,
                               ifelse(STUDENT_PROFILE_2018$`NO_CAR`=='2', 7,
                               ifelse(STUDENT_PROFILE_2018$`NO_CAR`=='3+', 10,
                                                                  NA)))))

unique(STUDENT_PROFILE_2018[, "NO_CAR"])

STUDENT_PROFILE_2018$NO_BOOK <- ifelse(STUDENT_PROFILE_2018$`NO_BOOK`=='-', 0,
                                ifelse(STUDENT_PROFILE_2018$`NO_BOOK`=='0-10', 3,
                                ifelse(STUDENT_PROFILE_2018$`NO_BOOK`=='11-25', 4,
                                ifelse(STUDENT_PROFILE_2018$`NO_BOOK`=='26-100', 5,
                                ifelse(STUDENT_PROFILE_2018$`NO_BOOK`=='101-200', 7,
                                ifelse(STUDENT_PROFILE_2018$`NO_BOOK`=='201-500', 9,
                                ifelse(STUDENT_PROFILE_2018$`NO_BOOK`=='more than 500', 10,
                                                                                 NA)))))))

# Transforming NO_BOOK variable to omit duplicates and reduce data-set
unique(STUDENT_PROFILE_2018[, "NO_BOOK"])

# We're also gonna label performances of both of MATH and SCIENCE variables
# STUDENT_PROFILE_2018$`SCIENCE`

MATH_PERF <- ifelse(STUDENT_PROFILE_2018$`MATH` == '-', 0,
             ifelse(STUDENT_PROFILE_2018$`MATH` <= 450, 'Developing',
             ifelse(STUDENT_PROFILE_2018$`MATH` <= 699, 'Good',
             ifelse(STUDENT_PROFILE_2018$`MATH` >= 700 , 'Excellent', NA))))

SCIENCE_PERF <- ifelse(STUDENT_PROFILE_2018$`SCIENCE` == '-', 0,
                ifelse(STUDENT_PROFILE_2018$`SCIENCE` <= 450, 'Developing',
                ifelse(STUDENT_PROFILE_2018$`SCIENCE` <= 699, 'Good',
                ifelse(STUDENT_PROFILE_2018$`SCIENCE` >= 700 , 'Excellent', NA))))

CNT <- STUDENT_PROFILE_2018$CNT
FATHER_ED <- STUDENT_PROFILE_2018$FATHER_ED
MOTHER_ED <- STUDENT_PROFILE_2018$MOTHER_ED
NO_COMPUTER <- STUDENT_PROFILE_2018$NO_COMPUTER
NO_CAR <- STUDENT_PROFILE_2018$NO_CAR
NO_TV <- STUDENT_PROFILE_2018$NO_TV
NO_BOOK <- STUDENT_PROFILE_2018$NO_BOOK
MATH <- STUDENT_PROFILE_2018$MATH
SCIENCE <- STUDENT_PROFILE_2018$SCIENCE
WEALTH <- STUDENT_PROFILE_2018$WEALTH
ESCS <- STUDENT_PROFILE_2018$ESCS

# K-Means Function
kmeansFunc <- function(dataset, plotTitle = "Cluster plot"){
  
  # Initializing variables
  KMEANS_DATA = dataset[2:3]
  
  # Scaling variables
  SCALED_KMEANS_DATA = scale(KMEANS_DATA, center = TRUE, scale = TRUE)
  
  # Calculating Distance
  KMEANS_DIST = dist(SCALED_KMEANS_DATA)
  
  # Applying the algorithm with k set to 3
  km.out <- kmeans(SCALED_KMEANS_DATA, centers = 3, nstart = 100)
  print(km.out)
  km.clusters <- km.out$cluster
  rownames(SCALED_KMEANS_DATA) <- paste(dataset$CNT, 1:dim(dataset)[1], sep = '_')
  
  # Visualizing the Cluster
  fviz_cluster(list(data=SCALED_KMEANS_DATA, cluster = km.clusters), main = plotTitle,)
}

# K-Means of Mathematics Performance against multiple economic variables

# Clustering of (Wealth, MATH)
MATH_TO_WEALTH = data.frame(
  CNT,
  WEALTH,
  MATH
)
# Applying the Algorithm
kmeansFunc(MATH_TO_WEALTH, "Math to Wealth Cluster")

# Clustering of (ESCS, MATH)
MATH_TO_ESCS = data.frame(
  CNT,
  ESCS,
  MATH
)

# Applying the Algorithm
kmeansFunc(MATH_TO_ESCS)

# Clustering of (FATHER_ED, MATH)
MATH_TO_FATHER_ED = data.frame(
  CNT,
  FATHER_ED,
  MATH
)

# Applying the Algorithm
kmeansFunc(MATH_TO_FATHER_ED)

# Clustering of (MOTHER_ED, MATH)
MATH_TO_MOTHER_ED = data.frame(
  CNT,
  MOTHER_ED,
  MATH
)

# Applying the Algorithm
kmeansFunc(MATH_TO_MOTHER_ED)

# Clustering of (NO_COMPUTER, Math)
MATH_TO_COMPUTERS = data.frame(
  CNT,
  NO_COMPUTER,
  MATH
)

# Applying the Algorithm
kmeansFunc(MATH_TO_COMPUTERS)

# Clustering of (NO_TV, Math)
MATH_TO_TV = data.frame(
  CNT,
  NO_TV,
  MATH
)

# Applying the Algorithm
kmeansFunc(MATH_TO_TV)

# Clustering of (NO_BOOK, Math)
MATH_TO_BOOKS = data.frame(
  CNT,
  NO_BOOK,
  MATH
)

# Applying the Algorithm
kmeansFunc(MATH_TO_BOOKS)

# K-Means of Science Performance against multiple economic variables

# Clustering of (Wealth, SCIENCE)
SCIENCE_TO_WEALTH = data.frame(
  CNT,
  WEALTH,
  SCIENCE
)

# Applying the Algorithm
kmeansFunc(SCIENCE_TO_WEALTH)

# Clustering of (ESCS, SCIENCE)
SCIENCE_TO_ESCS = data.frame(
  CNT,
  ESCS,
  SCIENCE
)

# Applying the Algorithm
kmeansFunc(SCIENCE_TO_ESCS)

# Clustering of (FATHER_ED, SCIENCE)
SCIENCE_TO_FATHER_ED = data.frame(
  CNT,
  FATHER_ED,
  SCIENCE
)

# Applying the Algorithm
kmeansFunc(SCIENCE_TO_FATHER_ED)

# Clustering of (MOTHER_ED, SCIENCE)
SCIENCE_TO_MOTHER_ED = data.frame(
  CNT,
  MOTHER_ED,
  SCIENCE
)

# Applying the Algorithm
kmeansFunc(SCIENCE_TO_MOTHER_ED)

# Clustering of (NO_COMPUTER, SCIENCE)
SCIENCE_TO_COMPUTERS = data.frame(
  CNT,
  NO_COMPUTER,
  SCIENCE
)

# Applying the Algorithm
kmeansFunc(SCIENCE_TO_COMPUTERS)

# Clustering of (NO_TV, SCIENCE)
SCIENCE_TO_TV = data.frame(
  CNT,
  NO_TV,
  SCIENCE
)

# Applying the Algorithm
kmeansFunc(SCIENCE_TO_TV)

# Clustering of (NO_BOOK, SCIENCE)
SCIENCE_TO_BOOKS = data.frame(
  CNT,
  NO_BOOK,
  SCIENCE
)

# Applying the Algorithm
kmeansFunc(SCIENCE_TO_BOOKS)
