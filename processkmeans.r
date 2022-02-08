library(learningtower)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(factoextra)
library(reactable)
library(arules)


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


PISA_2018 <- school_student_subset_2018

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

# Histograms of Math, Science and Wealth features

hist(ASEAN_PISA_2018$math, 
     main = paste("Histogram of", "PISA 2018 Mathematics Performance"), 
     col="#3e3f3a")


hist(ASEAN_PISA_2018$science, 
     main = paste("Histogram of", "PISA 2018 Science Performance"),
     col = "#3e3f3a")

hist(ASEAN_PISA_2018$wealth, 
     main = paste("Histogram of", "Wealth"),
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
                                         ifelse(STUDENT_PROFILE_2018$`FATHER_ED`=='less than ISCED1', 1,
                                                ifelse(STUDENT_PROFILE_2018$`FATHER_ED`=='ISCED 1', 2,
                                                       ifelse(STUDENT_PROFILE_2018$`FATHER_ED`=='ISCED 2', 3,
                                                              ifelse(STUDENT_PROFILE_2018$`FATHER_ED`=='ISCED 3B, C', 4,
                                                                     ifelse(STUDENT_PROFILE_2018$`FATHER_ED`=='ISCED 3A', 5,NA))))))

# Transforming FATHER_ED variable to omit duplicates and reduce data-set
unique(STUDENT_PROFILE_2018[, "FATHER_ED"])

STUDENT_PROFILE_2018$MOTHER_ED <- ifelse(STUDENT_PROFILE_2018$`FATHER_ED`=='-', 0,
                                         ifelse(STUDENT_PROFILE_2018$`FATHER_ED`=='less than ISCED1', 1,
                                                ifelse(STUDENT_PROFILE_2018$`FATHER_ED`=='ISCED 1', 2,
                                                       ifelse(STUDENT_PROFILE_2018$`FATHER_ED`=='ISCED 2', 3,
                                                              ifelse(STUDENT_PROFILE_2018$`FATHER_ED`=='ISCED 3B, C', 4,
                                                                     ifelse(STUDENT_PROFILE_2018$`FATHER_ED`=='ISCED 3A', 5,NA))))))

# Transforming MOTHER_ED variable to omit duplicates and reduce data-set
unique(STUDENT_PROFILE_2018[, "MOTHER_ED"])

STUDENT_PROFILE_2018$NO_COMPUTER <- ifelse(STUDENT_PROFILE_2018$`NO_COMPUTER`=='-', 0,
                                           ifelse(STUDENT_PROFILE_2018$`NO_COMPUTER`=='0', 0,
                                                  ifelse(STUDENT_PROFILE_2018$`NO_COMPUTER`=='1', 1,
                                                         ifelse(STUDENT_PROFILE_2018$`NO_COMPUTER`=='2', 3,
                                                                ifelse(STUDENT_PROFILE_2018$`NO_COMPUTER`=='3+', 5,NA)))))

# Transforming NO_COMPUTER variable to omit duplicates and reduce data-set
unique(STUDENT_PROFILE_2018[, "NO_COMPUTER"])

STUDENT_PROFILE_2018$NO_TV <- ifelse(STUDENT_PROFILE_2018$`NO_TV`=='-', 0,
                                     ifelse(STUDENT_PROFILE_2018$`NO_TV`=='0', 0,
                                            ifelse(STUDENT_PROFILE_2018$`NO_TV`=='1', 1,
                                                   ifelse(STUDENT_PROFILE_2018$`NO_TV`=='2', 3,
                                                          ifelse(STUDENT_PROFILE_2018$`NO_TV`=='3+', 5,NA)))))

# Transforming NO_TV variable to omit duplicates and reduce data-set
unique(STUDENT_PROFILE_2018[, "NO_TV"])

STUDENT_PROFILE_2018$NO_CAR <- ifelse(STUDENT_PROFILE_2018$`NO_CAR`=='-', 0,
                                      ifelse(STUDENT_PROFILE_2018$`NO_CAR`=='0', 0,
                                             ifelse(STUDENT_PROFILE_2018$`NO_CAR`=='1', 1,
                                                    ifelse(STUDENT_PROFILE_2018$`NO_CAR`=='2', 3,
                                                           ifelse(STUDENT_PROFILE_2018$`NO_CAR`=='3+', 5,NA)))))

unique(STUDENT_PROFILE_2018[, "NO_CAR"])

STUDENT_PROFILE_2018$NO_BOOK <- ifelse(STUDENT_PROFILE_2018$`NO_BOOK`=='-', 0,
                                       ifelse(STUDENT_PROFILE_2018$`NO_BOOK`=='0-10', 1,
                                              ifelse(STUDENT_PROFILE_2018$`NO_BOOK`=='11-25', 1,
                                                     ifelse(STUDENT_PROFILE_2018$`NO_BOOK`=='26-100', 2,
                                                            ifelse(STUDENT_PROFILE_2018$`NO_BOOK`=='101-200', 3,
                                                                   ifelse(STUDENT_PROFILE_2018$`NO_BOOK`=='201-500', 4,
                                                                          ifelse(STUDENT_PROFILE_2018$`NO_BOOK`=='more than 500', 5,NA)))))))

# Transforming NO_BOOK variable to omit duplicates and reduce data-set
unique(STUDENT_PROFILE_2018[, "NO_BOOK"])

# Converting iso3B country code to country name

STUDENT_PROFILE_2018$CNT <- ifelse(STUDENT_PROFILE_2018$`CNT`=='BRN', "Brunei",
                                   ifelse(STUDENT_PROFILE_2018$`CNT`=='IDN', "Indonesia",
                                          ifelse(STUDENT_PROFILE_2018$`CNT`=='MYS', "Malaysia",
                                                 ifelse(STUDENT_PROFILE_2018$`CNT`=='PHL', "Philippines",
                                                        ifelse(STUDENT_PROFILE_2018$`CNT`=='SGP', "Singapore",
                                                               ifelse(STUDENT_PROFILE_2018$`CNT`=='THA', "Thailand",
                                                                      ifelse(STUDENT_PROFILE_2018$`CNT`=='VNM', "Vietnam",
                                                                             NA)))))))

region <- STUDENT_PROFILE_2018$CNT
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
kmeansFunc <- function(dataset, plotTitle = "Cluster plot", scale = FALSE){
  
  # Initializing variables
  KMEANS_DATA = dataset[3:2]
  
  # Scaling variables
  SCALED_KMEANS_DATA = scale(KMEANS_DATA, center = TRUE, scale = scale)
  
  # Calculating Distance
  KMEANS_DIST = dist(SCALED_KMEANS_DATA)
  
  # Applying the algorithm with k set to 3
  km.out <- kmeans(SCALED_KMEANS_DATA, centers = 3, nstart = 10)
  print(km.out)
  km.clusters <- km.out$cluster
  rownames(SCALED_KMEANS_DATA) <- paste(dataset$CNT, 1:dim(dataset)[1], sep = '_')
  
  # Visualizing the Cluster
  fviz_cluster(list(data=KMEANS_DATA, cluster = km.clusters), main = plotTitle, show.clust.cent = TRUE,)
}

# K-Means of Mathematics Performance against multiple economic variables

# Clustering of (Wealth, MATH)
MATH_TO_WEALTH = data.frame(
  CNT,
  WEALTH,
  MATH
)
# Applying the Algorithm
kmeansFunc(MATH_TO_WEALTH, "(Wealth, Math) Clusters")

# Clustering of (ESCS, MATH)
MATH_TO_ESCS = data.frame(
  CNT,
  ESCS,
  MATH
)

# Applying the Algorithm
kmeansFunc(MATH_TO_ESCS, "(Economic, Social, and Cultural Status, Math) Clusters")

# Clustering of (FATHER_ED, MATH)
MATH_TO_FATHER_ED = data.frame(
  CNT,
  FATHER_ED,
  MATH
)

# Applying the Algorithm
kmeansFunc(MATH_TO_FATHER_ED, "(Father's Educational Attainment, Math) Clusters")

# Clustering of (MOTHER_ED, MATH)
MATH_TO_MOTHER_ED = data.frame(
  CNT,
  MOTHER_ED,
  MATH
)

# Applying the Algorithm
kmeansFunc(MATH_TO_FATHER_ED, "(Mother's Educational Attainment, Math) Clusters")

# Clustering of (NO_COMPUTER, Math)
MATH_TO_COMPUTERS = data.frame(
  CNT,
  NO_COMPUTER,
  MATH
)

# Applying the Algorithm
kmeansFunc(MATH_TO_COMPUTERS, "(Number of Computers, Math) Clusters")

# Clustering of (NO_TV, Math)
MATH_TO_TV = data.frame(
  CNT,
  NO_TV,
  MATH
)

# Applying the Algorithm
kmeansFunc(MATH_TO_TV, "(Number of TV, Math) Clusters")

# Clustering of (NO_BOOK, Math)
MATH_TO_BOOKS = data.frame(
  CNT,
  NO_BOOK,
  MATH
)

# Applying the Algorithm
kmeansFunc(MATH_TO_BOOKS, "(Number of Books Read, Math) Clusters")

# K-Means of Science Performance against multiple economic variables

# Clustering of (Wealth, SCIENCE)
SCIENCE_TO_WEALTH = data.frame(
  CNT,
  WEALTH,
  SCIENCE
)

# Applying the Algorithm
kmeansFunc(SCIENCE_TO_WEALTH, "(Wealth, Science) Clusters")

# Clustering of (ESCS, SCIENCE)
SCIENCE_TO_ESCS = data.frame(
  CNT,
  ESCS,
  SCIENCE
)

# Applying the Algorithm
kmeansFunc(SCIENCE_TO_ESCS, "(Economic, Social, and Cultural Status, Science) Clusters")

# Clustering of (FATHER_ED, SCIENCE)
SCIENCE_TO_FATHER_ED = data.frame(
  CNT,
  FATHER_ED,
  SCIENCE
)

# Applying the Algorithm
kmeansFunc(SCIENCE_TO_FATHER_ED, "(Father's Educational Attainment, Science) Clusters")

# Clustering of (MOTHER_ED, SCIENCE)
SCIENCE_TO_MOTHER_ED = data.frame(
  CNT,
  MOTHER_ED,
  SCIENCE
)

# Applying the Algorithm
kmeansFunc(SCIENCE_TO_MOTHER_ED, "(Mother's Educational Attainment, Science) Clusters")

# Clustering of (NO_COMPUTER, SCIENCE)
SCIENCE_TO_COMPUTERS = data.frame(
  CNT,
  NO_COMPUTER,
  SCIENCE
)

# Applying the Algorithm
kmeansFunc(SCIENCE_TO_COMPUTERS, "(Number of Computers, Science) Clusters")

# Clustering of (NO_TV, SCIENCE)
SCIENCE_TO_TV = data.frame(
  CNT,
  NO_TV,
  SCIENCE
)

# Applying the Algorithm
kmeansFunc(SCIENCE_TO_TV, "(Number of TV's, Science) Clusters")

# Clustering of (NO_BOOK, SCIENCE)
SCIENCE_TO_BOOKS = data.frame(
  CNT,
  NO_BOOK,
  SCIENCE
)

# Applying the Algorithm
kmeansFunc(SCIENCE_TO_BOOKS, "(Number of Books Read, Science) Clusters")

# Clustering of (MATH, SCIENCE)
MATH_TO_SCIENCE = data.frame(
  CNT,
  MATH,
  SCIENCE
)

kmeansFunc(MATH_TO_SCIENCE, "(Math, Science) Clusters")

# Geo-mapping Math, Science, and Wealth in ASEAN Countries

MATH_MAP = data.frame(
  region,
  MATH
)
SCIENCE_MAP = data.frame(
  region,
  SCIENCE
)
WEALTH_MAP = data.frame(
  region,
  WEALTH
)

WORLD_MAP <- map_data("world")

# Math Map
MAP_DATA_MATH <- WORLD_MAP
MAP_DATA_MATH <- left_join(MAP_DATA_MATH, MATH_MAP, by = "region" )
MAP_DATA_MATH_NEW <- MAP_DATA_MATH %>% filter(!is.na(MAP_DATA_MATH$MATH))
ASEAN_MATH_MAP <- ggplot(MAP_DATA_MATH_NEW, aes(x = long, y = lat, group=group)) + 
  geom_polygon(aes(fill = MATH), color = "black")
ASEAN_MATH_MAP2 <- ASEAN_MATH_MAP + scale_fill_gradient(name = "Math Scores", low = "red", high = "yellow", na.value = "grey50") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        rect = element_blank())
ASEAN_MATH_MAP2

# Science Map
MAP_DATA_SCIENCE <- WORLD_MAP
MAP_DATA_SCIENCE <- left_join(MAP_DATA_SCIENCE, SCIENCE_MAP, by = "region")
MAP_DATA_SCIENCE_NEW <- MAP_DATA_SCIENCE %>% filter(!is.na(MAP_DATA_SCIENCE$SCIENCE))
ASEAN_SCIENCE_MAP <- ggplot(MAP_DATA_SCIENCE_NEW, aes(x = long, y = lat, group=group)) + 
  geom_polygon(aes(fill = SCIENCE), color = "black")
ASEAN_SCIENCE_MAP2 <- ASEAN_SCIENCE_MAP + scale_fill_gradient(name = "Science Scores", low = "red", high = "yellow", na.value = "grey50") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        rect = element_blank())
ASEAN_SCIENCE_MAP2

# Wealth Map
MAP_DATA_WEALTH <- WORLD_MAP
MAP_DATA_WEALTH <- left_join(MAP_DATA_WEALTH, WEALTH_MAP, by = "region")
MAP_DATA_WEALTH_NEW <- MAP_DATA_WEALTH %>% filter(!is.na(MAP_DATA_WEALTH$WEALTH))
ASEAN_WEALTH_MAP <- ggplot(MAP_DATA_WEALTH_NEW, aes(x = long, y = lat, group=group)) + 
  geom_polygon(aes(fill = WEALTH), color = "black")
ASEAN_WEALTH_MAP2 <- ASEAN_WEALTH_MAP + scale_fill_gradient(name = "Wealth", low = "red", high = "yellow", na.value = "grey50") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        rect = element_blank())
ASEAN_WEALTH_MAP2