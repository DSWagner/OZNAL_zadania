library(readr)
library(tidyverse)
library(lobstr)
library(dplyr)
library(ggplot2)
library(magrittr)
library(data.table)
library(caret)
library(MASS)
library(purrr)
library(GGally)
library(ggplot2)
library(gplots)
library(pROC)
library(kernlab)
library(stats)
library(e1071)
library(igraph)
library(ggraph)


# Setting the working directory to where the dataset is located
# Uncomment the appropriate line below to match your directory structure
# setwd("C:/Users/dswag/Desktop/Zadania/OZNAL/Data")
setwd("C:/work/oznal/OZNAL_zadania/Data")

# Listing all files in the current working directory to verify the presence of our dataset
list.files()

# Reading the dataset into R
raw_data <- read_csv("ObesityDataSet.csv", col_names = TRUE, num_threads = 4)

# Displaying the first few rows of the dataset to ensure it's loaded correctly
head(raw_data)


# Renaming columns in the dataset to make them more descriptive
# names(raw_data)[names(raw_data) == "FAVC"] <- "FreqConsHighCalFood"
# names(raw_data)[names(raw_data) == "FCVC"] <- "FreqConsVegs"
# names(raw_data)[names(raw_data) == "NCP"] <- "NumMainMeals"
# names(raw_data)[names(raw_data) == "CAEC"] <- "ConsFoodBetwMeals"
# names(raw_data)[names(raw_data) == "CH2O"] <- "ConsWaterDaily"
# names(raw_data)[names(raw_data) == "CALC"] <- "ConsAlc"
# names(raw_data)[names(raw_data) == "SCC"] <- "CalsConsMon"
# names(raw_data)[names(raw_data) == "FAF"] <- "PhysActFreq"
# names(raw_data)[names(raw_data) == "TUE"] <- "TimeTechDev"
# names(raw_data)[names(raw_data) == "MTRANS"] <- "Trans"
# names(raw_data)[names(raw_data) == "family_history_with_overweight"] <- "ObesityFam"

# Identifying missing values in dataset
cleaned_data <- raw_data

# Check data types of each column
str(cleaned_data)

cleaned_data$FAF <- round(cleaned_data$FAF)
cleaned_data$FAF <- pmin(pmax(cleaned_data$FAF, 0), 3)
cleaned_data$FCVC <- round(cleaned_data$FCVC)
cleaned_data$FCVC <- pmin(pmax(cleaned_data$FCVC, 0), 3)

# Create categorical factors from FAF and FCVC
cleaned_data$FAF_category <- ifelse(cleaned_data$FAF == 0, 'No activity',
                            ifelse(cleaned_data$FAF == 1, 'Low activity',
                                   ifelse(cleaned_data$FAF == 2, 'Moderate activity', 'High activity')))

cleaned_data$FCVC_category <- ifelse(cleaned_data$FCVC == 1, 'Low consumption',
                             ifelse(cleaned_data$FCVC == 2, 'Moderate consumption', 'High consumption'))

cleaned_data$Age_category <- ifelse(cleaned_data$Age >= 14 & cleaned_data$Age <= 21, 'Youth',
                            ifelse(cleaned_data$Age >= 22 & cleaned_data$Age <= 30, 'Young Adults',
                                   ifelse(cleaned_data$Age >= 31 & cleaned_data$Age <= 50, 'Middle-Aged Adults', 'Senior')))

cleaned_data <- cleaned_data %>%
  mutate(across(where(is.character), as.factor))

str(cleaned_data)
