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
names(raw_data)[names(raw_data) == "FAVC"] <- "FreqConsHighCalFood"
names(raw_data)[names(raw_data) == "FCVC"] <- "FreqConsVegs"
names(raw_data)[names(raw_data) == "NCP"] <- "NumMainMeals"
names(raw_data)[names(raw_data) == "CAEC"] <- "ConsFoodBetwMeals"
names(raw_data)[names(raw_data) == "CH2O"] <- "ConsWaterDaily"
names(raw_data)[names(raw_data) == "CALC"] <- "ConsAlc"
names(raw_data)[names(raw_data) == "SCC"] <- "CalsConsMon"
names(raw_data)[names(raw_data) == "FAF"] <- "PhysActFreq"
names(raw_data)[names(raw_data) == "TUE"] <- "TimeTechDev"
names(raw_data)[names(raw_data) == "MTRANS"] <- "Trans"

# Identifying missing values in dataset
sum(is.na(raw_data))

# Identifying missing values per column
colSums(is.na(raw_data))

# If removing outliers introduced NAs, remove those rows
cleaned_data %<>% drop_na()

# Removing duplicate rows from the dataset, keeping only unique rows
cleaned_data <- cleaned_data %>% distinct()

# Check data types of each column
str(cleaned_data)