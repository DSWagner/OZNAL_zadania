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

# Function to identify outliers within one column (instead of 0.25 and 0.75 quantiles we used 0.15 and 0.85 to maintain a reasonable dataset size)
remove_outliers <- function(x) {
  # Ensure the column is numeric
  if(is.numeric(x)) {
    Q1 <- quantile(x, 0.15, na.rm = TRUE)
    Q3 <- quantile(x, 0.85, na.rm = TRUE)
    IQR <- Q3 - Q1
    lower_bound <- Q1 - 1.5 * IQR
    upper_bound <- Q3 + 1.5 * IQR
    x[x < lower_bound | x > upper_bound] <- NA  # Assign NA to outliers
  }
  x
}

# Apply the function to each column
cleaned_data <- raw_data %>% mutate(across(where(is.numeric), remove_outliers))

# If removing outliers introduced NAs, remove those rows
cleaned_data %<>% drop_na()
raw_data
cleaned_data

# Check for missing values again
sum(is.na(cleaned_data))

# Check data types of each column
str(cleaned_data)

# Convert all character columns to factors to represent categorical data
cleaned_data <- cleaned_data %>%
  mutate(across(where(is.character), as.factor))

# Check factorization of each column
str(cleaned_data)

# Removing duplicate rows from the dataset, keeping only unique rows
cleaned_data <- cleaned_data %>% distinct()

cleaned_data

# Select only the numeric columns for the pairs plot
numerical_data <- cleaned_data[sapply(cleaned_data, is.numeric) ]

# Use ggpairs to create the pairs plot, displaying both scatter plots and correlations
pairs(numerical_data)

# Calculate correlation matrix for numerical columns
cor_matrix <- cor(numerical_data, use = "complete.obs")

cor_matrix

# # Plotting the heatmap using base R's heatmap() function
# heatmap.2(x = cor_matrix,
#           scale = "none", 
#           Colv = NA, 
#           Rowv = NA, 
#           dendrogram = "none",
#           trace = "none", 
#           col = colorRampPalette(c("blue", "white", "red"))(n = 299), 
#           margin = c(5, 5), 
#           main = "Correlation Matrix Heatmap",
#           key = TRUE, # This ensures the color key (legend) is displayed
#           keysize = 1.5)

# # Creating a new binary variable 'ObesityBinary' where 1 indicates Obesity (I, II, III) and 0 indicates otherwise
cleaned_data$ObesityBinary <- ifelse(cleaned_data$Nobesity %in% c("Obesity_Type_I", "Obesity_Type_II", "Obesity_Type_III"), 1, 0)

set.seed(123) # for reproducibility

# Calculate the size of the training set (80% of the dataset)
training_size <- floor(0.8 * nrow(cleaned_data))

# Sample indices for the training data
training_indices <- sample(seq_len(nrow(cleaned_data)), size = training_size)

# Split the data
train_data <- cleaned_data[training_indices, ]
test_data <- cleaned_data[-training_indices, ]

# ========================DRUHY MINIPROJEKT===============================

# Výpis počtu prípadov v každej kategórii pre trénovacie dáta
table_train <- table(train_data$ObesityBinary)
cat("Počet prípadov v trénovacích dátach pre každú kategóriu 'ObesityBinary':\n")
print(table_train)
cat("\n")

# Výpis percentuálneho rozdelenia v trénovacích dátach
cat("Percentuálne rozdelenie pre 'ObesityBinary' v trénovacích dátach:\n")
print(prop.table(table_train) * 100)
cat("\n")

# Výpis počtu prípadov v každej kategórii pre testovacie dáta
table_test <- table(test_data$ObesityBinary)
cat("Počet prípadov v testovacích dátach pre každú kategóriu 'ObesityBinary':\n")
print(table_test)
cat("\n")

# Výpis percentuálneho rozdelenia v testovacích dátach
cat("Percentuálne rozdelenie pre 'ObesityBinary' v testovacích dátach:\n")
print(prop.table(table_test) * 100)
cat("\n")

# ========================LDA===============================
# Vytvorenie modelu LDA
lda_model <- lda(ObesityBinary ~ FreqConsHighCalFood + FreqConsVegs + NumMainMeals + ConsFoodBetwMeals + ConsAlc + Age + PhysActFreq + Gender, data = train_data)

# Výpis výsledkov modelu
print(summary(lda_model))

# Predikcia na testovacej sade, získanie pravdepodobností
lda_probabilities <- predict(lda_model, newdata = test_data, type = "response")$posterior[,2]

# Nastavenie prahu
threshold <- 0.6  # Tento prah znamená, že pravdepodobnosti nad 0.6 budú klasifikované ako pozitívne

# Aplikovanie prahu na predpovede
predicted_classes <- ifelse(lda_probabilities > threshold, 1, 0)

# Vytvorenie a výpis confusion matrix
conf_matrix <- table(Predicted = predicted_classes, Actual = test_data$ObesityBinary)
print(conf_matrix)

# Výpočet presnosti a ďalších metrík
accuracy <- sum(predicted_classes == test_data$ObesityBinary) / nrow(test_data)
print(paste("Accuracy:", accuracy))

# Výpočet ďalších metrík pomocou balíka caret
library(caret)
confusionMatrix(conf_matrix)

# ========================SVM===============================
# Vytvorenie modelu SVM s radiálnym jadrom
svm_model <- svm(ObesityBinary ~ FreqConsHighCalFood + FreqConsVegs + NumMainMeals + ConsFoodBetwMeals + ConsAlc + Age + PhysActFreq + Gender, 
                 data = train_data, 
                 type = 'C-classification', 
                 kernel = 'radial', 
                 probability = TRUE)  # Zapnutie pravdepodobností

# Výpis výsledkov modelu
print(summary(svm_model))

# Predikcia na testovacej sade, získanie pravdepodobností
svm_probabilities <- predict(svm_model, newdata = test_data, probability = TRUE)
svm_probabilities <- attr(svm_probabilities, "probabilities")[,2]

# Nastavenie prahu
threshold <- 0.7

# Aplikovanie prahu na predpovede
svm_predicted_classes <- ifelse(svm_probabilities > threshold, 1, 0)

# Vytvorenie a výpis confusion matrix
svm_conf_matrix <- table(Predicted = svm_predicted_classes, Actual = test_data$ObesityBinary)
print(svm_conf_matrix)

# Výpočet presnosti
svm_accuracy <- sum(svm_predicted_classes == test_data$ObesityBinary) / nrow(test_data)
print(paste("Accuracy:", svm_accuracy))

# Výpočet ďalších metrík pomocou balíka caret
library(caret)
svm_confusionMatrix <- confusionMatrix(svm_conf_matrix)
print(svm_confusionMatrix)


# ========================NAIVE BAYES===============================
# Vytvorenie modelu Naive Bayes
nb_model <- naiveBayes(ObesityBinary ~ FreqConsHighCalFood + FreqConsVegs + NumMainMeals + ConsFoodBetwMeals + ConsAlc + Age + PhysActFreq + Gender, 
                       data = train_data)

# Výpis výsledkov modelu
print(summary(nb_model))

# Predikcia na testovacej sade, získanie pravdepodobností
nb_probabilities <- predict(nb_model, newdata = test_data, type = "raw")[,2]

# Nastavenie prahu
threshold <- 0.5

# Aplikovanie prahu na predpovede
nb_predicted_classes <- ifelse(nb_probabilities > threshold, 1, 0)

# Vytvorenie a výpis confusion matrix
nb_conf_matrix <- table(Predicted = nb_predicted_classes, Actual = test_data$ObesityBinary)
print(nb_conf_matrix)

# Výpočet presnosti
nb_accuracy <- sum(nb_predicted_classes == test_data$ObesityBinary) / nrow(test_data)
print(paste("Accuracy:", nb_accuracy))

# Výpočet ďalších metrík pomocou balíka caret
nb_confusionMatrix <- confusionMatrix(nb_conf_matrix)
print(nb_confusionMatrix)
