library(readr)
library(tidyverse)
library(lobstr)
library(dplyr)
library(ggplot2)
library(magrittr)
library(data.table) # Implements the %like% operator
library(caret)
library(MASS)
# library(cutpointr)
library(purrr)
library(GGally)
library(ggplot2)
library(gplots)
library(pROC)

# Setting the working directory to where the dataset is located
# Uncomment the appropriate line below to match your directory structure

# setwd("C:/Users/David Wagner/Desktop/Zadania/4. Semester/OZNAL/Elements of AI/Data")
setwd("C:/Users/dswag/Desktop/Zadania/OZNAL/Data")

# Listing all files in the current working directory to verify the presence of our dataset
list.files()

# Reading the dataset into R
raw_data <- read_csv("ObesityDataSet.csv", col_names = TRUE, num_threads = 4)

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

# Displaying the first few rows of the dataset to ensure it's loaded correctly
head(raw_data)

# view(raw_data)

# Identifying missing values
sum(is.na(raw_data))
colSums(is.na(raw_data))

head(raw_data)

# # Function to identify outliers within one column
# remove_outliers <- function(x) {
#   # Ensure the column is numeric
#   if(is.numeric(x)) {
#     Q1 <- quantile(x, 0.25, na.rm = TRUE)
#     Q3 <- quantile(x, 0.75, na.rm = TRUE)
#     IQR <- Q3 - Q1
#     lower_bound <- Q1 - 1.5 * IQR
#     upper_bound <- Q3 + 1.5 * IQR
#     x[x < lower_bound | x > upper_bound] <- NA  # Assign NA to outliers
#   }
#   x
# }
# 
# # Apply the function to each column
# cleaned_data <- raw_data %>% mutate(across(where(is.numeric), remove_outliers))

cleaned_data <- raw_data
# Optionally, if you want to remove rows with any NAs (which might have been introduced by removing outliers)
cleaned_data %<>% drop_na()
raw_data
cleaned_data

# Check for missing values again
sum(is.na(cleaned_data))
# Check data types and convert if necessary
str(cleaned_data)

# Convert all character columns to factors
cleaned_data <- cleaned_data %>%
  mutate(across(where(is.character), as.factor))

str(cleaned_data)

cleaned_data

cleaned_data <- cleaned_data %>% distinct()

cleaned_data

# Assuming 'cleaned_data' is your preprocessed dataset containing only numeric columns
# Select only the numeric columns for the pairs plot
numerical_data <- cleaned_data[sapply(cleaned_data, is.numeric) ]

# Use ggpairs to create the pairs plot, displaying both scatter plots and correlations
pairs(numerical_data)

# # Loop over each numeric column and create a histogram
# for (col in names(numerical_data)) {
#   print(ggplot(numerical_data, aes_string(x = col)) + 
#           geom_histogram(bins = 10, fill = 'blue', color = 'black') + 
#           theme_minimal() +
#           ggtitle(paste("Distribution of", col)))
# }
# 
# 
# # Create histograms for all numeric variables
# numerical_cols <- names(cleaned_data[sapply(cleaned_data, is.numeric)])
# for (col in numerical_cols) {
#   p <- ggplot(cleaned_data, aes_string(x = col)) +
#     geom_histogram(bins = 30, fill = 'blue', color = 'black') +
#     labs(title = paste("Histogram of", col), x = col, y = "Frequency") +
#     theme_minimal()
#   print(p)
# }

cleaned_data %<>% 
  mutate(BMI = Weight / (Height)^2)

head(cleaned_data)

# Assuming 'numerical_data' contains only numeric columns and you've already calculated the correlation matrix 'cor_matrix'
cor_matrix <- cor(numerical_data, use = "complete.obs")

cor_matrix


# Plotting the heatmap using base R's heatmap() function
heatmap.2(x = cor_matrix,
          scale = "none", 
          Colv = NA, 
          Rowv = NA, 
          dendrogram = "none",
          trace = "none", 
          col = colorRampPalette(c("blue", "white", "red"))(n = 299), 
          margin = c(5, 5), 
          main = "Correlation Matrix Heatmap",
          key = TRUE, # This ensures the color key (legend) is displayed
          keysize = 1.5)


# regression_formula <- Age ~ FreqConsHighCalFood + FreqConsVegs + NumMainMeals + ConsFoodBetwMeals + ConsWaterDaily + ConsAlc + PhysActFreq + TimeTechDev + Trans + CalsConsMon
# 
# # Fit the linear regression model
# age_model <- lm(regression_formula, data = cleaned_data)
# 
# # Summarize the model to view coefficients and statistics
# summary(age_model)

# Fit the linear regression model
model <- lm(Weight ~ Height + FreqConsHighCalFood + FreqConsVegs + NumMainMeals + PhysActFreq, data = cleaned_data)

# Summarize the model to view coefficients and other statistics
summary(model)

# If you want to check the diagnostic plots for assumptions
par(mfrow = c(2, 2))
plot(model)

# Creating a new binary variable 'ObesityBinary' where 1 indicates Obesity (I, II, III) and 0 indicates otherwise
cleaned_data$ObesityBinary <- ifelse(cleaned_data$Nobesity %in% c("Obesity_Type_I", "Obesity_Type_II", "Obesity_Type_III"), 1, 0)

set.seed(123) # for reproducibility

# Calculate the size of the training set (80% of the dataset)
training_size <- floor(0.8 * nrow(cleaned_data))

# Sample indices for the training data
training_indices <- sample(seq_len(nrow(cleaned_data)), size = training_size)

# Split the data
train_data <- cleaned_data[training_indices, ]
test_data <- cleaned_data[-training_indices, ]

# Fit the model on the training data
model_train <- lm(Weight ~ Height + FreqConsHighCalFood + FreqConsVegs + NumMainMeals + PhysActFreq, data = train_data)

# Use the model to make predictions on the test data
predictions <- predict(model_train, newdata = test_data)

# Evaluate the model's performance
# You can calculate the Root Mean Squared Error (RMSE) as an example
actuals <- test_data$Weight
rmse <- sqrt(mean((predictions - actuals)^2))

# Print the RMSE
print(paste("RMSE on test data:", rmse))

# Summarize the model to view coefficients and other statistics
summary(model_train)

# If you want to check the diagnostic plots for assumptions
par(mfrow = c(2, 2))
plot(model_train)



# # Fit the polynomial regression model with quadratic terms
# polynomial_model <- lm(Weight ~ Height + I(Height^2) + FreqConsVegs + I(FreqConsVegs^2), data = cleaned_data)
# 
# # Summarize the model to view coefficients and other statistics
# summary(polynomial_model)
# 
# # Diagnostic plots for the polynomial model
# par(mfrow = c(2, 2))
# plot(polynomial_model)

# # Get the AIC value for the polynomial model
# aic_value <- AIC(polynomial_model)
# 
# # Get the BIC value for the polynomial model
# bic_value <- BIC(polynomial_model)
# 
# # Print the AIC and BIC values
# print(paste("AIC value for the polynomial model:", aic_value))
# print(paste("BIC value for the polynomial model:", bic_value))
# 
# linear_model_aic <- AIC(model)
# linear_model_bic <- BIC(model)
# 
# # Then you can print them out or compare them with the polynomial model
# print(paste("AIC value for the linear model:", linear_model_aic))
# print(paste("BIC value for the linear model:", linear_model_bic))
# 
# # Calculate the MSE of the model
# model_predictions <- predict(model, cleaned_data)
# model_mse <- mean((cleaned_data$Weight - model_predictions)^2)
# 
# # Calculate the MSE of the baseline model
# baseline_predictions <- rep(mean(cleaned_data$Weight), nrow(cleaned_data))
# baseline_mse <- mean((cleaned_data$Weight - baseline_predictions)^2)
# 
# # Calculate the percentage improvement over baseline
# improvement <- (baseline_mse - model_mse) / baseline_mse * 100
# 
# # Print the improvement
# print(paste("Improvement over baseline model:", improvement, "%"))

# # Fit the logistic regression model
# classification_model <- glm(ObesityBinary ~ FreqConsHighCalFood + NumMainMeals + 
#                               ConsFoodBetwMeals + ConsAlc + Age + Gender, 
#                             data = cleaned_data, family = binomial())
# 
# # Summarize the model
# summary(classification_model)
# 
# # Diagnostic plots
# par(mfrow = c(2, 2))
# plot(classification_model)
# 
# # Predict probabilities
# probabilities <- predict(classification_model, type = "response")
# 
# # Calculate and plot ROC curve
# roc_result <- roc(cleaned_data$ObesityBinary, probabilities)
# plot(roc_result)
# 
# # Calculate AUC
# auc(roc_result)


# Fit the logistic regression model on the training data
classification_model_train <- glm(ObesityBinary ~ FreqConsHighCalFood + NumMainMeals +
                                    ConsFoodBetwMeals + ConsAlc + Age + Gender, 
                                  data = train_data, family = binomial())

# Summarize the model to view coefficients and other statistics
summary(classification_model_train)

# Predict probabilities on the test data
test_probabilities <- predict(classification_model_train, newdata = test_data, type = "response")

# Calculate ROC curve and AUC on test data
roc_test_result <- roc(test_data$ObesityBinary, test_probabilities)
plot(roc_test_result)
auc_test <- auc(roc_test_result)

# Print AUC
print(paste("AUC on test data:", auc_test))

# Convert probabilities to binary outcomes based on a threshold
predicted_outcomes <- ifelse(test_probabilities > 0.5, 1, 0)

# Confusion Matrix
table(Predicted = predicted_outcomes, Actual = test_data$ObesityBinary)

# Creating the confusion matrix with caret
conf_matrix <- confusionMatrix(as.factor(predicted_outcomes), as.factor(test_data$ObesityBinary))

# Printing the confusion matrix
print(conf_matrix$table)

# Plotting the confusion matrix with gplots
heatmap.2(as.matrix(conf_matrix$table),
          Rowv = NA, Colv = NA,
          col = colorRampPalette(c("blue", "white", "red"))(n = 100),
          dendrogram = "none", trace = "none",
          key = TRUE, keysize = 1,
          main = "Confusion Matrix Heatmap",
          xlab = "Actual", ylab = "Predicted")

# Printing the overall statistics from the confusion matrix
print(conf_matrix$overall)

# Specific metrics can be accessed directly
accuracy <- conf_matrix$overall['Accuracy']
precision <- conf_matrix$byClass['Pos Pred Value']
recall <- conf_matrix$byClass['Sensitivity'] # Recall is the same as Sensitivity
specificity <- conf_matrix$byClass['Specificity']

# Printing the metrics
cat("Accuracy:", accuracy, "\n",
    "Precision:", precision, "\n",
    "Recall (Sensitivity):", recall, "\n",
    "Specificity:", specificity, "\n")
