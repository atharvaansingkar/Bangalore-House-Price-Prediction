getwd()
setwd('D:/DS Course Project')

#Data preprocessing and encoding is done in python using pandas and numpy libraries
#This processed dataset is imported to R studio for further evaluation.

#reading the data
dataset = read.csv("D:/DS Course Project/modified_reds_data.csv")
dataset
dataset <- subset(dataset, select = -location)
dataset

#Splitting the dataset
library(caTools)
set.seed(398) 
split <- sample.split(dataset$price, SplitRatio = 0.7)

training1 <- subset(dataset, split == TRUE)
testing1 <- subset(dataset, split == FALSE)

#data normalization
library(dplyr)
selected_cols <- c("total_sqft", "bath", "bhk")

train <- training1 %>%
  select(all_of(selected_cols)) %>%
  mutate(across(everything(), scale))
train

test <- testing1 %>%
  select(all_of(selected_cols)) %>%
  mutate(across(everything(), scale))
test

testing1[, c("total_sqft", "bath", "bhk")] <- list(test$total_sqft, test$bath, test$bhk)
testing1

training1[, c("total_sqft", "bath", "bhk", "price")] <- list(train$total_sqft, train$bath, train$bhk, training1$price)
training1

write.csv(training1, "training.csv")
write.csv(testing1, "testing.csv")

#Linear Regression
library(class)

#model training
model_lm <- lm(price ~ ., data = training1)

#prediction
prediction_lm <- predict(model_lm, newdata = testing1)

#evaluation
actual_price_lm <- testing1$price

r_squared <- 1 - (sum((actual_price_lm - prediction_lm)^2) / sum((actual_price_lm - mean(actual_price_lm))^2))
print(paste("R-squared (Coefficient of Determination):", r_squared))


#Random Forest
library(randomForest)

model_rf <- randomForest(price ~ ., data = training1)


prediction_rf <- predict(model_rf, newdata = testing)

actual_price_rf <- testing1$price  

# Calculate R-squared
SSR <- sum((prediction_rf - actual_price_rf)^2) 
SST <- sum((actual_price_rf - mean(actual_price_rf))^2)  
r_squared <- 1 - (SSR / SST)
print(paste("R-squared:", r_squared))


#SVM
library(e1071)

model_svm <- svm(price ~ ., data = training1, kernel = "radial")

prediction_svm <- predict(model_svm, newdata = testing1)

actual_price_svm <-testing1$price

SSR <- sum((prediction_svm - actual_price_svm)^2)  # Regression sum of squares
SST <- sum((actual_price_svm - mean(actual_price_svm))^2)
r_squared_svm <- 1 - (SSR / SST)
print(paste("R-squared (Coefficient of Determination):", r_squared_svm))



