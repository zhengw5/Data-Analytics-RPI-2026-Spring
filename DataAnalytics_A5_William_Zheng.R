## load libraries
library(ggplot2)
library(ggfortify)
library(GGally)
library(e1071)
library(class)
library(psych)
library(dplyr)
library(readr)
library(lubridate)

## set working directory so that files can be referenced without the full path
setwd("C:/Users/willi/Documents/Data Analysis/Assignment 5")

## read dataset
NYC <- read_csv("NYC_Citywide_Annualized_Calendar_Sales_Update_20241107.csv")

# EDA
dim(NYC)
str(NYC)
summary(NYC)
head(NYC)

# Check column names (NYC sales data has inconsistent naming)
names(NYC)
unique(NYC$BOROUGH)
table(NYC$BOROUGH, useNA = "ifany")
manhattan <- NYC %>%filter(BOROUGH == "MANHATTAN")

dim(manhattan)
table(manhattan$BOROUGH, useNA = "ifany")

manhattan$`SALE PRICE` <- as.numeric(manhattan$`SALE PRICE`)

# Clean data: remove $0 sales and NAs in price
manhattan_clean <- manhattan %>%
  filter(`SALE PRICE` > 0, !is.na(`SALE PRICE`))

dim(manhattan_clean)

manhattan_clean$month <- floor_date(as.Date(manhattan_clean$`SALE DATE`, format = "%m/%d/%Y"), "month")

monthly <- aggregate(`SALE PRICE` ~ month, data = manhattan_clean, FUN = median)


ggplot(monthly, aes(x = month, y = `SALE PRICE`)) +
  geom_line() +
  labs(title = "Median Sale Price Over Time", x = "Date", y = "Sale Price")

manhattan_clean$`GROSS SQUARE FEET` <- as.numeric(manhattan_clean$`GROSS SQUARE FEET`)


ggplot(manhattan_clean, aes(x = `GROSS SQUARE FEET`, y = `SALE PRICE`)) +
  geom_point(alpha = 0.3) +
  scale_x_log10() +
  scale_y_log10() +
  labs(title = "Gross Square Feet vs Sale Price")

#random sample of 1000 points for cleaner data
manhattan_sample <- manhattan_clean[sample(nrow(manhattan_clean), 1000), ]

ggplot(manhattan_sample, aes(x = `YEAR BUILT`, y = `SALE PRICE`)) +
  geom_point(alpha = 0.3) +
  scale_y_log10() +
  labs(title = "Year Built vs Sale Price")

ggplot(manhattan_sample, aes(x = Longitude, y = Latitude, color = log10(`SALE PRICE`))) +
  geom_point(alpha = 0.4) +
  scale_color_viridis_c() +
  labs(title = "Sale Price by Location")
#----------------------------------
# After some initial exploration of the data using dim, str,
# and summary, the dataset is filtered down to Manhattan sales only. The sale price column is
# converted to numeric and zero-dollar sales are removed to create a clean working dataset.
# From there, four exploratory plots are produced: a line plot of median sale price over time
# aggregated by month, a scatter plot of gross square feet versus sale price on a log scale,
# and - using a random sample of 1000 properties for clarity - a scatter plot of year built
# versus sale price and a spatial scatter plot of property locations colored by sale price.
#----------------------------------

# summary shows min, max, and quartiles - extreme min/max values are your outliers
summary(manhattan_clean$`SALE PRICE`)
summary(manhattan_clean$`GROSS SQUARE FEET`)
summary(manhattan_clean$`YEAR BUILT`)
summary(manhattan_clean$`TOTAL UNITS`)

# boxplots visually show the outliers as dots beyond the whiskers
boxplot(manhattan_clean$`SALE PRICE`, main = "Sale Price")
boxplot(manhattan_clean$`GROSS SQUARE FEET`, main = "Gross Square Feet")
boxplot(manhattan_clean$`YEAR BUILT`, main = "Year Built")
boxplot(manhattan_clean$`TOTAL UNITS`, main = "Total Units")


manhattan_clean$`LAND SQUARE FEET` <- as.numeric(manhattan_clean$`LAND SQUARE FEET`)
manhattan_clean$`TOTAL UNITS`      <- as.numeric(manhattan_clean$`TOTAL UNITS`)
manhattan_clean$`GROSS SQUARE FEET`<- as.numeric(manhattan_clean$`GROSS SQUARE FEET`)
manhattan_clean$`YEAR BUILT`       <- as.numeric(manhattan_clean$`YEAR BUILT`)

reg_data <- manhattan_clean[
  !is.na(manhattan_clean$`SALE PRICE`) &
    !is.na(manhattan_clean$`GROSS SQUARE FEET`) &
    !is.na(manhattan_clean$`LAND SQUARE FEET`) &
    !is.na(manhattan_clean$`TOTAL UNITS`) &
    !is.na(manhattan_clean$`YEAR BUILT`) &
    manhattan_clean$`GROSS SQUARE FEET` > 0 &
    manhattan_clean$`LAND SQUARE FEET`  > 0 &
    manhattan_clean$`YEAR BUILT`        > 0 &
    manhattan_clean$`TOTAL UNITS`       > 0, ]
dim(reg_data)

# Normalize for smaller units
reg_data$log_price <- log10(reg_data$`SALE PRICE`)
reg_data$log_sqft  <- log10(reg_data$`GROSS SQUARE FEET`)
reg_data$log_land  <- log10(reg_data$`LAND SQUARE FEET`)

set.seed(12)
train_index <- sample(nrow(reg_data), 0.8 * nrow(reg_data))
train <- reg_data[train_index, ]
test  <- reg_data[-train_index, ]

model1 <- lm(log_price ~ log_sqft, data = train)
summary(model1)

pred1 <- predict(model1, newdata = test)
rmse1 <- sqrt(mean((test$log_price - pred1)^2, na.rm = TRUE))
cat("Model 1 RMSE:", rmse1, "\n")

# Model 2: Add Land Square Feet and Total Units

model2 <- lm(log_price ~ log_sqft + log_land + `TOTAL UNITS`, data = train)
summary(model2)

pred2 <- predict(model2, newdata = test)
rmse2 <- sqrt(mean((test$log_price - pred2)^2, na.rm = TRUE))
cat("Model 2 RMSE:", rmse2, "\n")

# Model 3: Add Year Built

model3 <- lm(log_price ~ log_sqft + log_land + `TOTAL UNITS` + `YEAR BUILT`, data = train)
summary(model3)

pred3 <- predict(model3, newdata = test)
rmse3 <- sqrt(mean((test$log_price - pred3)^2, na.rm = TRUE))
cat("Model 3 RMSE:", rmse3, "\n")


cat("\nModel Comparison (lower RMSE = better):\n")
cat("Model 1 RMSE:", rmse1, "\n")
cat("Model 2 RMSE:", rmse2, "\n")
cat("Model 3 RMSE:", rmse3, "\n")

cat("\nModel R-squared values:\n")
cat("Model 1 R^2:", summary(model1)$r.squared, "\n")
cat("Model 2 R^2:", summary(model2)$r.squared, "\n")
cat("Model 3 R^2:", summary(model3)$r.squared, "\n")

# -----------------------------------------------------
# The best model was model 3 with the lowest RMSE and highest R-squared value.
# I had to first convert the columns that were numbers to numeric type, then clean out
# any Na or 0 values. I also normalized the data so the numbers wouldn't have as large of a range.
# The reason for the poor performance is most likely due to the extreme outliers.
#------------------------------------------------------
install.packages("randomForest")
library(randomForest)

table(manhattan_clean$NEIGHBORHOOD)

neighborhoods <- c("UPPER EAST SIDE (59-79)", "UPPER WEST SIDE (59-79)", 
                   "MIDTOWN WEST", "CHELSEA")

hood_data <- manhattan_clean[manhattan_clean$NEIGHBORHOOD %in% neighborhoods, ]
table(hood_data$NEIGHBORHOOD)


hood_data$`GROSS SQUARE FEET` <- as.numeric(hood_data$`GROSS SQUARE FEET`)
hood_data$`LAND SQUARE FEET`  <- as.numeric(hood_data$`LAND SQUARE FEET`)
hood_data$`TOTAL UNITS`       <- as.numeric(hood_data$`TOTAL UNITS`)
hood_data$`YEAR BUILT`        <- as.numeric(hood_data$`YEAR BUILT`)

# Keep only complete cases
hood_clean <- hood_data[
  !is.na(hood_data$`SALE PRICE`) &
    !is.na(hood_data$`GROSS SQUARE FEET`) &
    !is.na(hood_data$`TOTAL UNITS`) &
    !is.na(hood_data$`YEAR BUILT`) &
    hood_data$`GROSS SQUARE FEET` > 0 &
    hood_data$`YEAR BUILT` > 0, ]
dim(hood_clean)
# Convert neighborhood to factor
hood_clean$NEIGHBORHOOD <- as.factor(hood_clean$NEIGHBORHOOD)

train_index <- sample(nrow(hood_clean), 0.8 * nrow(hood_clean))
train <- hood_clean[train_index, ]
test  <- hood_clean[-train_index, ]

# Features to use
features <- c("SALE_PRICE", "GROSS_SQ_FT", "TOTAL_UNITS", "YEAR_BUILT")

train_x <- train[, features]
test_x  <- test[,  features]
train_y <- train$NEIGHBORHOOD
test_y  <- test$NEIGHBORHOOD
train$SALE_PRICE    <- train$`SALE PRICE`
train$GROSS_SQ_FT   <- train$`GROSS SQUARE FEET`
train$TOTAL_UNITS   <- train$`TOTAL UNITS`
train$YEAR_BUILT    <- train$`YEAR BUILT`

test$SALE_PRICE    <- test$`SALE PRICE`
test$GROSS_SQ_FT   <- test$`GROSS SQUARE FEET`
test$TOTAL_UNITS   <- test$`TOTAL UNITS`
test$YEAR_BUILT    <- test$`YEAR BUILT`
pred_knn <- knn(train_x, test_x, train_y, k = 5)

cat("kNN Contingency Table:\n")
table(Predicted = pred_knn, Actual = test_y)


model_rf <- randomForest(NEIGHBORHOOD ~ SALE_PRICE + GROSS_SQ_FT + 
                           TOTAL_UNITS + YEAR_BUILT, data = train)

pred_rf <- predict(model_rf, newdata = test)

cat("Random Forest Contingency Table:\n")
table(Predicted = pred_rf, Actual = test_y)


model_nb <- naiveBayes(NEIGHBORHOOD ~ SALE_PRICE + GROSS_SQ_FT + 
                         TOTAL_UNITS + YEAR_BUILT, data = train)

pred_nb <- predict(model_nb, newdata = test)

cat("Naive Bayes Contingency Table:\n")
table(Predicted = pred_nb, Actual = test_y)


get_metrics <- function(predicted, actual) {
  cm <- table(Predicted = predicted, Actual = actual)
  precision <- diag(cm) / rowSums(cm)
  recall    <- diag(cm) / colSums(cm)
  f1        <- 2 * (precision * recall) / (precision + recall)
  cat("Precision:", round(precision, 3), "\n")
  cat("Recall:   ", round(recall, 3),    "\n")
  cat("F1 Score: ", round(f1, 3),        "\n")
  cat("Overall Accuracy:", round(mean(predicted == actual), 3), "\n\n")
}

cat(" kNN Metrics \n");          get_metrics(pred_knn, test_y)
cat(" Random Forest Metrics \n"); get_metrics(pred_rf,  test_y)
cat(" Naive Bayes Metrics \n");   get_metrics(pred_nb,  test_y)

#---------------------------------------------
# Three supervised learning models were trained to classify Manhattan properties into one of
# four neighborhoods (Chelsea, Midtown West, Upper East Side, Upper West Side) using sale price,
# gross square feet, total units, and year built as features. Prior to modeling, columns were
# converted to numeric and rows with missing or zero values were removed, as these would cause
# errors in the models, resulting in a cleaned dataset of 127 rows. Random Forest performed
# best with 57.7% accuracy, followed by kNN at 53.8% and Naive Bayes at 34.6%; the relatively
# low accuracy across all models is likely due to the small sample size and the fact that these
# quantitative variables alone are not strongly distinctive between Manhattan neighborhoods.
#---------------------------------------------

#---------------------------------------------
# The NYC sales dataset is large and rich but presented several quality issues, including
# columns stored as characters instead of numbers, zero-dollar sales representing non-market
# transfers, and a significant number of missing values, all of which required cleaning before
# any modeling could be done. The regression models performed poorly, with R-squared values
# of only 3-4%, suggesting that gross square feet, land square feet, total units, and year
# built alone are insufficient predictors of sale price in Manhattan, where location and
# building class likely drive prices far more than physical size. The classification models
# showed modest results, with Random Forest performing best at 57.7% accuracy, while Naive
# Bayes struggled significantly at 34.6%; overall, the quantitative variables used were not
# distinctive enough between neighborhoods to allow reliable classification, and a larger
# and more balanced dataset would likely improve model performance substantially.
#---------------------------------------------