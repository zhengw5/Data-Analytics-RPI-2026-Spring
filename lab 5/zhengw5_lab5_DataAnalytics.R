## load libraries
library(ggplot2)
library(ggfortify)
library(GGally)
library(e1071)
library(class)
library(psych)
library(readr)

## set working directory so that files can be referenced without the full path
setwd("C:/Users/willi/Documents/Data Analysis/lab/lab4")

## read dataset
wine <- read_csv("wine.data", col_names = FALSE)

## set column names
names(wine) <- c("Type","Alcohol","Malic acid","Ash","Alcalinity_of_ash","Magnesium","Total phenols","Flavanoids","Nonflavanoid Phenols","Proanthocyanins","Color Intensity","Hue","Od280/od315 of diluted wines","Proline")

## inspect data frame
head(wine)
wine$Type <- as.factor(wine$Type)
set.seed(1)

train_idx <- sample(1:nrow(wine), 0.7 * nrow(wine))
train_PCA <- wine[train_idx, c("Type", "Proline", "Magnesium", "Alcalinity_of_ash")]
test_PCA <- wine[-train_idx, c("Type", "Proline", "Magnesium", "Alcalinity_of_ash")]
Y_train <- wine$Type[train_idx]
Y_test  <- wine$Type[-train_idx]

gamma.range <- seq(.1,10,.1)
gamma.range

cost.range <- seq(0.1, 10, 0.5)

tuned.linear <- tune.svm(Type ~ Proline + Magnesium + Alcalinity_of_ash,
                         data = train_PCA,
                         kernel = "linear",
                         cost = cost.range)

tuned.radial <- tune.svm(Type ~ Proline + Magnesium + Alcalinity_of_ash,
                         data = train_PCA,
                         kernel = "radial",
                         cost = cost.range,
                         gamma = gamma.range)

svm.mod1 <- svm(Type ~ Proline + Magnesium + Alcalinity_of_ash,
                data = train_PCA,
                kernel = "linear",
                cost = tuned.linear$best.parameters$cost)

svm.mod2 <- svm(Type ~ Proline + Magnesium + Alcalinity_of_ash,
                data = train_PCA,
                kernel = "radial",
                cost = tuned.radial$best.parameters$cost,
                gamma = tuned.radial$best.parameters$gamma)

pred_svm1 <- predict(svm.mod1, newdata = test_PCA)
pred_svm2 <- predict(svm.mod2, newdata = test_PCA)

conf_svm1 <- table(Predicted = pred_svm1, Actual = Y_test)
conf_svm2 <- table(Predicted = pred_svm2, Actual = Y_test)

precision_svm1 <- diag(conf_svm1) / rowSums(conf_svm1)
recall_svm1 <- diag(conf_svm1) / colSums(conf_svm1)
F1_svm1 <- 2 * (precision_svm1 * recall_svm1) / (precision_svm1 + recall_svm1)

precision_svm2 <- diag(conf_svm2) / rowSums(conf_svm2)
recall_svm2 <- diag(conf_svm2) / colSums(conf_svm2)
F1_svm2 <- 2 * (precision_svm2 * recall_svm2) / (precision_svm2 + recall_svm2)

conf_svm1
precision_svm1
recall_svm1
F1_svm1

conf_svm2

precision_svm2
recall_svm2
F1_svm2
#_____________________________________

X_subset <- wine[, c("Proline", "Magnesium", "Alcalinity_of_ash")]
Y <- wine$Type

set.seed(1)

train_idx <- sample(1:nrow(wine), 0.7*nrow(wine))

X_train <- X_subset[train_idx, ]
X_test  <- X_subset[-train_idx, ]

Y_train <- Y[train_idx]
Y_test  <- Y[-train_idx]

X_train_scaled <- scale(X_train)
X_test_scaled <- scale(X_test,
                       center = attr(X_train_scaled,"scaled:center"),
                       scale  = attr(X_train_scaled,"scaled:scale"))

k <- 5

pred1 <- knn(train = X_train_scaled,
             test = X_test_scaled,
             cl = Y_train,
             k = k)

table(Predicted = pred1, Actual = Y_test)

mean(pred1 == Y_test)

conf1 <- table(Predicted = pred1, Actual = Y_test)
conf1

precision1 <- diag(conf1) / rowSums(conf1)
precision1

recall1 <- diag(conf1) / colSums(conf1)
recall1


F1_1 <- 2 * (precision1 * recall1) / (precision1 + recall1)
F1_1

