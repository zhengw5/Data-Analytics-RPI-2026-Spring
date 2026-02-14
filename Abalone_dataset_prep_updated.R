################################################
#### Evaluating Classification & CLustering ####
################################################

library("caret")
library(GGally)
library(psych)

setwd("C:/Users/willi/Documents/Data Analysis/lab/lab3")

## read data
abalone <- read.csv("abalone.data", header=FALSE)

## rename columns
colnames(abalone) <- c("sex", "length", 'diameter', 'height', 'whole_weight', 'shucked_wieght', 'viscera_wieght', 'shell_weight', 'rings' ) 

## derive age group based in number of rings
abalone$age.group <- cut(abalone$rings, br=c(0,8,11,35), labels = c("young", 'adult', 'old'))

## take copy removing sex and rings
abalone.sub <- abalone[,c(2:8,10)]

## convert class labels to strings
abalone.sub$age.group <- as.character(abalone.sub$age.group)

## convert back to factor
abalone.sub$age.group <- as.factor(abalone.sub$age.group)

## split train/test
train.indexes <- sample(4177,0.7*4177)

train <- abalone.sub[train.indexes,]
test <- abalone.sub[-train.indexes,]

## separate x (features) & y (class labels)
X <- train[,1:7] 
Y <- train[,8]

## features subset
# train <- train[,5:8]
# test <- test[,5:8]

## feature boxplots
boxplot(X, main="abalone features")

## class label distributions
plot(Y)


## feature-class plots
featurePlot(x=X, y=Y, plot="ellipse")

featurePlot(x=X, y=Y, plot="box")

scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=X, y=Y, plot="density", scales=scales)

## psych scatterplot matrix
pairs.panels(X,gap = 0,bg = c("pink", "green", "blue")[Y],pch=21)

## GGally 
ggpairs(train, ggplot2::aes(colour = Y))



## EOF ##



# --- two feature subsets ---
features_A <- c("length","diameter","height","whole_weight","shucked_wieght","viscera_wieght","shell_weight")
features_B <- c("length","diameter","height","shell_weight")  # different subset

# helper: build X/Y for each model
X_train_A <- train[, features_A]
X_test_A  <- test[,  features_A]

X_train_B <- train[, features_B]
X_test_B  <- test[,  features_B]

Y_train <- train$age.group
Y_test  <- test$age.group

# pick an initial k (you'll tune later)
k0 <- 5

# preprocess (center/scale) using TRAIN only, then apply to test
pp_A <- preProcess(X_train_A, method = c("center","scale"))
X_train_A_sc <- predict(pp_A, X_train_A)
X_test_A_sc  <- predict(pp_A, X_test_A)

pp_B <- preProcess(X_train_B, method = c("center","scale"))
X_train_B_sc <- predict(pp_B, X_train_B)
X_test_B_sc  <- predict(pp_B, X_test_B)

# train knn models using caret::knn3
model_A <- knn3(x = X_train_A_sc, y = Y_train, k = k0)
model_B <- knn3(x = X_train_B_sc, y = Y_train, k = k0)

# predict on test
pred_A <- predict(model_A, X_test_A_sc, type = "class")
pred_B <- predict(model_B, X_test_B_sc, type = "class")

cm_A <- confusionMatrix(pred_A, Y_test)
cm_B <- confusionMatrix(pred_B, Y_test)

cm_A$table   # contingency table for model A
cm_B$table   # contingency table for model B

cm_A$overall["Accuracy"]
cm_B$overall["Accuracy"]


k_values <- seq(1, 51, by = 2)
acc <- numeric(length(k_values))

for (i in seq_along(k_values)) {
  k <- k_values[i]
  m <- knn3(x = X_train_A_sc, y = Y_train, k = k)
  p <- predict(m, X_test_A_sc, type = "class")
  acc[i] <- sum(p == Y_test) / length(Y_test)  
}

results <- data.frame(k = k_values, accuracy = acc)
results[order(-results$accuracy), ][1:10, ]  

best_k <- results$k[which.max(results$accuracy)]
best_k


# -----------Exercise 2------------------
library(cluster) 

# use same feature subset as best kNN model
X_cluster <- abalone.sub[, features_A]  

# scale features (very important for clustering)
X_cluster <- scale(X_cluster)

k_range <- 2:10
sil_kmeans <- numeric(length(k_range))

for (i in seq_along(k_range)) {
  k <- k_range[i]
  
  km_model <- kmeans(X_cluster, centers = k, nstart = 25)
  
  sil <- silhouette(km_model$cluster, dist(X_cluster))
  sil_kmeans[i] <- mean(sil[,3])
}

# Put results in dataframe
kmeans_results <- data.frame(k = k_range, silhouette = sil_kmeans)

kmeans_results

best_k_kmeans <- kmeans_results$k[which.max(kmeans_results$silhouette)]
best_k_kmeans


km_best <- kmeans(X_cluster, centers = best_k_kmeans, nstart = 25)

sil_km_best <- silhouette(km_best$cluster, dist(X_cluster))

plot(sil_km_best, main = paste("K-means Silhouette (k =", best_k_kmeans, ")"))

sil_pam <- numeric(length(k_range))

for (i in seq_along(k_range)) {
  k <- k_range[i]
  
  pam_model <- pam(X_cluster, k)
  
  sil <- silhouette(pam_model$cluster, dist(X_cluster))
  sil_pam[i] <- mean(sil[,3])
}

pam_results <- data.frame(k = k_range, silhouette = sil_pam)

pam_results

best_k_pam <- pam_results$k[which.max(pam_results$silhouette)]
best_k_pam

pam_best <- pam(X_cluster, best_k_pam)

sil_pam_best <- silhouette(pam_best$cluster, dist(X_cluster))

plot(sil_pam_best, main = paste("PAM Silhouette (k =", best_k_pam, ")"))
