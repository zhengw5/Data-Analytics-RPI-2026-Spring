##########################################
### Principal Component Analysis (PCA) ###
##########################################

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
names(wine) <- c("Type","Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium","Total phenols","Flavanoids","Nonflavanoid Phenols","Proanthocyanins","Color Intensity","Hue","Od280/od315 of diluted wines","Proline")

## inspect data frame
head(wine)

## change the data type of the "Type" column from character to factor
####
# Factors look like regular strings (characters) but with factors R knows 
# that the column is a categorical variable with finite possible values
# e.g. "Type" in the Wine dataset can only be 1, 2, or 3
####

wine$Type <- as.factor(wine$Type)


## visualize variables
pairs.panels(wine[,-1],gap = 0,bg = c("red", "yellow", "blue")[wine$Type],pch=21)

ggpairs(wine, ggplot2::aes(colour = Type))

###

X <- wine[,-1]
Y <- wine$Type

###
Xmat <- as.matrix(X)

Xc <- scale(Xmat, center = T, scale = F)
# compute covariance matrix
C <- cov(Xc)

C
C.eigens <- eigen(C)
C.eigens$values

sort(C.eigens$values, decreasing = TRUE)
V <- C.eigens$vectors

abs(V[,1])
sort(abs(V[,1]), decreasing = TRUE)
loadings <- data.frame(V)
rownames(loadings) <- colnames(X)

pc1_loadings <- abs(loadings[,1])
names(pc1_loadings) <- rownames(loadings)

sort(pc1_loadings, decreasing = TRUE)


# Proline contributes majority and magnesium is the second largest contribution

Z <- Xc %*% V

pca_df <- data.frame(
  PC1 = Z[,1],
  PC2 = Z[,2],
  Type = Y
)

ggplot(pca_df, aes(x = PC1, y = PC2, color = Type)) +
  geom_point(size = 2) +
  labs(title = "PCA of Wine Dataset",
       x = "First Principal Component",
       y = "Second Principal Component") +
  theme_minimal()


#----------------------
X_subset <- wine[, c("Proline", "Magnesium", "Alcalinity of ash")]
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

table(Predicted = pred, Actual = Y_test)

mean(pred1 == Y_test)


#----------------------------
pca <- princomp(X, cor = TRUE)


X_pca <- pca$scores[,1:2]


train_idx <- sample(1:nrow(X_pca), 0.7*nrow(X_pca))

train <- X_pca[train_idx,]
test  <- X_pca[-train_idx,]

Y_train <- Y[train_idx]
Y_test  <- Y[-train_idx]

pred2 <- knn(train, test, cl = Y_train, k = 5)

mean(pred2 == Y_test)


conf1 <- table(Predicted = pred1, Actual = Y_test)
conf1

conf2 <- table(Predicted = pred2, Actual = Y_test)
conf2

precision1 <- diag(conf1) / rowSums(conf1)
precision1

precision2 <- diag(conf2) / rowSums(conf2)
precision2
recall1 <- diag(conf1) / colSums(conf1)
recall1

recall2 <- diag(conf2) / colSums(conf2)
recall2

F1_1 <- 2 * (precision1 * recall1) / (precision1 + recall1)
F1_1

F1_2 <- 2 * (precision2 * recall2) / (precision2 + recall2)
F1_2
