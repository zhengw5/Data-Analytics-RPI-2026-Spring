## load libraries
library(ggplot2)
library(ggfortify)
library(GGally)
library(e1071)
library(class)
library(dplyr)
library(tidyr)
library(randomForest)
library(caret)
library(cluster)
library(rpart)
library(rpart.plot)
library(reshape2)

## set working directory
setwd("C:/Users/willi/Documents/Data Analysis/Assignment 6/census+income/")

# ─────────────────────────────────────────────────────────────────
# SECTION 1: LOAD & EXPLORE
# ─────────────────────────────────────────────────────────────────

# Parse column names from adult.names

names_raw <- readLines("adult.names")
feature_lines <- names_raw[grepl("^[a-zA-Z].*:", names_raw)]
col_names <- gsub(":.*", "", feature_lines)
col_names <- gsub("-", "_", trimws(col_names))   # replace hyphens with underscores
col_names <- c(col_names, "income")              # target column is last in adult.data

cat("Column names parsed from adult.names:\n")
print(col_names)

adult <- read.csv("adult.data", header = FALSE, col.names = col_names,
                  strip.white = TRUE, na.strings = "?")

dim(adult)
str(adult)
summary(adult)
head(adult)
# ─────────────────────────────────────────────────────────────────
# SECTION 2: CLEANING
# ─────────────────────────────────────────────────────────────────

# Remove rows with any NA (missing encoded as '?' → already caught by na.strings)
cat("Missing values per column:\n")
print(colSums(is.na(adult)))

adult_clean <- adult[complete.cases(adult), ]
dim(adult_clean)

# Standardize income labels (strip trailing period from test set if ever merged)
adult_clean$income <- gsub("\\.", "", adult_clean$income)
adult_clean$income <- trimws(adult_clean$income)
table(adult_clean$income)

# Encode income as binary factor
adult_clean$income <- as.factor(make.names(adult_clean$income))

# Convert remaining character cols to factor
char_cols <- sapply(adult_clean, is.character)
adult_clean[char_cols] <- lapply(adult_clean[char_cols], as.factor)

str(adult_clean)

# ─────────────────────────────────────────────────────────────────
# SECTION 3: EDA FIGURES
# ─────────────────────────────────────────────────────────────────

## Figure 1 – Class balance
ggplot(adult_clean, aes(x = income, fill = income)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  labs(title = "Figure 1 – Income Class Distribution",
       x = "Income", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none")

## Figure 2 – Distributions of numeric features
num_cols <- c("age", "fnlwgt", "education_num", "capital_gain",
              "capital_loss", "hours_per_week")

adult_long <- adult_clean %>%
  select(all_of(num_cols)) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value")

ggplot(adult_long, aes(x = value)) +
  geom_histogram(bins = 30, fill = "#4C8BB5", color = "white") +
  facet_wrap(~ variable, scales = "free", ncol = 3) +
  labs(title = "Figure 2 – Numeric Feature Distributions") +
  theme_minimal()
## Figure 3 – Key categorical features vs income
cat_plot <- function(col, title) {
  ggplot(adult_clean, aes_string(x = col, fill = "income")) +
    geom_bar(position = "fill") +
    labs(title = title, x = col, y = "Proportion") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 40, hjust = 1))
}

cat_plot("education",      "Figure 5a – Education vs Income")
cat_plot("occupation",     "Figure 5b – Occupation vs Income")
cat_plot("marital_status", "Figure 5c – Marital Status vs Income")
cat_plot("sex",            "Figure 5d – Sex vs Income")

# ─────────────────────────────────────────────────────────────────
# SECTION 4: PREPROCESSING FOR MODELS
# ─────────────────────────────────────────────────────────────────

# One-hot encode categoricals for distance-based models
dummies      <- dummyVars(income ~ ., data = adult_clean, fullRank = TRUE)
adult_enc    <- predict(dummies, newdata = adult_clean)
adult_scaled <- scale(adult_enc)

y <- adult_clean$income

set.seed(59)
train_idx  <- createDataPartition(y, p = 0.8, list = FALSE)
X_train    <- adult_scaled[ train_idx, ]
X_test     <- adult_scaled[-train_idx, ]
y_train    <- y[ train_idx]
y_test     <- y[-train_idx]

# Also keep unscaled data frame for tree-based models
train_df   <- adult_clean[ train_idx, ]
test_df    <- adult_clean[-train_idx, ]

cat("Train size:", nrow(train_df), " | Test size:", nrow(test_df), "\n")
cat("Class balance (train):", table(y_train), "\n")

cv_ctrl <- trainControl(method = "cv", number = 5,
                        classProbs = TRUE, summaryFunction = twoClassSummary)

print_metrics <- function(pred, actual, model_name) {
  pos <- levels(actual)[2]
  cm  <- confusionMatrix(pred, actual, positive = pos)
  cat("\n──", model_name, "──\n")
  print(cm$overall[c("Accuracy", "Kappa")])
  print(cm$byClass[c("Sensitivity", "Specificity", "Precision", "F1")])
}

# ─────────────────────────────────────────────────────────────────
# SECTION 5: MODEL 1 – LOGISTIC REGRESSION
# ─────────────────────────────────────────────────────────────────

model_lr <- train(income ~ ., data = train_df,
                  method = "glm", family = "binomial",
                  trControl = cv_ctrl, metric = "ROC")
summary(model_lr)

pred_lr <- predict(model_lr, newdata = test_df)
print_metrics(pred_lr, y_test, "Logistic Regression")

# Coefficient plot
coef_df <- as.data.frame(coef(summary(model_lr$finalModel)))
coef_df$term <- rownames(coef_df)
coef_df <- coef_df[coef_df$term != "(Intercept)", ]
coef_df <- coef_df[order(abs(coef_df$Estimate), decreasing = TRUE)[1:15], ]

ggplot(coef_df, aes(x = reorder(term, Estimate), y = Estimate,
                    fill = Estimate > 0)) +
  geom_col() +
  coord_flip() +
  labs(title = "Figure 7 – Top 15 Logistic Regression Coefficients",
       x = "", y = "Coefficient") +
  scale_fill_manual(values = c("#E8734A", "#4C8BB5"),
                    labels = c("Negative", "Positive")) +
  theme_minimal()

# ─────────────────────────────────────────────────────────────────
# SECTION 6: MODEL 2 – RANDOM FOREST
# ─────────────────────────────────────────────────────────────────

model_rf <- randomForest(income ~ ., data = train_df,
                         ntree = 200, mtry = 4,
                         importance = TRUE, seed = 42)
print(model_rf)

pred_rf <- predict(model_rf, newdata = test_df)
print_metrics(pred_rf, y_test, "Random Forest")

# Feature importance plot
imp_df <- as.data.frame(importance(model_rf))
imp_df$feature <- rownames(imp_df)
imp_df <- imp_df[order(imp_df$MeanDecreaseGini, decreasing = TRUE)[1:15], ]

ggplot(imp_df, aes(x = reorder(feature, MeanDecreaseGini),
                   y = MeanDecreaseGini)) +
  geom_col(fill = "#4C8BB5") +
  coord_flip() +
  labs(title = "Figure 8 – Random Forest Feature Importances",
       x = "", y = "Mean Decrease Gini") +
  theme_minimal()

# ─────────────────────────────────────────────────────────────────
# SECTION 7: MODEL 3 – DECISION TREE
# ─────────────────────────────────────────────────────────────────

model_dt <- rpart(income ~ ., data = train_df,
                  method = "class",
                  control = rpart.control(maxdepth = 6, cp = 0.001))

rpart.plot(model_dt, main = "Figure 9 – Decision Tree", type = 4,
           extra = 104, box.palette = "BuOr")

pred_dt <- predict(model_dt, newdata = test_df, type = "class")
print_metrics(pred_dt, y_test, "Decision Tree")

# Prune using 1-SE rule
printcp(model_dt)
best_cp <- model_dt$cptable[which.min(model_dt$cptable[,"xerror"]), "CP"]
model_dt_pruned <- prune(model_dt, cp = best_cp)
pred_dt_pruned <- predict(model_dt_pruned, newdata = test_df, type = "class")
print_metrics(pred_dt_pruned, y_test, "Decision Tree (Pruned)")

# ─────────────────────────────────────────────────────────────────
# SECTION 8: MODEL 4 – K-MEANS CLUSTERING (Unsupervised)
# ─────────────────────────────────────────────────────────────────

# Elbow method
set.seed(59)
inertia <- sapply(1:8, function(k) {
  km <- kmeans(adult_scaled, centers = k, nstart = 10, iter.max = 50)
  km$tot.withinss
})

plot(1:8, inertia, type = "b", pch = 19, col = "#4C8BB5",
     main = "Figure 10 – K-Means Elbow Curve",
     xlab = "Number of Clusters (k)", ylab = "Total Within-Cluster SSE")

# Fit k = 2
km2 <- kmeans(adult_scaled, centers = 2, nstart = 25, iter.max = 100)

# Cross-tab clusters vs true labels
ct <- table(Cluster = km2$cluster, TrueIncome = y)
cat("Cluster vs True Income Cross-Tab:\n")
print(ct)

# Align cluster labels to majority class
cluster_map <- apply(ct, 1, which.max)
y_km <- ifelse(km2$cluster == names(cluster_map)[1],
               colnames(ct)[cluster_map[1]],
               colnames(ct)[cluster_map[2]])
km_acc <- mean(y_km == as.character(y))
cat("Cluster-aligned Accuracy:", round(km_acc, 4), "\n")

# PCA plot of clusters
pca_km <- prcomp(adult_scaled, center = FALSE, scale. = FALSE)
pca_df <- as.data.frame(pca_km$x[, 1:2])
pca_df$cluster <- as.factor(km2$cluster)
pca_df$income  <- y

ggplot(pca_df, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(alpha = 0.2, size = 0.8) +
  labs(title = "Figure 11 – K-Means Clusters on PC1 vs PC2") +
  theme_minimal()

# ─────────────────────────────────────────────────────────────────
# SECTION 9: DIMENSION REDUCTION – PCA
# ─────────────────────────────────────────────────────────────────

pca <- prcomp(X_train, center = FALSE, scale. = FALSE)

# Scree / cumulative variance plot
cum_var <- cumsum(pca$sdev^2 / sum(pca$sdev^2))
n_95    <- which(cum_var >= 0.95)[1]
cat("PCs needed for 95% variance:", n_95, "out of", ncol(X_train), "\n")

plot(cum_var, type = "b", pch = 19, col = "#4C8BB5",
     main = "Figure 12 – PCA Cumulative Explained Variance",
     xlab = "Number of Principal Components",
     ylab = "Cumulative Explained Variance")
abline(h = 0.95, lty = 2, col = "#E8734A")
legend("bottomright", legend = "95% threshold",
       lty = 2, col = "#E8734A", bty = "n")

# Project train/test onto PCA space
X_train_pca <- predict(pca, X_train)[, 1:n_95]
X_test_pca  <- predict(pca, X_test)[,  1:n_95]

train_pca_df <- as.data.frame(X_train_pca)
train_pca_df$income <- y_train
test_pca_df  <- as.data.frame(X_test_pca)
test_pca_df$income  <- y_test

# Logistic Regression on PCA features
model_lr_pca <- train(income ~ ., data = train_pca_df,
                      method = "glm", family = "binomial",
                      trControl = trainControl(method = "cv", number = 5))
pred_lr_pca  <- predict(model_lr_pca, newdata = test_pca_df)
acc_lr_pca   <- mean(pred_lr_pca == y_test)

# Random Forest on PCA features
model_rf_pca <- randomForest(income ~ ., data = train_pca_df,
                             ntree = 200, seed = 42)
pred_rf_pca  <- predict(model_rf_pca, newdata = test_pca_df)
acc_rf_pca   <- mean(pred_rf_pca == y_test)

acc_lr_full  <- mean(pred_lr == y_test)
acc_rf_full  <- mean(pred_rf == y_test)

cat("\nPCA Dimension Reduction Comparison:\n")
cat(sprintf("  Logistic Regression  – Full: %.4f  |  PCA (%d dims): %.4f\n",
            acc_lr_full, n_95, acc_lr_pca))
cat(sprintf("  Random Forest        – Full: %.4f  |  PCA (%d dims): %.4f\n",
            acc_rf_full, n_95, acc_rf_pca))

# ─────────────────────────────────────────────────────────────────
# SECTION 10: MODEL COMPARISON SUMMARY
# ─────────────────────────────────────────────────────────────────

pos_class <- levels(y_test)[2]

prob_lr <- predict(model_lr, newdata = test_df, type = "prob")[, pos_class]
prob_rf <- predict(model_rf, newdata = test_df, type = "prob")[, pos_class]
prob_dt <- predict(model_dt_pruned, newdata = test_df, type = "prob")[, pos_class]

library(pROC)
summary_df <- data.frame(
  Model    = c("Logistic Regression", "Random Forest", "Decision Tree"),
  Accuracy = c(mean(pred_lr == y_test), mean(pred_rf == y_test), mean(pred_dt_pruned == y_test)),
  ROC_AUC  = c(as.numeric(auc(roc(y_test, prob_lr, quiet = TRUE))),
               as.numeric(auc(roc(y_test, prob_rf, quiet = TRUE))),
               as.numeric(auc(roc(y_test, prob_dt, quiet = TRUE))))
)
print(summary_df)

# ROC curves
plot(roc(y_test, prob_lr, quiet = TRUE), col = "#4C8BB5", lwd = 2, main = "ROC Curves")
plot(roc(y_test, prob_rf, quiet = TRUE), col = "#E8734A", lwd = 2, add = TRUE)
plot(roc(y_test, prob_dt, quiet = TRUE), col = "#56A97F", lwd = 2, add = TRUE)
legend("bottomright",
       legend = sprintf(c("Logistic Reg (AUC=%.3f)", "Random Forest (AUC=%.3f)", "Decision Tree (AUC=%.3f)"),
                        summary_df$ROC_AUC),
       col = c("#4C8BB5", "#E8734A", "#56A97F"), lwd = 2, bty = "n")

# Confusion matrices
cat("\nLogistic Regression:\n"); print(confusionMatrix(pred_lr, y_test, positive = pos_class)$table)
cat("\nRandom Forest:\n");       print(confusionMatrix(pred_rf, y_test, positive = pos_class)$table)
cat("\nDecision Tree:\n");       print(confusionMatrix(pred_dt_pruned, y_test, positive = pos_class)$table)
