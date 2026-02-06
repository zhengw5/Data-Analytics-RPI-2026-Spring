library("ggplot2")
library("readr")

setwd("C:/Users/willi/Documents/Data Analysis/lab/lab2/Lab 2")

## read dataset
NY_House_Dataset <- read_csv("NY-House-Dataset.csv")

dataset <- NY_House_Dataset


ggplot(dataset, aes(x = PROPERTYSQFT, y = PRICE)) +
  geom_point()

ggplot(dataset, aes(x = log10(PROPERTYSQFT), y = log10(PRICE))) +
  geom_point()


## filter data
dataset <- dataset[dataset$PRICE<195000000,]

## column names
names(dataset)

## fit linear model
lmod0 <- lm(PRICE~PROPERTYSQFT, data = dataset)

## print model output
summary(lmod0)

## scatter plot of 2 variables
plot(PRICE~PROPERTYSQFT, data = dataset)
abline(lmod0)

## better scatter plot of 2 variables with best fit line
ggplot(dataset, aes(x = PROPERTYSQFT, y = PRICE)) +
  geom_point() +
  stat_smooth(method = "lm", col="red")

lmod1 <- lm(log10(PRICE)~log10(PROPERTYSQFT), data = dataset)

## print model output
summary(lmod1)

## scatter plot of 2 variables with best fit line
plot(log10(PRICE)~log10(PROPERTYSQFT), data = dataset)
abline(lmod1)

## better scatter plot of 2 variables with best fit line

ggplot(dataset, aes(x = log10(PROPERTYSQFT), y = log10(PRICE))) +
  geom_point() +
  stat_smooth(method = "lm", col="red")


ggplot(lmod1, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0)


## filter data

dataset <- dataset[dataset$PROPERTYSQFT!=2184.207862,]


lmod2 <- lm(log10(PRICE)~log10(PROPERTYSQFT), data = dataset)

## print model output
summary(lmod2)

## scatter plot of 2 variables with best fit line
plot(log10(PRICE)~log10(PROPERTYSQFT), data = dataset)
abline(lmod2)

## better scatter plot of 2 variables with best fit line
ggplot(dataset, aes(x = log10(PROPERTYSQFT), y = log10(PRICE))) +
  geom_point() +
  stat_smooth(method = "lm", col="red")



### THE END ###

#----------------------------------------------------------------------------------
dataset <- NY_House_Dataset
dataset <- dataset[dataset$PRICE<195000000,]

#-------------BEDS--------------


# Try more filtering
dataset <- dataset[dataset$BEDS<=30,]


# Try normalizing Beds
BEDS_lmod2 <- lm(log10(PRICE)~log10(BEDS), data = dataset)


## scatter plot of 2 variables with best fit line
plot(log10(PRICE)~log10(BEDS), data = dataset)
abline(BEDS_lmod2)

## better scatter plot of 2 variables with best fit line

ggplot(dataset, aes(x = log10(BEDS), y = log10(PRICE))) +
  geom_point() +
  stat_smooth(method = "lm", col="red")

res_df <- data.frame(
  fitted = fitted(BEDS_lmod2),
  residuals = resid(BEDS_lmod2)
)

ggplot(res_df, aes(x = fitted, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "Residual Plot for Beds Model",
       x = "Fitted Values",
       y = "Residuals")

#-------------------Bath-------------------
dataset <- NY_House_Dataset
dataset <- dataset[dataset$PRICE<195000000,]

# Try normalizing Beds---------Model 3
dataset_less_bath <- dataset[dataset$BATH>0,]
BATH_lmod2 <- lm(log10(PRICE)~log10(BATH), data = dataset_less_bath)


## scatter plot of 2 variables with best fit line
plot(log10(PRICE)~log10(BATH), data = dataset_less_bath)
abline(BATH_lmod2)

## better scatter plot of 2 variables with best fit line

ggplot(dataset_less_bath, aes(x = log10(BATH), y = log10(PRICE))) +
  geom_point() +
  stat_smooth(method = "lm", col="red")

res_df <- data.frame(
  fitted = fitted(BATH_lmod2),
  residuals = resid(BATH_lmod2)
)

ggplot(res_df, aes(x = fitted, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "Residual Plot for Bath Model",
       x = "Fitted Values",
       y = "Residuals")


## print PROPERTY model output
summary(lmod2)

## print BEDS model output
summary(BEDS_lmod2)
## print Bath model output
summary(BATH_lmod2)


#----------------Most significant variable vs price--------------

# PropertySQFT is the most significant because its R^2 value is the largest out of the three models
dataset <- NY_House_Dataset
dataset <- dataset[dataset$PRICE<195000000,]
dataset <- dataset[dataset$PROPERTYSQFT!=2184.207862,]


lmod2 <- lm(log10(PRICE)~log10(PROPERTYSQFT), data = dataset)

## print model output
summary(lmod2)

## scatter plot of 2 variables with best fit line
plot(log10(PRICE)~log10(PROPERTYSQFT), data = dataset)
abline(lmod2)

## better scatter plot of 2 variables with best fit line
ggplot(dataset, aes(x = log10(PROPERTYSQFT), y = log10(PRICE))) +
  geom_point() +
  stat_smooth(method = "lm", col="red")

res_df <- data.frame(
  fitted = fitted(lmod2),
  residuals = resid(lmod2)
)

ggplot(res_df, aes(x = fitted, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "Residual Plot for Property Model",
       x = "Fitted Values",
       y = "Residuals")
