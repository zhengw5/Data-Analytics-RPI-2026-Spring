library(readr)
library(EnvStats)
library(nortest)

# set working directory (relative path)
setwd("C:/Users/willi/Documents/Data Analysis/lab/lab1")

# read data
epi.data <- read_csv("epi_results_2024_pop_gdp.csv")

# view dataframe
View(epi.data)
names(epi.data)
# print vlues in variable
epi.data$EPI.new

# print summary of variable
summary(epi.data$EPI.new)


### Explore Variable ###

## take a copy of a variable from a dataframe into a separate variable
EPI <- epi.data$EPI.new

# find NAs in variable: function outputs vector of logical values, true if NA, false otherwise
NAs <- is.na(EPI)

# print
NAs

# function "which" returns row numbers of rows with NAs
rownums <- which(NAs)

# print rows with NAs
EPI[rownums]

## create copy of new variable

MHP <- epi.data$MHP.new

# print values in variable
MHP

# find NAs inv variavle - outputs vector of logical values, true if NA, false otherwise
NAs <- is.na(MHP)

rownums <- which(NAs)

# print NAs
MHP[rownums]

# take subset of NOT NAs from variable
MHP.complete <- MHP[!NAs]

MHP.complete

# filter for only values above 30 and assign result to new variable
MHP.above30 <- MHP.complete[MHP.complete>30]

MHP.above30
  
# stats
summary(MHP.above30)

# boxplot of variable(s)
boxplot(EPI, MHP.above30, names = c("EPI","MHP>30"))


### Histograms ###

# histogram (frequency distribution)
hist(EPI)

# define sequence of values over which to plot histogram
x <- seq(20., 80., 5)
  
# histogram (frequency distribution) over specified range
hist(EPI, x, prob=TRUE)

# print estimated density curve for variable
lines(density(EPI,bw="SJ")) # or try bw=“SJ”

# print rug under histogram
rug(EPI)


## plot  histogram again

# histogram (frequency distribution) over range
hist(EPI, x, prob=TRUE) 

# range of values
x1<-seq(20,80,1)

# generate probability density values for a normal distribution with given mean and sd
d1 <- dnorm(x1,mean=46, sd=11,log=FALSE)

# print density values
lines(x1,d1)


### Empirical Cumulative Distribution Function ###

# plot ecdfs
plot(ecdf(EPI), do.points=FALSE, verticals=TRUE) 

plot(ecdf(MHP), do.points=FALSE, verticals=TRUE) 


### Quantile-quantile Plots ###

# print quantile-quantile plot for variable with theoretical normal distribuion
qqnorm(EPI); qqline(EPI)


# print quantile-quantile plot for random numbers from a normal distribution with theoretical normal distribution

# rnorm generates random values from a normal distribution with given mean and sd
x <- rnorm(180, mean=46, sd=10)

qqnorm(x); qqline(x)


# print quantile-quantile plot of two variables

qqplot(EPI, MHP, xlab = "Q-Q plot for EPI & MHP") 

# print quantile-quantile plot for 2 variables
qqplot(epi.data$EPI.new, epi.data$ECO.new, xlab = "Q-Q plot for EPI.new & EPI.old") 


## Statistical Tests

x <- rnorm(500)
y <- rnorm(500)

hist(x)
hist(y)

shapiro.test(x)
shapiro.test(y)

ad.test(x)
ad.test(y)

ks.test(x,y)

wilcox.test(x,y)

var.test(x,y)
t.test(x,y)

hist(x, col='lightsteelblue')
hist(y, col='lightgreen', add=TRUE)


### THE END ###

# ----------------Exercise 2 for Lab 1-----------------

v1_name <- "ECO.new"
v2_name <- "AIR.new"

var1 <- epi.data[[v1_name]]
var2 <- epi.data[[v2_name]]

summary(var1)
summary(var2)

boxplot(var1, var2, names=c(v1_name, v2_name), main="Boxplots")

hist(var1, prob=TRUE, main=paste("Histogram:", v1_name), xlab=v1_name)
x1 <- seq(min(var1), max(var1))
lines(x1, dnorm(x1, mean=mean(var1), sd=sd(var1)))

hist(var2, prob=TRUE, main=paste("Histogram:", v2_name), xlab=v2_name)
x2 <- seq(min(var2), max(var2))
lines(x2, dnorm(x2, mean=mean(var2), sd=sd(var2)))

plot(ecdf(var1), do.points=FALSE, verticals=TRUE) 
plot(ecdf(var2), do.points=FALSE, verticals=TRUE) 

qqnorm(var1); qqline(var1)
qqnorm(var2); qqline(var2)

qqplot(var1, var2, main = "Q-Q plot for ECO & AIR") 
print("Normality tests var1")
print(shapiro.test(var1))
print(ad.test(var1))
print("Normality tests var2")
print(shapiro.test(var2))
print(ad.test(var2))

print("Tests whether variables have identical distribution")
ks.test(var1,var2)

wilcox.test(var1,var2)

var.test(var1,var2)
t.test(var1,var2)

hist(var1, col='lightsteelblue')
hist(var2, col='lightgreen', add=TRUE)
