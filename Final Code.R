# Assignment 2 - R code

# clear global environment
rm(list = ls())

# set and check working directory
setwd(choose.dir())
getwd()

# install packages (if required)
# install.packages("ggpubr", "ggplot2", "psych")

# load packages in current session
library(ggpubr)
library(ggplot2)
library(psych)

# import data
kenexa <- read.csv(paste("Kenexa.csv", sep = ""))
View(kenexa)

# look at the structure of the data
str(kenexa)

# check for missing values
sapply(kenexa, function(x) sum(is.na(x)))

# sample statistics - charts created in Excel and Tableau for better aesthetics
describeBy(kenexa, group = 'bloc') # to get number of branches in each area

# subset data to focus on variables included in logic model
data <- kenexa[, c("einvol", "etra", "ecomm", "eteam", "cbrpb", "cloy", "teltr",
                   "prod")]

# descriptive statistics
Mean <- sapply(data, function(x) round(mean(x), 2))
Variation <- sapply(data, function(x) round(sd(x), 2))
Minimum <- sapply(data, function(x) round(min(x), 2))
Maximum <- sapply(data, function(x) round(max(x), 2))

DescStats <- data.frame(Mean, Variation, Minimum, Maximum)
DescStats

# correlations - plot created in Python for better aesthetics

# scatter plots
# strongest relationship
strong <- ggscatter(data, x = "etra", y = "ecomm", color = "blue", shape = 21,
                    add = "reg.line", add.params = list(color = "red", size = 0.75),
                    xlab = "Employee Training", ylab = "Employee Communication")
ggpar(strong, font.x = c(10), font.y = c(10), font.tickslab = c(10))

# 2nd strongest relationship
strong2 <- ggscatter(data, x = "einvol", y = "eteam", color = "blue", shape = 21,
                     add = "reg.line", add.params = list(color = "red", size = 0.75),
                     xlab = "Employee Involvement", ylab = "Employee Teamwork")
ggpar(strong2, font.x = c(10), font.y = c(10), font.tickslab = c(10))

# regression analysis
# regressing employee teamwork on employee involvement and employee training
model1 <- lm(eteam ~ einvol + etra, data = data)
summary(model1)
# assumptions
# 1. linearity - met (by looking at the correlations and scatter plot)
# 2. normality - met (almost bell-shaped curve)
hist(data$eteam)
# 3. zero residual mean
mean(model1$residuals)
# 4. homoscedasticity - met (looking at the normal q-q plot)
par(mfrow = c(2,2))
plot(model1)

# regressing employee communication on employee involvement and employee training
model2 <- lm(ecomm ~ einvol + etra, data = data)
summary(model2)
# assumptions
# 1. linearity - met (by looking at the correlations and scatter plot)
# 2. normality - met (almost bell-shaped curve)
par(mfrow = c(1,1))
hist(data$ecomm)
# 3. zero residual mean
mean(model2$residuals)
# 4. homoscedasticity - met (looking at the normal q-q plot)
par(mfrow = c(2,2))
plot(model2)

# looking at the correlation coefficient and the regression coefficient, no
# multicollinearity was found in the above 2 models

# regressing customer loyalty on employee communication and employee teamwork
model3 <- lm(cloy ~ ecomm + eteam, data = data)
summary(model3)

# regressing customer satisfaction with personal bankers on employee communication
# and employee teamwork
model4 <- lm(cbrpb ~ ecomm + eteam, data = data)
summary(model4)

# regressing overall productivity on employee communication and employee teamwork
model5 <- lm(prod ~ ecomm + eteam, data = data)
summary(model5)

# regressing teller productivity on employee communication and employee teamwork
model6 <- lm(teltr ~ ecomm + eteam, data = data)
summary(model6)

# regressing overall productivity on customer loyalty and customer satisfaction
# with personal bankers
model7 <- lm(prod ~ cloy + cbrpb, data = data)
summary(model7)

# regressing teller productivity on customer loyalty and customer satisfaction
# with personal bankers
model8 <- lm(teltr ~ cloy + cbrpb, data = data)
summary(model8)

# end
