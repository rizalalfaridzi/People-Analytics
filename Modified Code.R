# Assignment 2 - R code

# clear global environment
rm(list = ls())

# set and check working directory
setwd(choose.dir())
getwd()

# install packages (if required)
#install.packages("GGally", "ggpubr", "ggplot2", "corrplot", "corrgram",
#                 "QuantPsyc", "Hmisc", "psych")

# load packages in current session
library(GGally)
library(ggpubr)
library(ggplot2)
library(corrplot)
library(corrgram)
library(QuantPsyc)
library(Hmisc)
library(psych)

# import data
kenexa <- read.csv(paste("Kenexa.csv", sep = ""))
View(kenexa)

# look at the structure of the data
str(kenexa)

# check for missing values
sapply(kenexa, function(x) sum(is.na(x)))

# subset data to focus on variables included in logic model
data <- kenexa[, c("einvol", "etra", "ecomm", "eteam", "cbrpb", "cloy", "teltr", "prod")]

# descriptive statistics
Mean <- sapply(data, function(x) round(mean(x), 2))
Standard_Deviation <- sapply(data, function(x) round(sd(x), 2))
Minimum <- sapply(data, function(x) round(min(x), 2))
Maximum <- sapply(data, function(x) round(max(x), 2))

DescStats <- data.frame(Mean, Standard_Deviation, Minimum, Maximum)
DescStats

# correlations - plot created in Python for better aesthetics

# scatter plots
# strongest relationship
strong <- ggscatter(data, x = "etra", y = "ecomm", color = "blue", shape = 21,
                    add = "reg.line", add.params = list(color = "red", size = 0.75),
                    cor.coef = TRUE, cor.method = "pearson",
                    xlab = "Employee Training", ylab = "Employee Communication")
ggpar(strong, font.x = c(10), font.y = c(10), font.tickslab = c(10))

# weakest relationship
weak <- ggscatter(data, x = "ecomm", y = "cloy", color = "blue", shape = 21,
                  add = "reg.line", add.params = list(color = "red", size = 0.75),
                  cor.coef = TRUE, cor.method = "pearson",
                  xlab = "Employee Communication", ylab = "Customer Loyalty")
ggpar(weak, font.x = c(10), font.y = c(10), font.tickslab = c(10))

# 

# Last updated - 26/09/22

# change to character
#kenexa$bloc <- as.character(kenexa$bloc, levels = c(0,1), labels = c("non-metro","metro"))
#kenexa$ccon <- as.character(kenexa$ccon, levels = c(0,1), labels = c("less frequent","more frequent"))

#simple histogram for Employee Customer Orientation with normal curve overlay
# ggplot(kenexa, aes(ecuso)) +
#   geom_histogram(aes(y=..density..), binwidth = 0.1) + stat_function(fun=dnorm, args=list(mean = mean(kenexa$ecuso), sd =sd(kenexa$ecuso)), col="#1b98e0", size=1)

#simple histogram for Customer Loyalty with normal curve overlay
# ggplot(kenexa, aes(cloy)) +
#   geom_histogram(aes(y=..density..), binwidth = 0.1) + stat_function(fun=dnorm, args=list(mean = mean(kenexa$cloy), sd =sd(kenexa$cloy)), col="#1b98e0", size=1)

#simple histogram for Overall Branch Productivity with normal curve overlay
# ggplot(kenexa, aes(prod)) +
#   geom_histogram(aes(y=..density..), binwidth = 20) + stat_function(fun=dnorm, args=list(mean = mean(kenexa$prod), sd =sd(kenexa$prod)), col="#1b98e0", size=1)

#Correlation matrix for all numeric variables including p-values
# correlation.matrix<-rcorr(as.matrix(kenexa[,4:21]))
# correlation.matrix

#Fancy correlation plots
# corrplot<-corrplot(corr=cor(kenexa[ , c(4:18)], use="complete.obs"), 
#                           method ="ellipse")
# 
# 
# correlations.plot <- corrplot(corr=cor(kenexa[ , c(4:18)], use="complete.obs"), 
#                           method="number")

# display a pair plot for multiple columns of data
# GGally::ggpairs(kenexa[,5:15])

#Corrgram of all the variables

# corrgram(kenexa, order=TRUE,
#          main="Corrgram of all the Variables",
#          lower.panel=panel.shade, upper.panel=panel.pie,
#          diag.panel=panel.minmax, text.panel=panel.txt) 

#Scatterplots
# plot1a<-ggscatter(kenexa, x = "einvol", y = "eeng",
#           add = "reg.line", conf.int = TRUE,
#           cor.coef = TRUE, cor.method = "pearson",
#           xlab = "Employee Involvement", ylab = "Employee Engagement")
# plot1a
# 
# plot1b<-ggscatter(kenexa, x = "etra", y = "eeng",
#                  add = "reg.line", conf.int = FALSE,
#                  cor.coef = TRUE, cor.method = "pearson",
#                  xlab = "Employee Training", ylab = "Employee Engagement")
# plot1b
# 
# plot2a<-ggscatter(kenexa, x = "eteam", y = "cloy",
#                   add = "reg.line", conf.int = FALSE,
#                   cor.coef = TRUE, cor.method = "pearson",
#                   xlab = "Employee Teamwork", ylab = "Customer Loyalty")
# plot2a
# 
# plot2b<-ggscatter(kenexa, x = "ecomm", y = "cloy",
#                   add = "reg.line", conf.int = TRUE,
#                   cor.coef = TRUE, cor.method = "pearson",
#                   xlab = "Employee Communication", ylab = "Customer Loyalty")
# plot2b
# 
# plot3a<-ggscatter(kenexa, x = "cloy", y = "prod",
#                  add = "reg.line", conf.int = TRUE,
#                  cor.coef = TRUE, cor.method = "pearson",
#                  xlab = "Customer Loyalty", ylab = "Overall Branch Productivity")
# plot3a
# 
# plot3b<-ggscatter(kenexa, x = "cloy", y = "teltr",
#                  add = "reg.line", conf.int = FALSE,
#                  cor.coef = TRUE, cor.method = "pearson",
#                  xlab = "Customer Loyalty", ylab = "Teller Productivity")
# plot3b
# 
# plot4a<-ggscatter(kenexa, x = "etra", y = "prod",
#                   add = "reg.line", conf.int = FALSE,
#                   cor.coef = TRUE, cor.method = "pearson",
#                   xlab = "Employee Involvement", ylab = "Overall Productivity")
# plot4a

#Regression Predicting Customer Satisfaction-Personal Bankers with some Branch Factors {branch size) and some Employee Factors {quality, involvement, teamwork, engagement, communcation}
# regr1 <- lm(cbrpb ~ bloc + equal + einvol + eteam + eeng + ecomm, data = kenexa)
# summary(regr1)
# coef(regr1)
# regr1$coefficients
# residuals(regr1)
# lm.beta(regr1)

#Regression Predicting Customer Loyalty with some Employee Factors {quality, involvement, teamwork, engagement, communcation} and some Customer Factors {customer satisfaction}
# regr2 <- lm(cloy ~ equal + einvol + eteam + eeng + ecomm +cserq + cbrtel + cbr + cbrpb, data = kenexa)
# summary(regr2)
# coef(regr2)
# regr2$coefficients
# residuals(regr2)
# lm.beta(regr2)

#Regression Predicting Overall Branch Productivity with some Employee Factors {quality, involvement, teamwork, engagement, communcation} and some Customer Factors {customer satisfaction, customer loyalty}
# regr3 <- lm(prod ~ equal + einvol + eteam + eeng + ecomm + cserq + cbrtel + cbr + cbrpb + cloy, data = kenexa)
# summary(regr3)
# coef(regr3)
# regr3$coefficients
# residuals(regr3)
# lm.beta(regr3)

#Regression Predicting Teller Productivity with some Employee Factors {quality, involvement, teamwork, engagement, communcation} and some Customer Factors {customer satisfaction, customer loyalty}
# regr4 <- lm(teltr ~ equal + einvol + eteam + eeng + ecomm + cserq + cbrtel + cbr + cbrpb + cloy, data = kenexa)
# summary(regr4)
# coef(regr4)
# regr4$coefficients
# residuals(regr4)
# lm.beta(regr4)

#Regression Predicting Employee Engagement with some Branch Factors {branch size) and some Employee Factors {quality, teamwork, communcation} and some Customer Factors {customer loyalty}
# regr5 <- lm(eeng ~ bloc + equal + eteam + ecomm + cloy, data = kenexa)
# summary(regr5)
# coef(regr5)
# regr5$coefficients
# residuals(regr5)
# lm.beta(regr5)

#Now it's your turn. 
#You will need to.... 
        #adjust the R code above
        #write new R code
        #test other regression models
        #discern which output is important and which is not
        #test your logic model!

#All the best, Catherine :-) 