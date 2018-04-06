
#### MY CODE THAT I CREATED ####

# beers = c(5,2,9,8,3,7,3,5,3,5,4,6,5,7,1,4)
# BAC = c(.01, .03,.19,.12,.04,.095,.07,.06,.02,.05,.07,.1,.085,.09,.01,.05)
# 
# model <- lm(BAC~beers)
# anova(model)
# # plot(model)
# summary(model)
# ?abline
# x = 6
# y = -0.0172 + 0.017 * x
# 0.017731/ 0.003032
# 
# confint(model, level = .95)
# 
# shapiro.test(residuals(model))
# plot(model,5)

#################################
#8/31/17 R Code
#Notes 1 Regression Review
#Alana Unfried STAT 410
#################################


#This will clear the workspace environment
rm(list=ls())

#Given data
beers <- c(5,2,9,8,3,7,3,5,3,5,4,6,5,7,1,4)
bac <- c(0.010, 0.030, 0.190, 0.120, 0.040, 0.095, 0.070, 0.060, 0.020, 0.050, 0.070, 0.100, 0.085, 0.090, 0.010, 0.050)
data <- data.frame(beers, bac)
data

#basic scatterplot
#NOTE: R was the x variable first, then y!
plot(beers, bac)

#simple lineear regression model
model <- lm(bac~beers)

#plot with the regression line
plot(beers, bac)
abline(model)

#a fancier scatterplot- install the "car" package
library(car)
scatterplot(beers, bac, smoother=FALSE)

#ANOVA table for the simple linear regression
anova(model)

#t-tests and parameter estimates
summary(model)

#cycle through regression diagnostic plots
plot(model)

#Test for normality of residuals
shapiro.test(residuals(model))
#get just the QQ plot
plot(model,2)

#Residuals vs. Fitted plot
plot(model, 1)

#Cook's Distance
plot(model,5)

#Coefficient of Determination
#look at summary output again
summary(model)

####################################
#Confidence and Prediction Intervals
####################################

plot(beers, bac)
abline(model, col="lightblue")

newx <- seq(min(beers), max(beers), by=0.05)
conf_interval <- predict(model, newdata=data.frame(beers=newx), interval="confidence",
                         level = 0.95)
lines(newx, conf_interval[,2], col="blue", lty=2)
lines(newx, conf_interval[,3], col="blue", lty=2)

pred_interval <- predict(model, newdata=data.frame(beers=newx), interval="prediction",
                         level = 0.95)
lines(newx, pred_interval[,2], col="orange", lty=2)
lines(newx, pred_interval[,3], col="orange", lty=2)


####################################
#Correlation
####################################

#Test for significant (non-zero) correlation
cor.test(beers, bac, alternative="two.sided")

#calculate basic correlation
cor(beers, bac)

sqrt(.138)
sqrt(.787)
sqrt(.2153)
