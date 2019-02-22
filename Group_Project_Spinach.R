## Group Project: Spinach Data
## STAT 395: Special Topics
## 11/28/17


library(readr)
spinach <- read_csv("~/Desktop/ucsc/spinach.csv")
# View(spinach)
names(spinach)

#Research Question: Does density increase from different groups? 

#week 0 is 10/10/2017
#week 1 is 10/17/2017
#week 2 is 10/24/2017

#Attach dataset and check names
attach(spinach)
names(spinach)

#Install packages
# install.packages("lme4")
# install.packages("lmerTest")

# library(lme4)
# library(lmerTest)

#create model
fit1 <- lmer(yellow.prop ~ date + ( 1 + date | id))

install.packages( "lme4" ) ### INSTALL PACKAGE nlm4
install.packages( "lmerTest" ) ### INSTALL PACKAGE lmerTest
library(lme4) ### CALL LIBRARY FOR nlm4
library(lmerTest) ### CALL LIBRARY FOR lmerTest

#recreate new variables to get NA's
time <- date
eyed <- id
group1 <- ifelse(group == "C1" | group == "C2", 0,1)
group1 
dens <- density
spt <- spot
yell <- yellow
pst <- pest
spotp <- spt / dens
yellp <- yell / density
pestp <- pst / dens

#View variables to check 
group1
yellp
spotp
pestp

#Model for yellow proportion
fit1 <- lmer(yellp ~ time * group1 + ( 1 + time | eyed))
summary(fit1)
#assign variables to partial slopes
yellgamma0 <- summary(fit1)$coef[1,1]
yellgamma1 <- summary(fit1)$coef[2,1]
yellgamma2 <- summary(fit1)$coef[3,1]
yellgamma3 <- summary(fit1)$coef[4,1]

#plot time versus yellow proportion and add model lines
c.temp = ifelse( group1 == 0, "red", "blue" )
p.temp <- ifelse( group1 == 0, 0, 2 )
plot(time, yellp)
plot(time, yellp, col=c.temp, pch = p.temp, xlab = "Weeks",
     ylab = "Yellow Spot Proportion",main = "Control vs Treatment- Yellow Proportion")
abline(a = yellgamma0, b = yellgamma1, col = "red", lty =1 )
abline(a = yellgamma0 + yellgamma2 , yellgamma1 + yellgamma3 , col= "blue", lty = 2)
legend("topleft", col=c("red", "blue"), lty = c(1,2), legend = c("Control","Treatment"), bty = "n")
head(group1)

## Model for pesticide proportion  ##

fit2 <- lmer(pestp ~ time * group1 + ( 1 + time | eyed))
summary(fit2)

pestgamma0 <- summary(fit2)$coef[1,1]
pestgamma1 <- summary(fit2)$coef[2,1]
pestgamma2 <- summary(fit2)$coef[3,1]
pestgamma3 <- summary(fit2)$coef[4,1]

c.temp = ifelse( group1 == 0, "red", "blue" )
p.temp <- ifelse( group1 == 0, 0, 2 )
plot(time, pestp, col=c.temp, pch = p.temp, xlab = "Weeks",
     ylab = "Yellow Spot Proportion",main = "Control vs Treatment- Yellow Proportion")
abline(a = pestgamma0, b = pestgamma1, col = "red", lty =1 )
abline(a = pestgamma0 + pestgamma2 , pestgamma1 + pestgamma3 , col= "blue", lty = 2)
legend("topleft", col=c("red", "blue"), lty = c(1,2), legend = c("Control","Treatment"), bty = "n")
head(group1)



## Spinach Weight Data 
## Linear Model


library(readr)
weight <- read_csv("~/Desktop/ucsc/spinach_weight.csv")
View(spinach_weight)

attach(weight)
names(weight)

new_group = ifelse(group == 1 | group == 4 ,1,0)

new_group

#Create model for weight versus what group they are in
wt2grouplm <- lm(grams ~ new_group)
summary(wt2grouplm)
plot(new_group, grams,xlab= "groups(Control = 0, Treatment = 1)", ylab = "grams",
     main = "Grams for Each Group")
abline(wt2grouplm, col = "red")
summary(grams)

exp(-.12478)
exp(1.71434) + exp(.53232) /
  .4993 * (1 + exp(1.71434) + exp(.53232))
exp(0.37224)

1 / (1 + exp(1.71434 - .1278*(40)) + exp(.5323 - 0.01423*(40)) )
