pi11 <- 19/ 391
pi12 <- 72 / 409
pi13 <- 204/ 413
pi14 <- 338 / 417
pi15 <- 432 / 455
pi16 <- 205/ 208
beta.hat0 <- -2.247
beta.hat1 <- .209
y <- beta.hat0 + beta.hat1* 16
y

  
i = 5

odds.age12 <- pi12 / (1 - pi12)

odds.age16 / odds.age15


log(odds.age16)

age <- c(11,12,13, 14, 15, 16)
girls <- c(391, 409, 413, 417, 455, 208)

menar <- c(19, 72, 204, 338, 432, 205)

data <- data.frame(age, girls, menar)
View(data)
fit1 <- lm(menar/girls ~ age)
summary(fit1)
log.odds <- c(odds.age11, odds.age12, odds.age13, odds.age14,odds.age15,odds.age16)

library(readr)
oring <- read_csv("~/Desktop/ucsc/oring.csv")
attach(oring)
names(oring)

exp(1.454)
(log(.05/.95) - 15.0429) / -.2322

######################
### LOGISTIC MODEL ###
######################

### CI FOR THE PROBABILITY AT GIVEN EXPLANATORY VARIABLE

free.lunch2 = function( model, comb, conf.level ) {
  
  alpha = 1 - conf.level
  gamma.hat = summary(model)$coef[,1]
  estimate = as.numeric( t(comb) %*% gamma.hat )
  v = vcov(model)
  c = qnorm( 1 - alpha / 2 )
  se.hat = as.numeric( sqrt( t(comb) %*% v %*% comb ) )
  temp = estimate + c( 0, -1, 1 ) * c * se.hat
  temp2 = exp(temp) / ( 1 + exp(temp) )
  out = list( temp2[1], temp2[2:3] )
  names(out) = c( "estimate", "ci" )
  out
  
}

library(readr)
oring <- read_csv("~/Desktop/ucsc/oring.csv")
View(oring)
x = oring$temp
y = ifelse( oring$fail == "Y", 1,0 )
fit <- glm( y ~ x, family = "binomial")
summary(fit)
free.lunch2(fit, comb= c(1,77.46485), conf.level = .95)


library(readr)
urine <- read_csv("~/Desktop/ucsc/urine.csv")
View(urine)
 
fit1 <- glm( urine$coc ~ urine$calc , family = "binomial")
summary(fit1)
(4.33e-05) / 2
exp(.4686)
.4686 + 1.96 * .1146
exp(.243984)
exp(.693216)

num <- exp( -143.04 + 140.17 * 1.03)
num / (1 + num)

free.lunch2(fit1, comb = c(1, 10), conf.level = .95)
fit2 <- glm(urine$coc ~ urine$grav, family = "binomial")
summary(fit2)
.000622 / 2
140.17 + 1.96 * 40.97
exp(59.8688) / 100
exp(220.4712)
exp(140.17)
summary(urine$grav)
free.lunch2(fit2, comb = c(1, 1.03), conf.level = .95)
