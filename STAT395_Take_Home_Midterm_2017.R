## STAT395 - Take Home Midterm 
## November 27, 2017
## Linear Regression


# CI for Difference in Means
#Lecture A7 for Stat395

ci.mean.diff <- function( y1, y2, conf.level=0.95 )
{
  n1 = length(y1)
  n2 = length(y2)
  ybar1 = mean(y1)
  ybar2 = mean(y2)
  s1 = sd(y1)
  s2 = sd(y2)
  num.temp = (( n1 - 1 ) * s1^2) + (( n2 - 1 ) * s2^2)
  # den.temp = n1 + n2 - 2
  # s.pool = sqrt( num.temp / den.temp )
  # se.hat = s.pool * sqrt( (1 / n1) + (1 / n2))
  se.hat = sqrt( (s1^2) / n1 + (s2^2)/ n2)
  alpha = 1 - conf.level
  # c = qt( 1 - alpha / 2, n1 + n2 - 2 )
  c = qnorm( 1 - alpha / 2)
  L = ( ybar1 - ybar2 ) - c * se.hat
  U = ( ybar1 - ybar2 ) + c * se.hat
  out = list( L, U )
  names(out) = c( "ci.lower", "ci.upper" )
  out
}



sigma1 = 10
sigma2 = 50
mu1 = 100
mu2 = 100
n1 = 150
n2 = 150

n.sim = 10000
L = U = rep( NA, n.sim )

for ( i in 1:n.sim ){
  y1.sim = rnorm( n1, mu1, sigma1 )
  y2.sim = rnorm( n2, mu2, sigma2 )
  rslt = ci.mean.diff( y1=y1.sim, y2=y2.sim, conf.level = .95 )
  L[i] = rslt$ci.lower
  U[i] = rslt$ci.upper
}
mu.diff = mu1 - mu2
mean( L <= mu.diff & mu.diff <= U) ### COVERAGE PROBABILITY
mean( U - L)

library(readr)
stat100 <- read_csv("~/Desktop/ucsc/stat100.csv")
# View(stat100)

#Fit linear model for response points versus explanatory first.gen model
names(stat100)
attach(stat100)
point2firstlm <- lm(point ~ first.gen)
summary(point2firstlm)
c = qnorm(.975)
se.hat = .03925
ci.lower <- -.23210 - c * se.hat
ci.upper <- -.23210 + c * se.hat
confint(point2firstlm)
names(stat100)
point2gender <- lm(point ~ gender)
summary(point2gender)
point2low <- lm(point ~ low.income)
summary(point2low)
point2verb <- lm(point ~ sat.verbal)
summary(point2verb)
names(stat100)
point2math <- lm(point ~ sat.math)
summary(point2math)
point2writing <- lm(point ~ sat.writing)
summary(point2writing)

#full model
point2full <- lm( point ~ factor(first.gen) + factor(low.income) + factor(gender)  
                  + sat.verbal + sat.math + sat.writing )
summary(point2full)
gama.hat <- -.0357518
sefull.hat <- .0430022
full.lower <- gama.hat - c * sefull.hat
full.upper <- gama.hat + c * sefull.hat
confint(point2full)
y <- (0.1134422-.1378517-0.2584894-(0.0004606*500) + (0.0038539*500) 
      + (0.0015483*500))
c
y - c* sefull.hat
y + c* sefull.hat

free.lunch(point2full, c(1, 1,1,1,500,500,500), conf.level = .95)

free.lunch(point2full, c(1, 0,1,1,500,500,500), conf.level = .95)
free.lunch = function( model, comb, conf.level )
{
alpha = 1 - conf.level
gamma.hat = summary(model)$coef[,1]
estimate = t(comb) %*% gamma.hat
v = vcov(model)
c = qnorm( 1 - alpha / 2 )
se.hat = sqrt( t(comb) %*% v %*% comb )
ci = estimate + c( -1, 1 ) * c * se.hat
out = list( estimate, c, se.hat, ci )
names(out) = c( "estimate", "c", "se.hat", "ci" )
out
}

y1 <- ifelse( point >= 2, 1, 0)
x <- sat.math
fit.adj <- glm( y1 ~ x + low.income + gender + sat.verbal + sat.writing, family="binomial")
summary(fit.adj)
exp(0.0050650)
confint(fit.adj)
exp(0.0037487270)
exp(0.006392545)
new <- 100*0.0037487270
exp(new)
free.lunch(fit.adj, c(0,100,0,0,0,0), conf.level = .95)
exp(0.3743499)
exp(0.6386538)
free.lunch(fit.adj, c(1, 400,1,1,400,400), conf.level = .95)
free.lunch2(fit.adj, c(1, 400,1,1,400,400), conf.level = .95)

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

free.lunch2(fit.adj, c(1,500,1,1,400,400), conf.level = .95)
studa <- 1- .6978543
studb <- 1-.5819051
100* .00506
exp(.506)
(.6978543 / studa) / (.5819051 / studb)

#problem 4
dose <- c(0,1, 2.5, 5, 10,20,40)
case <- c(45, 30, 29, 30, 30, 29, 29)
rats <- c(8, 1, 3, 3, 4, 21, 24)

fit4 <- glm(rats/ case ~ dose,family = binomial(), weight = case)
summary(fit4)
exp(.11059)

