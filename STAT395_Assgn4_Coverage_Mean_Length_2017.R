# x = c(100,98,88, 82)
# mean(x)
# sd(x)
# t.test(x, mu = 70)

# n = 5
# mu = 35
# sigma = 3
# alpha = .1 ###CONFIDENCE LEVEL 1 - alpha = .9

# x = rnorm(n, mu, sigma)
# xbar = mean(x)
# s = sd(x)
# se.hat = s / sqrt(n)
# c = qt( 1 - alpha / 2, 4)
# ci.lower = xbar - c* se.hat
# ci.upper = xbar + c *se.hat
# c( ci.lower, ci.upper)

### Simulation for EXp to find coverage and mean length

# n = 1000
# theta = 40
# alpha = .05 ##CONFIDENCE LEVEL 1 - alpha = .9
# 
# ci.lower = ci.upper = rep( NA, 10000)
# 
# for (i in 1:10000){
#   x = rexp( n, 1/ theta)
#   xbar = mean(x)
#   s = sd(x)
#   se.hat = s / sqrt(n)
#   c = qt( 1 - alpha / 2, n - 1)
  # ci.lower[i] = xbar - c * se.hat
  # ci.upper[i] = xbar + c * se.hat
# }
# 
# mean( ci.lower <= theta & theta <= ci.upper)  #### COVERAGE PROBABILITY
# mean( ci.upper - ci.lower)  ### MEAN LENGTH

### SIMULATION FOR UNIFORM DISTRIBUTION TO GET COVERAGE AND MEAN LENGTH OF CI
# 
# n = 100
# theta1 = 0
# theta2 = 60
# mu = (theta1 + theta2) / 2
# mu
# alpha = .05 ##CONFIDENCE LEVEL 1 - alpha = .9
# 
# ci.lower = ci.upper = rep(NA, 10000)
# 
# for ( i in 1:10000 )
# {
#   x = runif( n, theta1, theta2 )
#   xbar = mean(x)
#   s = sd(x)
#   se.hat = s / sqrt(n)
#   c = qt( 1 - alpha / 2, n -1)
#   ci.lower[i] = xbar - c * se.hat
#   ci.upper[i] = xbar + c * se.hat
# }
# 
# mean( ci.lower <= mu & mu <= ci.upper)  #### COVERAGE PROBABILITY
# mean( ci.upper - ci.lower)  ### MEAN LENGTH

data = read.csv(file.choose())
View(data)
x = data$wgain
xbar = mean(x)
s = sd(x)
n = length(x)
se = s / sqrt(n)
c = qt(.995, n - 1)
xbar - c * se
xbar + c * se


