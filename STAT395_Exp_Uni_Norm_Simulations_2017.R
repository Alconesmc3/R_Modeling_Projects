
### SIMULATION FROM EXPONENTIAL MODEL
n = 100
theta = 60
sigma.exp = theta
sd.exp <- (sigma.exp)/(sqrt(n))
xbar.exp = rep( NA, 10000 )
for ( i in 1:10000 )
{
  x = rexp( n, 1/theta )
  xbar.exp[i] = mean(x)
}
hist( xbar.exp, xlim=c(0,60), breaks=seq(0,1000,1), main="...", xlab="..." )
mean.exp <- mean(xbar.exp)
se.exp <- sd(xbar.exp)
### SIMULATION FROM UNIFORM MODEL
theta1 = 0
theta2 = 120
mu <- (theta1 + theta2)/2
sigma.uni <- sqrt((theta2-theta1)^2/12)
sd.uni <- sigma.uni / sqrt(n)
xbar.unif = rep( NA, 10000 )
for ( i in 1:10000 )
{
  x = runif( n, theta1, theta2 )
  xbar.unif[i] = mean(x)
}
hist( xbar.unif, xlim=c(0,60), breaks=seq(0,1000,1), main="...", xlab="..." )
mean.unif <- mean(xbar.unif)
se.unif <- sd(xbar.unif)
### SIMULATION FROM NORMAL MODEL
mu = 30
sigma = 30
sd.norm <- sigma / sqrt(n)
xbar.norm = rep( NA, 10000 )
for ( i in 1:10000 )
{
  x = rnorm( n, mu, sigma )
  xbar.norm[i] = mean(x)
}
hist( xbar.norm, xlim=c(0,60), breaks=seq(0,1000,1), main="...", xlab="..." )
mean.norm <- mean(xbar.norm)
se.norm <- sd(xbar.norm)

