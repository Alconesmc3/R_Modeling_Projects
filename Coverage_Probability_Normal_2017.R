
### Simulation to study the probability of rejecting H0
### Adolfo Huerta | Date: 9/11/2017


n = 20
mu = 0
sigma = 6
alpha = .05

n.sim = 10000
t = p.value = rep( NA, n.sim)

for ( i in 1:n.sim) {
d = rnorm(n, mu, sigma)
dbar = mean(d)
s = sd(d)
se.hat = s / sqrt(n)
t[i] = dbar / se.hat
p.value[i] = 1 - pt( t[i], n - 1)

}

hist(t)
c = qt( 1 - alpha, n -1 )
abline(v=c , col = "red")
mean(t >= c)    ###Rejection probability

hist(p.value, xlim = c(0,1))
abline(v = alpha, col= "red")
mean( p.value <= alpha)
