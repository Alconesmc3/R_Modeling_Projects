### STAT395 Double Loop for Assignment4 (Coverage mean and CI length)


n = c( 5, 10, 20, 30, 50, 100, 200 )
n.loop = length(n)
theta = 40
alpha = .05 ### CONFIDENCE LEVEL 1 - alpha

coverage = mean.length = rep( NA, n.loop )

for ( j in 1:n.loop ) {
  
  ci.lower = ci.upper = rep( NA, 10000 )
  
  for ( i in 1:10000 ) {
    
    x = rexp( n[j], 1/theta )
    xbar = mean(x)
    s = sd(x)
    se.hat = s / sqrt( n[j] )
    c = qt( 1 - alpha / 2, n[j] - 1 )
    ci.lower[i] = xbar - c * se.hat
    ci.upper[i] = xbar + c * se.hat
    
  }
  
  coverage[j] = mean( ci.lower <= theta & theta <= ci.upper ) ### COVERAGE
  mean.length[j] = mean( ci.upper - ci.lower ) ### MEAN LENGTH
  
}

coverage
mean.length
