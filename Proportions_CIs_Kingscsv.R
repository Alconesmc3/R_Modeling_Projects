
##### Code for assignment A5 - Proportions and CI of Kings.csv 




data = read.csv(file.choose())
View(data)

dim(data)
x = data$gender
table(x)

prop.boys <- 1291 / (1209 + 1291)
se.boys <- sqrt((prop.boys*(1 - prop.boys))/ 2500)
prop.boys - 1.96 * se.boys
prop.boys + 1.96 * se.boys
y = data$smoker
table(y)

prop.smoke = 175/ ( 175 + 2325)
se.smoke <- sqrt((prop.smoke * (1 - prop.smoke))/ 2500)
prop.smoke - 1.96 * se.smoke
prop.smoke + 1.96 * se.smoke

z = data$bwt
dim(z)
head(z)
z

table(z)

for (i in 1:2500){
  low.bwt = 0
  if (z[i] < 2500){
    low.bwt = low.bwt + 1
  }
    
}
low.bwt

low.bwt <- (z < 2500) == TRUE
low.bwt
table(low.bwt)
summary(z)

prop.low = 127 / ( 127 + 2373)
se.low = sqrt((prop.low * (1 - prop.low)) / 2500)
prop.low - 2.576 * se.low
prop.low + 2.576 * se.low
