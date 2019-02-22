### Adolfo Huerta
### Assignment B3 for STAT395
### 10/11/17

data = read.csv( file.choose() ) ### READ cats.csv
head(data)
y = data$hwt ### RESPONSE
x = ifelse( data$sex == "F", 0, 1 ) ### EXPLANATORY (DEFINE THE BINARY CODE)
z = data$bwt ### POTENTIAL THIRD VARIABLE
plot( x, y )
cor( x, y )
fit1 = lm( y~x )
summary(fit1)$coef
confint(fit1, level = .95)
abline( fit1, col="red", lty=2 )
plot( x, y )

data.num = data.frame( sex=x, hwt=y, bwt=z )
plot(data.num)
cor(data.num)
fit2 <- lm( y~x + z )
summary(fit2)$coef
confint(fit2, level = .95)
sd(x)
sd(z)

(0.52396* ( sd(z)/ sd(x)) )* 4.07576 -.08209684


heights <- read.csv(file.choose())
attach(heights)
names(heights)
head(heights)
data.num1 <- data.frame(father, child, gender)
cor(data.num1)
fit3 <- lm(child ~ father )
summary(fit3)
c = qt(.975, nrow(heights) - 2)
0.348 + c * 0.226
confint(fit3, level = .95)
fit4 <- lm(child ~ father + gender)
summary(fit4)
confint(fit4, levle = .95)
