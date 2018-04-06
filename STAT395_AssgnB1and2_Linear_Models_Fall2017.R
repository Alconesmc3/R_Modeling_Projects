library(readr)
cats <- read_csv("~/Desktop/ucsc/cats.csv")
View(cats)


attach(cats)
names(cats)
plot(bwt,hwt)
plot(factor(sex), hwt)
bwt.hwt.lm <- lm(hwt~bwt)
sex.hwt.lm <- lm(hwt~factor(sex))
abline(sex.hwt.lm)
summary(bwt.hwt.lm)
summary(sex.hwt.lm)
confint(bwt.hwt.lm, level = .95)
confint(sex.hwt.lm, level = .95)

(3.38 * 10^-7) / 2

.0845 / 2
1 - .04225

gamedata <- read.csv(file.choose())
head(gamedata)
data2 = subset( gamedata, att < 19136)
y = data2$ptdiff
x = data2$att
plot(x,y)
fitgame = lm(y~x)
summary(fitgame)
confint(fitgame, level = .95)
height <- c(68,72, 77, 63, 61, 82)
gpa.col <- c(2.2, 3.3, 3.6, 3.6, 2.9, 3.1)
studata <- data.frame(height, gpa.col)
head(studata)
ht.gpa.lm <- lm(gpa.col~height)
plot(height, gpa.col)
abline(ht.gpa.lm)
summary(ht.gpa.lm)

carsdata <- read.csv(file.choose())
attach(carsdata)
stop.spd.lm <- lm(dist~speed)
summary(stop.spd.lm)
plot(stop.spd.lm, 1)
plot(speed, dist)
y.hat = stop.spd.lm$fitted.values
r = stop.spd.lm$residuals
plot(y.hat, r)
abline( stop.spd.lm)
lines(smooth.spline( r ~ y.hat, df = 3), col="red")
plot(y)

sq.lm <- lm(sqrt(dist)~speed)
summary(sq.lm)
plot(speed, sqrt(dist))
plot(sq.lm, 1)
summary(sq.lm)
y2 = 1.27705 + 0.32241*25
y1 = -17.5791 + 3.9324*25
(.32241)^2


animals <- read.csv(file.choose())
names(animals)
head(animals)
attach(animals)
fit.bwt.bodwt <- lm( brain ~ body, weight = 1 / body^2)
plot(body, brain)
abline(fit.bwt.bodwt)
summary(fit.bwt.bodwt)
(1.01e-10) / 2
?confint
confint(fit.bwt.bodwt, level = 0.95)

fit.bwt.bodwt$
lower <- 8.166328 - 1.645 * 1.05491
upper <- 8.166328 + 1.645 * 1.05491
length(c(0,0,1,0,0,0,0,1,0,1,0,0,1,0,0,0))
mtrx <- matrix(c(0,0,1,0,0,0,0,1,0,1,0,0,1,0,0,0), nrow = 4, ncol = 4)
mtrx
mtrx ** mtrx
mtrx ** mtrx ** mtrx ** mtrx


sqrt(196)
