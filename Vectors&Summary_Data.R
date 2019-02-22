id <- c(1,2,3,4,5)
x = c(80, 90,100, 120, 100)
y = c(115, 105, 95, 90, 105)
z = c("F", "M", "M", "F", "F")
data <- data.frame(cbind(id,z,x,y))
data
mean(z)
mean(x)
var(x)
sd(x)
plot(x, y, xlab = "ABC", ylab = "xyz", xlim = c(80, 120), ylim = c(50, 150))
cor(y, z)
apply( data[,3:4], 1, sum)

mean(z == "M")

data[,3:4]
var(z)



