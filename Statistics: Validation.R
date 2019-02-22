###########################
#STAT 410 Notes 4a Validation
#Reference ISLR book section 5.1.1
###########################

#install this package first (R package for Introduction to Statistical Learning Book)
library(ISLR)

#We will use the Auto dataset
attach(Auto)
head(Auto)
names(Auto)
#use ?Auto to read about the dataset
?Auto
#392 total observations
n <- nrow(Auto)

############
#Validation
############

#we want to predict mpg using horsepower

plot(horsepower, mpg)

#sample (without replacement) half of the rows 
#this code says: out of n numbers, pick half of them
set.seed(1)
train<-sample(n,n/2)
#view all the row numbers you have selected
train
sort(train)

#here is a plot using only our training data; this is what we are fitting the model to
plot(horsepower[train], mpg[train])

#subset option in lm will only use the rows of data you specify to fit the model
#fit the model ONLY using the training data
lm.fit<-lm(mpg~horsepower,data=Auto,subset=train)
summary(lm.fit)

#predict function will calculate fitted y value (mpg) for each response
#then we want to average the squared residuals JUST from the training set
  #this is an estimate of test MSE!
  #note that really we have just calculated the numerator of MSE, but this is good enough
mean((mpg-predict(lm.fit,Auto))[-train]^2)
?poly
#now we do the same thing with a quadratic model
lm.fit2<-lm(mpg~poly(horsepower,2),data=Auto,subset=train)
summary(lm.fit2)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)

#and the same thing with a cubic model
lm.fit3<-lm(mpg~poly(horsepower,3),data=Auto,subset=train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)

#but- what if we change the seed? our training and testing datasets are split up differently now
set.seed(2)
train<-sample(n,n/2)
sort(train)
lm.fit<-lm(mpg~horsepower,subset=train)
mean((mpg-predict(lm.fit,Auto))[-train]^2)
lm.fit2<-lm(mpg~poly(horsepower,2),data=Auto,subset=train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)
lm.fit3<-lm(mpg~poly(horsepower,3),data=Auto,subset=train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)

