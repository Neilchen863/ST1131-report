setwd("/Users/chenjiahao/Desktop/ST1131")
car = read.csv("auto_mpg_cleaned.csv")
attach(car)
summary(mpg) #get the statistic summary of the response variable

#get the boxplot and histogram of mpg
boxplot(mpg, main = "boxplot of Fuel Consumption", ylab = "consumption(mpg)")
hist(mpg, xlab = "consumption(mpg)")

#find the summary of all other variables
summary(car)

#draw the boxplot of mpg and origin, year, model year and No. of cylinders
boxplot(mpg~cylinders, main = "Consumption and No. of Cylinders")
boxplot(mpg~origin, main = "Consumption and Origin")
boxplot(mpg~model.year, main = "Consumption and Model Year")
boxplot(mpg~year, main = "Consumption and Year")


#draw the scatter plot of mpg and acceleration, displacement, horsepower and weight
plot(mpg,acceleration, main = "Consumption and Acceleration")
plot(mpg,displacement, main = "Consumption and Displacement")
plot(mpg,horsepower, main = "Consumption and Horsepower")
plot(mpg,weight, main = "Consumption and Weight")

#check the correlation between mpg and other variables
cor(mpg, origin)
cor(mpg, cylinders)
cor(mpg, displacement)
cor(mpg, horsepower)
cor(mpg, weight)
cor(mpg, accleration)
cor(mpg, year)

#Conduct hypothesis testing to each variable
M = lm(mpg~ cylinders,data = car)
summary(M)
M = lm(mpg~ model.year,data = car)
summary(M)
M = lm(mpg~ origin,data = car)
summary(M)
M = lm(mpg~ year,data = car)
summary(M)
M = lm(mpg~ acceleration,data = car)
summary(M)
M = lm(mpg~ displacement,data = car)
summary(M)
M = lm(mpg~ horsepower,data = car)
summary(M)
M = lm(mpg~ weight,data = car)
summary(M)


#M1
M1 = lm(mpg ~ displacement + weight + origin, data = car)
summary(M1)
SR1 = rstandard(M1)
plot(M1$fitted.values, SR1, xlab = 'Predicted Response', main = "residual plot"); abline(h=0, col = "red")
qqnorm(SR1); qqline(SR1, col = 'red')
hist(SR1, prob = TRUE)
which(SR1 > 3 | SR1 < -3)
Cook = cooks.distance(M1); which(Cook > 1)


#M2
M2 = lm(mpg ~ displacement + weight, data = car)
summary(M2)
SR2 = rstandard(M2)
plot(M2$fitted.values, SR2, xlab = 'Predicted Response', main = "residual plot"); abline(h=0, col = "red")
qqnorm(SR2); qqline(SR2, col = 'red')
hist(SR2, prob = TRUE)
which(SR2 > 3 | SR2 < -3)
Cook = cooks.distance(M2); which(Cook > 1)


#M3
M3 = lm(mpg ~ weight + horsepower, data = car)
summary(M3)
SR3 = rstandard(M3)
plot(M3$fitted.values, SR3, xlab = 'Predicted Response', main = "residual plot"); abline(h=0, col = "red")
qqnorm(SR3); qqline(SR3, col = 'red')
hist(SR3, prob = TRUE)
which(SR3 > 3 | SR3 < -3)
Cook = cooks.distance(M3); which(Cook > 1)



#M4
M4 = lm(log(mpg) ~ weight + horsepower, data = car)
summary(M4)
SR4 = rstandard(M4)
plot(M4$fitted.values, SR4, xlab = 'Predicted Response', main = "residual plot"); abline(h=0, col = "red")
qqnorm(SR4); qqline(SR4, col = 'red')
hist(SR4, prob = TRUE)
which(SR4 > 3 | SR4 < -3)
Cook = cooks.distance(M4); which(Cook > 1)





