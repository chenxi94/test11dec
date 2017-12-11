wheel <- rep(c(rep("wheel1",1), rep("wheel2",1), rep("wheel3",1), rep("wheel4",1)),4)
car<- c(rep("car1",4), rep("car2",4), rep("car3",4), rep("car4",4))
seed <- c("C","B","A","D","D","C","B","A","A","D","C","B","B","A","D","C")
freq <- c(12,14,17,13,11,12,14,14,13,11,10,10,8,13,9,9)
mydata <- data.frame(car, wheel, seed, freq)
matrix(mydata$seed, 4,4)
myfit <- lm(freq ~seed+car+wheel, mydata)
myfit <- lm(freq ~ wheel+seed + wheel:seed, mydata)
anova(myfit)
fit <- aov(freq ~ wheel+car+seed,mydata)
TukeyHSD(fit,wheeled = T,conf.level = 0.95)
anova(myfit)



mi <- c(3,7,11,9,2,12,14,3,5,9,8,6,3,2,1,4,12,6,5,8)
yi <- c(5,1,23,14,6,28,24,45,6,23,14,13,7,5,1,6,28,15,11,12)*10
ybar <- sum(yi)/sum(mi)
mbar <- sum(mi)/n
sr<- sd(yi - ybar*mi)
N <- 96
n <- 20
var  = (N-n)/(N*n*mbar^2)*sr^2
t = sum(yi)*N/n
var_2 = var(yi)*20/19*96^2/20*(1-20/96)
t + 2*sqrt(var_2)
t - 2*sqrt(var_2)
