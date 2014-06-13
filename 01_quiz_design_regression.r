
# Quizz #1 coursera on Regression

# https://class.coursera.org/regmods-002/quiz/attempt?quiz_id=101

setwd("C:/Users/pedroc/Desktop/courseras 2014/coursera regression models")
library(UsingR)

hist(galton$child, breaks = 100)
hist(galton$parent, breaks = 100)

head(galton)

plot(galton$parent, galton$child)

# 1

# Consider the data set given below

x <- c(0.18, -1.54, 0.42, 0.95)

# And weights given by

w <- c(2, 1, 3, 1)

# Give the value of ?? that minimizes the least squares equation ???ni=1wi(xi?????)2

lm(w ~ x + I(x^2))

lm(w ~ 0+x)

lm(w ~ x)

library(manipulate)
myHist <- function(mu){
  hist(galton$child,col="blue",breaks=100)
  lines(c(mu, mu), c(0, 150),col="red",lwd=5)
  mse <- mean((galton$child - mu)^2)
  text(63, 150, paste("mu = ", mu))
  text(63, 140, paste("MSE = ", round(mse, 2)))
}
manipulate(myHist(mu), mu = slider(62, 74, step = 0.5))

mean(galton$child) # is the optimal which minimizes MSE 

# Haciendo la cuenta de la vieja

mu <- c(0.1471, 1.077, 0.3, 0.0025)

for (i in 1:length(w)){
  mse <- sum(w[i]*(x[i] - mu[i])^2)
  print(mse)
}


# 2 


#Consider the following data set

x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)

# Fit the regression through the origin and get the slope treating y 
# as the outcome and x as the regressor. 
# (Hint, do not center the data since we want regression through the origin, 
# not through the means of the data.)

plot(y ~ x)

lm(y ~ 0 + x)


# 3

# Do data(mtcars) from the datasets package and fit the regression model 
# with mpg as the outcome and weight as the predictor. Give the slope coefficient.

data(mtcars)

lm(mtcars$mpg ~ mtcars$wt)

#-5.344



# Question 4

# Consider data with an outcome (Y) and a predictor (X). 
# The standard deviation of the predictor is one half that of the outcome. 
# The correlation between the two variables is .5. 
# What value would the slope coefficient for the regression model with Y as the outcome and X as the predictor?

#rxy = .5
#sx/sy = .5

# b = rxy * sx/sy

b = .5 * .5
print(b)



# Question 6

# Consider the data given by the following

x <- c(8.58, 10.46, 9.01, 9.64, 8.86)

# What is the value of the first measurement if x were normalized (to have mean 0 and variance 1)?

xnorm <- (x - mean(x)) / sd(x)

print(xnorm)


#Question 7

#Consider the following data set (used above as well). 
#What is the intercept for fitting the model with x as the predictor and y as the outcome?

x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)

lm(y ~x)

#Question 9

# Consider the data given by

w <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)

# What value minimizes the sum of the squared distances between these points and itself?


mu <- c(0.573, 0.8, 0.36, 0.44)

for (i in 1:length(mu)){
  mse <- sum((x - mu[i])^2)
  print(mse)
}

