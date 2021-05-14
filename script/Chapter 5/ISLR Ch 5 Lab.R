library(ISLR)
head(Auto)

set.seed(1)
train <- sample(392,196)

#Creating linear regression model
lm.fit <- lm(mpg ~ horsepower, data = Auto, subset = train)

#Making predictions
pred <- predict(lm.fit, Auto[-train,])
mse <- mean((Auto[-train,]$mpg - pred) ** 2 )

#The way the book did it
attach(Auto)
mean((mpg -predict (lm.fit ,Auto))[-train ]^2) #26.14

"Both ways lead to the same value."

#Building polinomial regression model
lm.fit2 <- lm(mpg ~ poly(horsepower,2), data=  Auto, subset = train)
pred.2 <- predict(lm.fit2, Auto[-train,])
mse.2 <- mean((Auto[-train,]$mpg - pred.2) ** 2 )

mean((mpg -predict (lm.fit2 ,Auto ))[- train]^2) #18.71646

lm.fit3 <- lm(mpg ~ poly(horsepower,3), data=  Auto, subset = train)
pred.3 <- predict(lm.fit3, Auto[-train,])
mse.3 <- mean((Auto[-train,]$mpg - pred.3) ** 2 )

mean((mpg -predict (lm.fit3 ,Auto ))[- train]^2) #18.79401

"Both methods are the same. We can see that our MSE drops when we add the new
predictor horsepower^2, but it went up again when we add horsepower^3. We
should just use quadratic regression then."

#Building simple linear regression with the glm function

"We can do this by not specifying anything in the family parameter"

glm.fit <- glm(mpg ~ horsepower, data = Auto)
glm.fit$coefficients

lm.fit <- lm(mpg ~ horsepower, data = Auto)
lm.fit$coefficients

"Here we can see that the coefficients are exactly the same. But the glm model
can be used in the cv.glm function."

#Performing Cross Validation
library(boot)
cv.error <- cv.glm(Auto, glm.fit)
cv.error$delta

"The two numbers in the delta vector contain the cross-validation results. The
estimated test error is 24.23. The cv.glm works like the predict function.

The first is the standard k-fold CV estimate. The second is a bias-corrected
version."

#Performing Cross validation for multiple polynomial degrees
cv.error <- c()
for (i in 1:5) {
  glm.fit <- glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.error[i] <- cv.glm(data = Auto, glm.fit)$delta[1]
}

"We can see a substantial improvement in our MSE after adding the quadratic
terms but there is almost no effect in adding higher polynomial terms. We
should settle with the quadratic regression model"

#Performing K-fold CV
set.seed(1)

cv.error.10 <- c()
for (i in 1:10){
  glm.fit = glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.error.10[i] <- cv.glm(Auto, glm.fit, K = 10)$delta[1]
}

cv.error.10

"We can see that the default of k is the number of observation. Thus if K is
not specified, the algorithm will perform LOOCV. We are doing 10-fold CV. The
result is not indentical but it is very similar. This fact demostrates the
redundantness of LOOCV."

#Performing Bootstrap
"To use the Bootstrap, we have to first define a function that we are interested
in."

alpha.fn = function(data, index){
  X = data$X[index]
  Y = data$Y[index]
  return ((var(Y)-cov(X,Y))/(var(X)+var(Y) -2*cov(X,Y)))
}

alpha.fn(Portfolio ,1:100)
alpha.fn(Portfolio, sample(100, 100, replace = TRUE))

"We can implement a bootstrap analysis by performing this command many
times, recording all of the corresponding estimates for α, and computing
the resulting standard deviation. However, the boot() function automates
this approach"

boot(data = Portfolio, alpha.fn, R = 1000)

"We can see that the alpha and SE of alpha is automatically calculated. They
are 0.57 and 0.09 respectively."

#Estimating the Accuracy of a Linear Regression Model
boot.fn=function (data ,index){
  return(coef(lm(mpg ~ horsepower , data = data, subset = index)))
}

boot.fn(Auto ,1:392)

"The boot.fn() function can also be used in order to create bootstrap estimates
for the intercept and slope terms by randomly sampling from among the
observations with replacement."

set.seed(1)
boot.fn(Auto ,sample (392,392, replace=T))
boot.fn(Auto ,sample (392,392, replace=T))

boot(Auto ,boot.fn ,1000)

summary(lm(mpg ~ horsepower, data = Auto))$coefficient

"We can see that the coef estimate do not differ. The standard error does."

#Performing Bootstrap on the quadratic model
boot.fn=function (data ,index){
  return(coef(lm(mpg ~ poly(horsepower,2) , data = data, subset = index)))
}

set.seed(1)
boot.fn(Auto ,sample (392,392, replace=T)) #For example
boot(Auto, boot.fn, R = 1000)

summary(lm(mpg ~ poly(horsepower, 2), data = Auto))$coefficient

"Again the coef estimates are exactly the same. The SE are different. The boot
strap method are of course better than the estimate from linear regression
since unlike linear regression, bootstrap does not rely on sigma**2 when
estimating SE. "