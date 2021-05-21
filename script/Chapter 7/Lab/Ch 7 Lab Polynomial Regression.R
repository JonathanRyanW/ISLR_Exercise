library(ISLR)
names(Wage)
dim(Wage)

"We will try to predict the response wage with various predictors."

#Building 4-degree polynomial model of age to predict wage
wage_age_poly4 <- lm(wage ~ poly(age, 4), data = Wage)
summary(wage_age_poly4)

"We can see that all the components have <0.05 p-value except for x**4. We
should not use x**4.

The function poly() returns a matrix whose columns are a basis of orthogonal
polynomials, which means that each column is a linear orthogonal combination
of the variables age, age^2, age^3 and age^4.

If we want to get the actual values of age, age^2, age^3 and age^4 then we
should use the argument raw=TRUE"

wage_age_poly4 <- lm(wage ~ poly(age, 4, raw = TRUE), data = Wage)
summary(wage_age_poly4)

"We can see that the coefficient estimates are different. However this will not
affect the fitted values."

#Grabbing the first 6 fitted values
head(lm(wage ~ poly(age, 4, raw = TRUE), data = Wage)$fitted.values)
head(lm(wage ~ poly(age, 4), data = Wage)$fitted.values)

"We can see that the fitted values are exactly the same. As such the MSE will
also be exactly the same."

#Other equivalent ways of fitting this model
fit2a=lm(wage~age+I(age^2)+I(age^3)+I(age^4),data=Wage)
coef(fit2a)

# This simply creates the polynomial basis functions on the fly, taking care
# to protect terms like age^2 via the wrapper function I()
# (the ^ symbol has a special meaning in formulas).

fit2b=lm(wage~cbind(age ,age^2,age^3, age ^4),data=Wage)
coef(fit2b)

"We can see that the wrapper I(), cbind(), and poly() functions return the
same coefficient estimate. Any function call such as cbind() inside a formula
also serves as a wrapper."

rm(fit2a, fit2b)

# We now create a grid of values for age at which we want predictions, and
# then call the generic predict() function, specifying that we want standard
# errors as well

agelims = range(Wage$age)
age.grid = seq(from = agelims[1], to = agelims[2])

predictions = predict(wage_age_poly4,
                      newdata = list(age = age.grid),
                      se=TRUE)

#Looking at the results
names(predictions)
head(predictions$fit)
head(predictions$se.fit)

#Creating 95% Confidence Interval for every observation
se.bands = cbind(predictions$fit + 2*predictions$se.fit,
                 predictions$fit - 2* predictions$se.fit)

#Plotting the results
library(ggplot2)

ggplot(data = Wage, aes(x = age, y = wage)) +
  geom_point(color = "grey", shape = 1) +
  geom_smooth(formula = y ~ poly(x,4), se = TRUE, fill = "red") +
  theme_bw()
  
#The way the book create the plot
par(mfrow = c(1,2), mar = c(4.5,4.5,1,1), oma=c(0,0,4,0))
plot(Wage$age ,Wage$wage ,xlim=agelims ,cex =.5,col=" darkgrey ")
title(" Degree -4 Polynomial ",outer=T)
lines(age.grid ,predictions$fit ,lwd=2,col="blue")
matlines (age.grid ,se.bands ,lwd=1, col=" blue",lty=3)

#Just cleaning up
rm(predictions, se.bands)

#Choosing the right degree of polynomial using ANOVA
wage_age_poly <- lm(wage ~ age, data = Wage)
wage_age_poly2 <- lm(wage ~ poly(age, 2), data = Wage)
wage_age_poly3 <- lm(wage ~ poly(age, 3), data = Wage)
wage_age_poly4 <- lm(wage ~ poly(age, 4), data = Wage)
wage_age_poly5 <- lm(wage ~ poly(age, 5), data = Wage)

anova(wage_age_poly, wage_age_poly2, wage_age_poly3,
      wage_age_poly4, wage_age_poly5)

"The order of the models are important as anova will assume that the first
model is a subset of the second model, the second is a subset of the third
and so on."

"The p-value of comparing the quadratic model with the linear model is
essentially zero. Thus the quadratic model is definitely better than the
linear model.

The p-value after comparing the cubic model with the quadratic model is
very lowthus the cubic model is definitely preferable.

The p-value comparing the 4-degree polynomial is not significant at alpha
= 0.05, thus the 4 degree-polynomial seems redundant. However it is very
close to 0.05, we should still consider it.

The p-value comparing the 5-degree polynomial with the 4-degree polynomial
is huge. Suggesting that a 5-degree polynomial is definitely redundant."

coef(summary(wage_age_poly5))
"Notice that the square of the t-values are the F-statistics from the anova()
function. This is only true if we use the orthogonal polynomials."

rm(wage_age_poly, wage_age_poly2, wage_age_poly3,
   wage_age_poly4, wage_age_poly5)

# Next we will predict whether an individual earns more than $250,000 per year
# based on their age.

wage_age_logit_poly4 = glm(I(wage >250) ~ poly(age, 4),
                           data = Wage,
                           family = "binomial")

# We use the wrapper I() to create the binary response variable on the fly.
# The expression wage > 250 evaluates to a logical variablecontaining TRUEs
# and FALSEs, which glm() coerces to binary by setting the TRUEs to 1 and the
# FALSEs to 0.

#Calculating the probabilities
wage_age_logit_poly4_probs = predict(wage_age_logit_poly4, 
                                     newdata = list(age = age.grid),
                                     type = "response",
                                     se = TRUE)

#Making the predictions
wage_age_logit_poly4_preds = c()

for (i in c(1:length(wage_age_logit_poly4_probs$fit))) {
  if (wage_age_logit_poly4_probs$fit[i] < 0.5) {
    wage_age_logit_poly4_preds[i] <- 0
  } else {
    wage_age_logit_poly4_preds[i] <- 1
  }
}
rm(i)

se_bands <- cbind(wage_age_logit_poly4_probs$fit - 2*wage_age_logit_poly4_probs$se.fit,
                  wage_age_logit_poly4_probs$fit + 2*wage_age_logit_poly4_probs$se.fit)

"There are negative lower bounds of the se_bands. This happens because the
standard error that was return was the standard error for the logit, not the
posterior probability that we requested with the argument type='response'"

#Predicting the logit, not the probabilities
wage_age_logit_poly4_logit = predict(wage_age_logit_poly4, 
                                     newdata = list(age = age.grid),
                                     se = TRUE)

wage_age_logit_poly4_probs = exp(wage_age_logit_poly4_logit$fit) /
                              (1 + exp(wage_age_logit_poly4_logit$fit))

se_bands_logit = cbind(wage_age_logit_poly4_logit$fit + 2*wage_age_logit_poly4_logit$se.fit,
                       wage_age_logit_poly4_logit$fit - 2*wage_age_logit_poly4_logit$se.fit)
                           
se_bands = exp(se_bands_logit) / (1+exp(se_bands_logit))

"Now we can see that the lower bounds have no negative values"

#Plotting the results
logit_result <- data.frame(cbind(se_bands, age.grid, wage_age_logit_poly4_probs))
library(dplyr)
logit_result <- rename(logit_result, Upper_bound = V1,
                       Lower_bound = V2,
                       Age = age.grid,
                       Probability = wage_age_logit_poly4_probs)
ggplot(logit_result[1:53,], aes(x = Age)) +
  geom_line(aes(y = Probability), color = "blue") +
  geom_line(aes(y = Lower_bound), color = "blue", linetype = 2) +
  geom_line(aes(y = Upper_bound), color = "blue", linetype = 2) +
  theme_bw()

"We are omitting age higher than 70 as the upper bound for the 95% confidence
interval is very large and the plot becomes uninformative."

#The way the book create the plot
plot(age ,I(wage >250),xlim=agelims ,type="n",ylim=c(0,.2))
points(jitter(age), I((wage >250)/5),cex=.5,pch ="|",
         col="darkgrey ")
lines(age.grid ,pfit ,lwd=2, col ="blue")
matlines (age.grid ,se.bands ,lwd=1, col=" blue",lty=3)

"Thus the Lab for Polynomial Regression is done."