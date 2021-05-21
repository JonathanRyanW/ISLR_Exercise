#Loading packages
library(splines)
library(ISLR)

#A look on the dataset
names(Wage)
dim(Wage)

"We saw that regression splines can be fit by constructing an appropriate
matrix of basis functions.

The bs() function generates the entire matrix of basis functions for splines
with the specified set of knots. By default, cubic splines are produced."

#Creating a regression spline to the Wage dataset
wage_age_splines= lm(wage ~ bs(age, knots = c(25,40,60)), data = Wage)

#Making predictions
agelims = range(Wage$age)
age.grid = seq(from = agelims[1], to = agelims[2])

predictions = predict(wage_age_splines,
                      newdata = list(age = age.grid),
                      se = TRUE)

#Calculating lower and uppper bound of the 95% confidence interval
lower_bound <- predictions$fit - 2*predictions$se.fit
upper_bound <- predictions$fit + 2*predictions$se.fit

#Storing the results in a dataframe
splines_result <- data.frame(predictions$fit, lower_bound, upper_bound,
                             age = age.grid)

#Plotting the result using ggplot2
library(ggplot2)
ggplot(data = splines_result, aes(x = age)) +
  geom_line(aes(y = predictions.fit), color = "blue") +
  geom_line(aes(y = lower_bound), color = "blue", linetype = 2) +
  geom_line(aes(y = upper_bound), color = "blue", linetype = 2) +
  theme_bw()

# Specifying knots at ages 25, 40, and 60.
dim(bs(Wage$age, knots = c(25,40,60)))

"This produces a spline with six basis functions. (A cubic spline with three
knots has seven degrees of freedom; these degrees of freedom are used up by an
intercept and six basis functions.) We could also use the df option to
produce a spline with knots at uniform quantiles of the data."

dim(bs(Wage$age ,df=6))
attr(bs(Wage$age ,df=6) ,"knots")

"In this case R chooses knots at ages 33.8, 42.0, and 51.0, which correspond
to the 25th, 50th, and 75th percentiles of age. The function bs() also has
a degree argument, so we can fit splines of any degree, rather than the
default degree of 3 (which yields a cubic spline)."

#Fitting a natural regression spline
wage_age_natural_splines = lm(wage ~ ns(age, df = 4), data = Wage)

#Making predictions
ns_predictions = predict(wage_age_natural_splines,
                         newdata = list(age = age.grid),
                         se = TRUE)

#Calculating lower and uppper bound of the 95% confidence interval
lower_bound <- ns_predictions$fit - 2*ns_predictions$se.fit
upper_bound <- ns_predictions$fit + 2*ns_predictions$se.fit

#Storing the results in a dataframe
ns_result <- data.frame(ns_predictions$fit, lower_bound, upper_bound,
                        age = age.grid)
                             
#Plotting the result using ggplot2
library(ggplot2)
ggplot(data = ns_result, aes(x = age)) +
  geom_line(aes(y = ns_predictions.fit), color = "blue") +
  geom_line(aes(y = lower_bound), color = "blue", linetype = 2) +
  geom_line(aes(y = upper_bound), color = "blue", linetype = 2) +
  theme_bw()

"We can see that the 95$ Confidence Interval is more stable near the end and
the beginning of the line than the previous plot. Note that As with the bs()
function, we could specify the knots using the knots argument."

# Fitting a Smoothing Spline Regression on the Wage dataset.
wage_age_smooth_splines= smooth.spline(Wage$age, Wage$wage, df = 16)
wage_age_smooth_splines$lambda #0.0006537868

#Using Cross Validation to determine lambda
wage_age_smooth_splines_cv= smooth.spline(Wage$age, Wage$wage, cv = TRUE)
wage_age_smooth_splines_cv$lambda #0.02792303
wage_age_smooth_splines_cv$df #6.794596

"In the first model, we specified df = 16. The smooth.spline() function then
determines which value of λ leads to 16 degrees of freedom.

In the second model, we select the smoothness level by cross validation. This
results in a value of λ that yields 6.8 degrees of freedom."

#Making predictions
ss_predictions <- predict(wage_age_smooth_splines, 
                          newdata = list(age = age.grid),
                          se = TRUE)

ss_cv_predictions <- predict(wage_age_smooth_splines_cv, 
                             newdata = list(age = age.grid),
                             se = TRUE)

#Plotting the results

#The way the book create the plot
plot(Wage$age, Wage$wage, xlim=agelims, cex =.5, col = "darkgrey")
title("Smoothing Spline ")
lines(ss_predictions, col="red", lwd =2)
lines(ss_cv_predictions, col="blue", lwd=2)
legend ("topright", legend = c("16 DF" ,"6.8 DF"),
        col = c("red","blue"), lty = 1, lwd = 2, cex = 0.8)















