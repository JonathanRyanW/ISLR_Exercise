library(ISLR)
names(Wage)
dim(Wage)

cut(Wage$age, 4)
table(cut(Wage$age, 4))

"The cut functions takes continuous variables and returns an ordered categorical
variable (ordinal scaled). It automatically assigns cutpoints at 33.5, 49, and
64.5. We can assign our own cutpoints by typing the argument breaks = c()"

table(cut(Wage$age, breaks = c(min(Wage$age), 35, 50, 65, max(Wage$age))))


# Fitting a Piecewise Step Linear model

wage_age_step = lm(wage ~ cut(age,4), data = Wage)
summary(wage_age_step)
coef(summary(wage_age_step))

"The function cut() returns an ordered categorical variable, the lm() function
then creates a set of dummy variables for use in the regression.

The age<33.5 category is left out, so the intercept coefficient of $94,160 can
be interpreted as the average salary for those under 33.5 years of age. The
other coefficients can be interpreted as the average additional salary for
those in the other age groups."

#Making Predictions
agelims = range(Wage$age)
age.grid = seq(from = agelims[1], to = agelims[2])

wage_age_step_preds <- predict(wage_age_step,
                               newdata = list(age = age.grid),
                               se = TRUE)

head(wage_age_step_preds$fit)
head(wage_age_step_preds$se.fit)

lower_bound <- wage_age_step_preds$fit - 2*wage_age_step_preds$se.fit
upper_bound <- wage_age_step_preds$fit + 2*wage_age_step_preds$se.fit

step_result <- data.frame(age = age.grid,
                          predictions = wage_age_step_preds$fit,
                          lower_bound, upper_bound)

#Plotting the result
library(ggplot2)

ggplot(data = step_result, aes(x = age)) +
  geom_line(aes(y = predictions), color = "blue") +
  geom_line(aes(y = lower_bound), color = "blue", linetype = 2) +
  geom_line(aes(y = upper_bound), color = "blue", linetype = 2) +
  theme_bw()