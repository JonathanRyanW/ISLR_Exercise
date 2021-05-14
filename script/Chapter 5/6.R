# 6. We continue to consider the use of a logistic regression model to
# predict the probability of default using income and balance on the
# Default data set. In particular, we will now compute estimates for
# the standard errors of the income and balance logistic regression
# coefficients in two different ways:
# (1) using the bootstrap
# (2) using the standard formula for computing the SE in the glm() function.

# (a) Using the summary() and glm() functions, determine the estimated standard
# errors for the coefficients associated with income and balance in a multiple
# logistic regression model that uses both predictors.

library(ISLR)
head(Default)
dim(Default)

default_logit <- glm(default ~ income+balance, data = Default, family = "binomial")
summary(default_logit)
coef(default_logit)

"The SE for balance is 2.274e-04 while the SE for income is 4.985e-06"

# (b) Write a function, boot.fn(), that takes as input the Default data
# set as well as an index of the observations, and that outputs
# the coefficient estimates for income and balance in the multiple
# logistic regression model.

boot.fn <- function(data, index) {
  return(coef(glm(default ~ income+balance,
                  data = Default, 
                  subset = index,
                  family = "binomial")))
}

# (c) Use the boot() function together with your boot.fn() function to
# estimate the standard errors of the logistic regression coefficients
# for income and balance.

library(boot)

boot(Default, boot.fn, R = 100)

'Here we can see that the SE are a little bit different.'

# (d) Comment on the estimated standard errors obtained using the
# glm() function and using your bootstrap function.

"The estimated SE from the Bootstrap method is very close to the SE in the
logistic regression."