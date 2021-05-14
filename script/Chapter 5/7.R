# 7. In Sections 5.3.2 and 5.3.3, we saw that the cv.glm() function can be
# used in order to compute the LOOCV test error estimate. Alternatively, one
# could compute those quantities using just the glm() and predict.glm()
# functions, and a for loop. You will now take this approach in order to compute
# the LOOCV error for a simple logistic regression model on the Weekly data set.

# (a) Fit a logistic regression model that predicts Direction using Lag1
# and Lag2.

library(ISLR)
head(Weekly)
dim(Weekly)

weekly_logit <- glm(Direction ~ Lag1+Lag2,
                    data = Weekly,
                    family = "binomial")
summary(weekly_logit)

# (b) Fit a logistic regression model that predicts Direction using Lag1
# and Lag2 using all but the first observation.

weekly_logit_1 <- glm(Direction ~ Lag1+Lag2,
                      data = Weekly[-1,],
                      family = "binomial")
summary(weekly_logit_1)

# (c) Use the model from (b) to predict the direction of the first observation.
# Was this observation correctly classified?

prediction <- predict(weekly_logit_1, Weekly[1,], type = "response")
Weekly$Direction[1]

"The prediction is Up since it is 0.571 > 0.5 while the actual value is Down.
It was incorrectly classified."

# (d) Write a for loop from i = 1 to i = n, where n is the number of
# observations in the data set, that performs each of the following
# steps:
# i. Fit a logistic regression model using all but the ith observation to
# predict Direction using Lag1 and Lag2.
# ii. Compute the posterior probability of the market moving up
# for the ith observation.
# iii. Use the posterior probability for the ith observation in order
# to predict whether or not the market moves up.
# iv. Determine whether or not an error was made in predicting
# the direction for the ith observation. If an error was made,
# then indicate this as a 1, and otherwise indicate it as a 0.

error <- c()
for (i in 1:1089) {
  weekly_logit <- glm(Direction ~ Lag1+Lag2,
                        data = Weekly[-i,],
                        family = "binomial")
  
  prediction <- predict(weekly_logit_1, Weekly[i,], type = "response")
  
  if (prediction < 0.5) {
    prediction <- "Down"
  } else {
    prediction <- "Up"
  }
  
  error[i] <- prediction == Weekly$Direction[i]
}
rm(i, prediction)

# (e) Take the average of the n numbers obtained in (d)iv in order to
# obtain the LOOCV estimate for the test error. Comment on the
# results.

sum(error)
mean(error)

"We have 606 error out of 1089 attempts (55.64% error rate). This is not
suprising as we have already worked with this dataset and established that
even a 60% accuracy is impressive. Our accuracy this time is 44.36%."
