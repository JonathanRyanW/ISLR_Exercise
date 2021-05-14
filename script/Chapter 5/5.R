# 5. In Chapter 4, we used logistic regression to predict the probability of
# default using income and balance on the Default data set. We will
# now estimate the test error of this logistic regression model using the
# validation set approach. Do not forget to set a random seed before
# beginning your analysis.

# (a) Fit a logistic regression model that uses income and balance to
# predict default.

library(ISLR)
head(Default)
dim(Default)

#Creating the logistic regression model
default_logit <- glm(default ~ income+balance, 
                     data = Default,
                     family = "binomial")
summary(default_logit)

"Both predictors are influencing default."

#Making predictions on the whole dataset
default_logit_prob <- predict(default_logit, Default, type = "response")
default_logit_pred <- c()

for (i in 1:10000) {
  if (default_logit_prob[i] < 0.5) {
    default_logit_pred[i] <- "No"
  } else {
    default_logit_pred[i] <- "Yes"
  }
}
rm(i)

#Creating Confusion Matrix
table(default_logit_pred, Default$default)

#Calculating accuracy
mean(default_logit_pred == Default$default)

"High accuracy of 97.37%. We have already did this analysis before, the accuracy
is similar. KNN aslso provides similar accuracy.

We only succeeded to detect 108 / (108 + 225) = 32.43% of all default. We
should consider lowering the cutoff point to make it easier for logit to assign
Yes to the response

We tested cutoff time of 0.3 and 0.1 The result is as follows:

0.3
We have sacrificed some of our accuracy. It went down from 97.37% to 96.86%.
But we succeeded to detect 164 out of 333 default (49%).

0.1
We sacrificed even more accuracy, this time it went down to 93.48% but we
successfully detected 243 out of 333 defaults 72.97%"

rm(default_logit_pred, default_logit_prob)

# (b) Using the validation set approach, estimate the test error of this
# model. In order to do this, you must perform the following steps:
# i. Split the sample set into a training set and a validation set.

library(caTools)
set.seed(1)
sample <- sample.split(Default$default, SplitRatio = 0.7)
train <- Default[sample,]
test <- Default[!sample,]

sample <- sample.split(test$default, SplitRatio = 0.5)
valid <- test[sample,]
test <- test[!sample,]

# ii. Fit a multiple logistic regression model using only the training
# observations.
default_train_logit <- glm(default ~ income+balance, 
                           data = train,
                           family = "binomial")
summary(default_train_logit)

"Both predictors are still significantly affecting default."

# iii. Obtain a prediction of default status for each individual in
# the validation set by computing the posterior probability of
# default for that individual, and classifying the individual to
# the default category if the posterior probability is greater
# than 0.5.

default_logit_prob <- predict(default_train_logit,
                                    train,
                                    type = "response")
default_logit_pred <- c()

for (i in 1:1500) {
  if (default_logit_prob[i] < 0.5) {
    default_logit_pred[i] <- "No"
  } else {
    default_logit_pred[i] <- "Yes"
  }
}
rm(i)

#Creating Confusion Matrix
table(default_logit_pred, valid$default)

#Calculating accuracy
mean(default_logit_pred == valid$default)

"Oh this is bad. Our accuracy is high (95.40%) but we did not predict any of the
default cases successfully. Out of 50 default in the valid dataset, we correctly
predicted none of them."

# iv. Compute the validation set error, which is the fraction of
# the observations in the validation set that are misclassified.

1 - mean(default_logit_pred == valid$default)

# (c) Repeat the process in (b) three times, using three different splits
# of the observations into a training set and a validation set.
# Comment on the results obtained.

accuracy <- c()
sensitivity <- c()

set.seed(1000)
for(i in 1:3) {
  sample <- sample.split(Default$default, SplitRatio = 0.7)
  train <- Default[sample,]
  test <- Default[!sample,]
  
  sample <- sample.split(test$default, SplitRatio = 0.5)
  valid <- test[sample,]
  test <- test[!sample,]
  
  default_train_logit <- glm(default ~ income+balance, 
                             data = train,
                             family = "binomial")
  
  default_logit_prob <- predict(default_train_logit,
                                train,
                                type = "response")
  default_logit_pred <- c()
  
  for (i in 1:1500) {
    if (default_logit_prob[i] < 0.5) {
      default_logit_pred[i] <- "No"
    } else {
      default_logit_pred[i] <- "Yes"
    }
  }
  rm(i)

  #Calculating accuracy
  accuracy <- append(accuracy, mean(default_logit_pred == valid$default))
  
  #How many of the default are correctly predicted?
  sensitivity <- append(sensitivity,
                        mean(default_logit_pred == "Yes" & valid$default == "Yes"))
}

"Our model is very bad, they can't accurately predict which observations are
default or not. The have high accuracy but their sensitivity is very low."

# (d) Now consider a logistic regression model that predicts the probability of
# default using income, balance, and a dummy variable for student. Estimate
# the test error for this model using the validation set approach. Comment on
# whether or not including a dummy variable for student leads to a
# reduction in the test error rate.

#Splitting the dataset
set.seed(1001)
sample <- sample.split(Default$default, SplitRatio = 0.7)
train <- Default[sample,]
test <- Default[!sample,]

sample <- sample.split(test$default, SplitRatio = 0.5)
valid <- test[sample,]
test <- test[!sample,]

#Building the logistic regression model
default_student_logit <- glm(default ~ income+balance+student,
                             data = Default,
                             family = "binomial")
summary(default_student_logit)

#Making predictions

default_student_logit_prob <- predict(default_student_logit,
                                      valid,
                                      type = "response")
default_student_logit_pred <- c()

for (i in 1:1500) {
  if (default_student_logit_prob[i] < 0.5) {
    default_student_logit_pred[i] <- "No"
  } else {
    default_student_logit_pred[i] <- "Yes"
  }
}
rm(i)

#Creating Confusion Matrix
table(default_student_logit_pred, valid$default)

#Calculting accuracy
mean(default_student_logit_pred == valid$default)

"We can see that the error rate is in fact reduced. We also predicted 22 out of
28 default correctly. This model is certainly better. It is important to note
that when we add student as a predictor, income becomes irrelevant. This is an
evidence of collinearity between income and balance. We should definitely not
use income in the future."