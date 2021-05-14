# This data is similar in nature to the Smarket data, except that it contains
# 1089 weekly returns for 21 years, from 1990 to 2010.

# (a) Produce some numerical and graphical summaries of the Weekly
# data. Do there appear to be any patterns?
library(ISLR)
head(Weekly)
names(Weekly)

"I have done some EDA on the dataset. The results are plenty and informative.
I will not repeat myself here. Please read the EDA Weekly.R script."

# (b) Use the full data set to perform a logistic regression with
# Direction as the response and the five lag variables plus Volume
# as predictors. Use the summary function to print the results. Do
# any of the predictors appear to be statistically significant? If so,
# which ones?

weekly_logit <- glm(formula = Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
                    data = Weekly,
                    family = "binomial")
summary(weekly_logit)

"Based on the p-values, the only relevant variables are Intercept and Lag2."

# (c) Compute the confusion matrix and overall fraction of correct
# predictions. Explain what the confusion matrix is telling you
# about the types of mistakes made by logistic regression.

"i am making predictions on the train dataset (the whole dataset)"
logit_prob <- predict(weekly_logit,
                      Weekly[,2:7],
                      type = "response")

logit_pred <- c()
for (i in c(1:1089)) {
  if (logit_prob[i] < 0.5) {
    logit_pred[i] = "Down"
  } else {
    logit_pred[i] = "Up"
  }
}

rm(i)

contrasts(Weekly$Direction)
"Up is 1 and Down is 0"

#Building confusion matrix
table(logit_pred, Weekly$Direction)
accuracy_logit <- mean(logit_pred == Weekly$Direction)

"We can see that our model predicts most of the weeks are going Up. We should
change the cutoff value. The accuracy is 56% but this model is not useful at
this state. possible cutoff values are 0.6, 0.7, 0.8, and so on."

# (d) Now fit the logistic regression model using a training data period
# from 1990 to 2008, with Lag2 as the only predictor. Compute the
# confusion matrix and the overall fraction of correct predictions
# for the held out data (that is, the data from 2009 and 2010).

#Splitting the dataset
train <- Weekly[Weekly$Year < 2009,]
test <- Weekly[Weekly$Year >= 2009,]

#Building the logistic regression model
weekly_logit_lag2 <- glm(formula = Direction ~ Lag2,
                         data = train,
                         family = "binomial")
summary(weekly_logit_lag2)

#Creating the confusion matrix
logit_lag2_prob <- predict(weekly_logit_lag2,
                           test[,3],
                           type = "response")

logit_lag2_pred <- c()
for (i in c(1:104)) {
  if (logit_lag2_prob[i] < 0.5) {
    logit_lag2_pred[i] = "Down"
  } else {
    logit_lag2_pred[i] = "Up"
  }
}

rm(i)

table(logit_lag2_pred, test$Direction)
accuracy_logit_lag2 <- mean(logit_lag2_pred == test$Direction)

"The same thing happens again, the model predicts most of the test observations
as Up weeks. The accuracy is high (62.5%) because most of the test dataset is Up
(61/104 = 58.6%)"

# (e) Repeat (d) using LDA.
library(MASS)
weekly_lda <- lda(formula = Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
                  data = train)
weekly_lda_lag2 <- lda(formula = Direction ~ Lag2,
                       data = train)

lda_pred <- predict(weekly_lda, test[,2:7])
lda_lag2_pred <- predict(weekly_lda_lag2, test[,2:7])

result_lda <- data.frame(Actual = test$Direction, 
                         LDA = lda_pred$class, 
                         LDA_Lag2 =lda_lag2_pred$class)

table(lda_pred$class, test$Direction)
table(lda_lag2_pred$class, test$Direction)

mean(lda_pred$class == test$Direction)
mean(lda_lag2_pred$class == test$Direction)

"The lda with the 6 predictors got 46.15% accuracy while the one with only Lag2
as predictor got 62.5% accuracy. The 6 predictor model's prediction are not
biased towards any direction but the one with only Lag2 heavily lean to Up.
Since there are in fact more Up weeks than Down weeks, it performs better in
terms of accuracy."

# (f) Repeat (d) using QDA.
weekly_qda <- qda(formula = Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
                  data = train)
weekly_qda_lag2 <- qda(formula = Direction ~ Lag2,
                       data = train)

qda_pred <- predict(weekly_qda, test[,2:7])
qda_lag2_pred <- predict(weekly_qda_lag2, test[,2:7])

result_qda <- data.frame(Actual = test$Direction, 
                         QDA = qda_pred$class, 
                         QDA_Lag2 =qda_lag2_pred$class)

table(qda_pred$class, test$Direction)
table(qda_lag2_pred$class, test$Direction)

mean(qda_pred$class == test$Direction)
mean(qda_lag2_pred$class == test$Direction)

"The result are similar with the LDA model. Again, the model with 6 predictors
are not heavily biased to any direction whilde the model with only Lag2 predicts
every single week as an Up week. Since there are in fact more Up weeks than Down
the model with only Lag2 has a higher accuracy"

# (g) Repeat (d) using KNN with K = 1.
library(class)

weekly_knn_pred <- knn(train[,2:7],
                       test[,2:7],
                       train[,9],
                       k = 1)
table(weekly_knn_pred, test$Direction)

weekly_knn_lag2_pred <- knn(data.frame(train[,3]),
                            data.frame(test[,3]),
                            train[,9],
                            k = 1)
table(weekly_knn_lag2_pred, test$Direction)

mean(weekly_knn_pred == test$Direction)
mean(weekly_knn_lag2_pred == test$Direction)

"There is an added accuracy when we only use the Lag2 variable. The 2 models
are virtually the same."

# (h) Which of these methods appears to provide the best results on
# this data?

"Based on the accuracy, the LDA model with only Lag2 as predictor performs the
best among every other model. In predicting the direction of the market, i dont
think there is a preference in predicting Up weeks correctly or Down weeks
correctly since if we are sure of the direction of the week we can choose to 
trade long or short accordingly. That is why i think the model with the 
highest accuracy is the best model to choose."

# (i) Experiment with different combinations of predictors, including possible
# transformations and interactions, for each of the
# methods. Report the variables, method, and associated confusion matrix that
# appears to provide the best results on the held
# out data. Note that you should also experiment with values for
# K in the KNN classifier.




# 11. In this problem, you will develop a model to predict whether a given
# car gets high or low gas mileage based on the Auto data set.
library(ISLR)
head(Auto)

# (a) Create a binary variable, mpg01, that contains a 1 if mpg contains
# a value above its median, and a 0 if mpg contains a value below
# its median. You can compute the median using the median()
# function. Note you may find it helpful to use the data.frame()
# function to create a single data set containing both mpg01 and
# the other Auto variables.

auto_extended <- Auto
mpg_median <- median(Auto$mpg)
mpg01 <- c()

for (i in c(1:392)) {
  if (Auto$mpg[i] < mpg_median){
    mpg01[i] = 0
  } else{
    mpg01[i] = 1
  }
}

auto_extended$mpg01 <- as.factor(mpg01)

rm(i, mpg_median, mpg01)

# (b) Explore the data graphically in order to investigate the association
# between mpg01 and the other features. Which of the other
# features seem most likely to be useful in predicting mpg01? Scatterplots and
# boxplots may be useful tools to answer this question. Describe your findings.

library(ggplot2)
ggplot(auto_extended, aes(x = mpg01, y = horsepower)) +
  geom_boxplot() +
  ylab("Engine Horsepower") +
  theme_bw()

"We can see that cars with mpg higher than the median have substantially lower
horsepower than cars with mpg lower than the median. This "

ggplot(auto_extended, aes(x = mpg01, y = weight)) +
  geom_boxplot() +
  ylab("Vehicle weight (lbs.)") +
  theme_bw()

"Cars with mpg higher than the median have substantially lower weight than cars
with mpg lower than the median. Is this not causal? I think the car weight might
have something to do with mpg since the heavier the car the more force it needs
to muster to move itself, such lower mpg values."

ggplot(auto_extended, aes(x = mpg01, y = acceleration)) +
  geom_boxplot() +
  ylab("Time to accelerate from 0 to 60 mph (seconds)") +
  theme_bw()

"Cars with higher mpg than the median needs slightly more time to go from 0 to
60 mph than cars with lower mpg than the median. I think there might be
connection with the horsepower. We already know that the cars with higher mpg
than the median have lower horsepower, maybe this is why they also need more
time."

# (c) Split the data into a training set and a test set.

library(caTools)

sample <- sample.split(Auto$mpg, SplitRatio = 7/10)
train <- auto_extended[sample,]
test <- auto_extended[!sample,]

# (d) Perform LDA on the training data in order to predict mpg01
# using the variables that seemed most associated with mpg01 in
# (b). What is the test error of the model obtained?

# Building LDA
library(MASS)

auto_lda <- lda(mpg01 ~ cylinders+displacement+horsepower+weight+acceleration,
                train)
auto_lda_pred <- predict(auto_lda, test)
auto_lda_result <- data.frame(test$mpg01, auto_lda_pred$class)

#Creating Confusion Matrix
table(auto_lda_pred$class, test$mpg01)
mean(test$mpg01 == auto_lda_pred$class)

"The accuracy of the model is quite high with 88.46%. I wonder if the incorrect
predictions are those cars with mpg near the median. Maybe that is why the LDA
cannot correctly classify them."

mean(test$mpg[test$mpg01 == 0 & auto_lda_pred$class == 0]) #16.45294
mean(test$mpg[test$mpg01 == 0 & auto_lda_pred$class == 1]) #20

"Here we can see that when the real mpg values are far from the median 22.75
LDA can perform well. While for the cases in which LDA incorrectly classify
mpg01 the mean is closer to 22.75, that is, 20. This suggest that the closer
the mpg value to the median, the harder it is for LDA to correctly assign their
class.

But mpg is not part of the chosen predictors. Which means that mpg is connected
with the other variables such that we can deduce the mpg by looking at the
other values."

mean(test$mpg[test$mpg01 == 1 & auto_lda_pred$class == 1]) #29.33171
mean(test$mpg[test$mpg01 == 1 & auto_lda_pred$class == 0]) #26.55714

"The same thing is true for cars with mpg higher than the median. The incorrect
cases are those whose mpgs are close to the median. Suggesting that LDA stuggles
to classify cars with mpg values near the median."

# (e) Perform QDA on the training data in order to predict mpg01
# using the variables that seemed most associated with mpg01 in
# (b). What is the test error of the model obtained?

# Building QDA
auto_qda <- qda(mpg01 ~ cylinders+displacement+horsepower+weight+acceleration,
                train)
auto_qda_pred <- predict(auto_qda, test)
auto_qda_result <- data.frame(test$mpg01, auto_qda_pred$class)

#Creating Confusion Matrix
table(auto_qda_pred$class, test$mpg01)

#Calculating accuracy and error rate
mean(test$mpg01 == auto_qda_pred$class)
1 - mean(test$mpg01 == auto_qda_pred$class) #0.1153846

"The results are exactly the same. Why is the result exactly the same? The test
error rate is 11.53%"

# (f) Perform logistic regression on the training data in order to predict mpg01
# using the variables that seemed most associated with
# mpg01 in (b). What is the test error of the model obtained?

auto_logit <- glm(mpg01 ~ cylinders+displacement+horsepower+weight+acceleration,
                  data = train,
                  family = "binomial")
summary(auto_logit)

"Only displacement and horsepower are affecting mpg01."
auto_logit_prob <- predict(auto_logit, test, type = "response")
auto_logit_pred <- c()

for (i in c(1:104)) {
  if (auto_logit_prob[i] < 0.5) {
    auto_logit_pred[i] = 0
  } else {
    auto_logit_pred[i] = 1
  }
}
rm(i)

auto_logit_result <- data.frame(test$mpg01, auto_logit_pred)

# Creating Confusion Matrix
table(auto_logit_pred, test$mpg01)
mean(test$mpg01 == auto_logit_pred)

"High accuracy with 87.50%. This is slightly lower than the LDA and QDA accuracy.
"

# (g) Perform KNN on the training data, with several values of K, in
# order to predict mpg01. Use only the variables that seemed most
# associated with mpg01 in (b). What test errors do you obtain?
# Which value of K seems to perform the best on this data set?

"Before we perform the KNN, we need to know which variables are significantly
affecting mpg01. I will build a logistic regression model with all variables
as predictor and then evaluate their p-value."

auto_logit_all <- glm(mpg01 ~ cylinders+displacement+horsepower+weight+
                        acceleration+year+origin,
                      data = train[,c(-1,-9)],
                      family = "binomial")
summary(auto_logit_all)

"We find a very interesting result. Only year and weight are a significant
predictor of mpg01. We shall perform KNN with only these 2 predictors"

#Performing KNN
library(class)
library(dplyr)

# Selecting only the relevant variables
train_knn <- select(train, weight, year)
test_knn <- select(test, weight, year)

#Performing KNN with k = 3
auto_knn_weight_year <- knn(train_knn, test_knn, train$mpg01, k = 3)
mean(test$mpg01 == auto_knn_weight_year)

"The accuracy is 87.5%. We can perform KNN repeatedy to find the best k value."

#Finding best k
auto_knn_accuracy <- c()
for (i in c(1:20)) {
  temporary <- knn(train_knn, test_knn, train$mpg01, k = i)
  auto_knn_accuracy[i] <- mean(test$mpg01 == temporary)
}
auto_knn_accuracy

"There are some k values that performs better than k = 3. The highest accuracy
is provided by k = 4. The difference is so small. I will go with k = 3 because
it is an odd value of k and as such prevents any ties in a binary classification.
"

# 12. This problem involves writing functions.
# (a) Write a function, Power(), that prints out the result of raising 2
# to the 3rd power. In other words, your function should compute
# 2**3 and print out the results.

Power <- function(){print(2**3)}

# (b) Create a new function, Power2(), that allows you to pass any
# two numbers, x and a, and prints out the value of x^a.

Power2 <- function(x,a){print(x**a)}

#   (c) Using the Power2() function that you just wrote, compute 10**3,
#   8**17, and 131**3.

Power2(10, 3)
Power2(8, 17)
Power2(131, 3)

#   (d) Now create a new function, Power3(), that actually returns the
#   result x^a as an R object, rather than simply printing it to the
#   screen.

Power3 <- function(x,a){return(x**a)}

# (e) Now using the Power3() function, create a plot of f(x) = x**2.
# The x-axis should display a range of integers from 1 to 10, and
# the y-axis should display x2. Label the axes appropriately, and
# use an appropriate title for the figure. Consider displaying either
# the x-axis, the y-axis, or both on the log-scale. You can do this
# by using log=‘‘x’’, log=‘‘y’’, or log=‘‘xy’’ as arguments to
# the plot() function.

df <- data.frame(x = c(1:10))
df$Power3 <- Power3(df$x,2)

plot(x = df$x, y = df$Power3)
plot(x = df$x, y = df$Power3, log = "xy")

"Using log scale makes the plot linear."

# (f) Create a function, PlotPower(), that allows you to create a plot
# of x against x^a for a fixed a and for a range of values of x.

PlotPower <- function(x,a){
  plot(x, Power3(x,a))
}

# 13. Using the Boston data set, fit classification models in order to predict
# whether a given suburb has a crime rate above or below the median.
# Explore logistic regression, LDA, and KNN models using various subsets of the
# predictors. Describe your finding

head(Boston)
dim(Boston)

# Adding a new variable crim01 that indicates whether the town's crime rate
# is above (1) or below (0) the median 

crim_median <- median(Boston$crim)
boston_extended <- Boston
crim01 <- c()

for (i in c(1:506)) {
  if (Boston$crim[i] < crim_median) {
    crim01[i] <- 0
  } else {
    crim01[i] <- 1
  }
}

boston_extended$crim01 <- crim01

rm(i, crim01, crim_median)

#Splitting the data
sample <- sample.split(Boston$crim, SplitRatio = 0.7)
train <- boston_extended[sample,]
test <- boston_extended[!sample,]

# Building logistic regression model
boston_logit_all <- glm(crim01 ~ .-crim,
                    data = train,
                    family = "binomial")
summary(boston_logit_all)

"Out of all variables (excluding crim of course since using crim as a predictor
would mean target leakage) the good predictors of crim01 (p-value below 0.05)
are indus, nox, dis, rad, tax, ptratio, medv."

boston_logit_all_prob <- predict(boston_logit_all,
                                 test,
                                 type = "response")
boston_logit_all_pred <- c()

for (i in c(1:152)) {
  if (boston_logit_all_prob[i] < 0.5) {
    boston_logit_all_pred[i] <- 0
  } else {
    boston_logit_all_pred[i] <- 1
  }
}
rm(i)

#Creating the Confusion Matrix
table(boston_logit_all_pred, test$crim01)

#Calculating accuracy
mean(boston_logit_all_pred == test$crim01)

"High accuracy of 87.5%."

#Building logistic regression model with the selected variables
boston_logit_select <- glm(crim01 ~ indus+nox+dis+rad+tax+ptratio+medv,
                        data = train,
                        family = "binomial")
summary(boston_logit_select)

boston_logit_select_prob <- predict(boston_logit_select,
                                    test,
                                    type = "response")
boston_logit_select_pred <- c()

for (i in c(1:152)) {
  if (boston_logit_select_prob[i] < 0.5) {
    boston_logit_select_pred[i] <- 0
  } else {
    boston_logit_select_pred[i] <- 1
  }
}
rm(i)

#Creating the Confusion Matrix
table(boston_logit_select_pred, test$crim01)

#Calculating accuracy
mean(boston_logit_select_pred == test$crim01)

"Same accuracy with 89.47% with less variables. This model is more
preferable than the previous model."

#Building LDA and QDA model with selected variables
boston_lda <- lda(crim01 ~ indus+nox+dis+rad+tax+ptratio+medv, data = train)
boston_qda <- qda(crim01 ~ indus+nox+dis+rad+tax+ptratio+medv, data = train)                  

#Making predictions
boston_lda_pred <- predict(boston_lda, test)
boston_qda_pred <- predict(boston_qda, test)

table(boston_lda_pred$class, test$crim01)
table(boston_qda_pred$class, test$crim01)

mean(boston_lda_pred$class == test$crim01) #0.8355263
mean(boston_qda_pred$class == test$crim01) #0.875

"The QDA performs slightly better than LDA."

#Finding the best k value for KNN
train_knn <- select(train, indus, nox, dis, rad, tax, ptratio, medv)
test_knn <- select(test, indus, nox, dis, rad, tax, ptratio, medv)

accuracy <- c()
for(i in c(1:20)){
  temp <- knn(train, test, train$crim01, k = i)
  accuracy[i] <- mean(temp == test$crim01)
}
rm(i, temp)
which(accuracy == max(accuracy))

"The best accuracy is provided by k = 5. We will proceed with this value."

#Performing KNN with k = 5
boston_knn_select <- knn(train, test, train$crim01, k = 5)

#Creating Confusion Matrix
table(boston_knn_select, test$crim01)

#Calculating accuracy
mean(boston_knn_select == test$crim01)

"We have 94.07% accuracy"

"In conclusion, the KNN performs the best with 94.07% accuracy."