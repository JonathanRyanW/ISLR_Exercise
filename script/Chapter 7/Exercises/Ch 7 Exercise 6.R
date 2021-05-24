# (a) Perform polynomial regression to predict wage using age. Use CV to select
# the optimal degree for the polynomial. What degree was chosen, and how does
# this compare to the results of hypothesis testing using ANOVA? Make a plot of
# the resulting polynomial fit to the data.

#A look at the dataset
library(ISLR)
dim(Wage)
names(Wage)

#Assigning observations into 10 distinct subsets
set.seed(1001)
subset <- sample(c(rep(c(1:10), 300)), 3000)
wage <- Wage
wage$subset <- subset

rm(subset)

##Performing 10 fold CV to find the optimal polynomial degree
"We will choose the polynomial degree between 1 to 10"

#Creating a matrixx to store the resulting errors
error <- matrix(nrow = 10, ncol = 10)
rownames(error) <- c(1:10)
colnames(error) <- paste("poly_", c(1:10), sep = "")

#Performing the 10 fold CV
for(i in c(1:10)) {
  train <- wage[wage$subset != i,]
  test <- wage[wage$subset == i,]
  
  for (j in c(1:10)){
    model <- lm(wage ~ poly(age,j), data = train)
    predictions <- predict(model, test)
    error[i,j] <- mean((predictions - test$wage)**2)
  }
}
rm(train, test, i, j, model, predictions)

#Calculating the mean error for every polynomial degree
mean <- c()
for (i in c(1:10)) {
  mean[i] <- mean(error[,i])
}
rm(i)

#Attaching the mean of MSE to the matrix of error
error <- rbind(error, mean)

#Finding the minimum MSE
which.min(mean)

"We find that the minimum error is obtained with the 9 degree polynomial."

#Plotting the MSE
plot(mean)
rm(mean)

"We can see that there is a huge decrease of error after adding the quadratic
term, but there is no significant improvement from adding additional terms."

#Performing anova
anova(lm(wage ~ age, data = Wage),
      lm(wage ~ poly(age, 2), data = Wage),
      lm(wage ~ poly(age, 3), data = Wage),
      lm(wage ~ poly(age, 4), data = Wage),
      lm(wage ~ poly(age, 5), data = Wage),
      lm(wage ~ poly(age, 6), data = Wage),
      lm(wage ~ poly(age, 7), data = Wage),
      lm(wage ~ poly(age, 8), data = Wage),
      lm(wage ~ poly(age, 9), data = Wage),
      lm(wage ~ poly(age, 10), data = Wage))

"We can see that there is a significant improvement after adding the quadratic
and the cubic terms. However there does not seem to be any improvement after
adding more terms. There is however a significant improvement after adding the
x**9 term. Using such a complicated model for a minimal boost in accuracy is
definitely not worth it."

#Finding the coefficients for the 3 variables model
coef(lm(wage ~ poly(age, 3), data = Wage))

#Plotting the cubic model to the dataset
library(ggplot2)
ggplot(data = Wage, aes(x = age, y = wage)) +
  geom_point() +
  geom_smooth(formula = y ~ poly(x,3), se = TRUE, color = "red") +
  theme_bw()

# (b) Fit a step function to predict wage using age, and perform cross
# validation to choose the optimal number of cuts. Make a plot of
# the fit obtained.

#Creating a matrix to store the errors
error_step <- matrix(nrow = 10, ncol = 9)
rownames(error_step) <- c(1:10)
colnames(error_step) <- paste("cuts_", c(2:10), sep = "")

#Performing 10-fold CV on the same processed wage data set as before
for(i in c(1:10)) {
  train <- wage[wage$subset != i,]
  test <- wage[wage$subset == i,]
  age <- test$age
  
  for(j in c(2:10)){
    model <- lm(wage ~ cut(train$age,j), data = train)
    predictions <- predict(model, newdata = list(age), se = TRUE)
    error_step[i,j-1] <- mean((predictions$fit - test$wage)**2)
  }
}
rm(test, train, age, i, j, model, predictions)

#Calculating the mean error for every polynomial degree
mean <- c()
for (i in c(1:9)) {
  mean[i] <- mean(error_step[,i])
}
rm(i)

#Attaching the mean of MSE to the matrix of error
error_step <- rbind(error_step, mean)

#Finding the minimum MSE
which.min(mean)

"We find that the minimum error is obtained after only dividing the plot
into 2 regions. Unsurprisingly the minimum mean for the step function is
higher than the mean from the polynomial regression."

#Plotting the MSE
plot(mean)
rm(mean)

#Plotting the 1 cut step function
ggplot(data = Wage, aes(x = age, y = wage)) +
  geom_point(shape = 2, color = "grey") +
  geom_smooth(formula = y ~ cut(x,2), se = TRUE,
              color = "blue", fill = "red") +
  theme_bw()