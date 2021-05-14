# 8. We will now perform cross-validation on a simulated data set.
# (a) Generate a simulated data set as follows:

set.seed(1)
x = rnorm(100)
y = x - 2 * x**2 + rnorm(100)

# In this data set, what is n and what is p? Write out the model
# used to generate the data in equation form.

"n is the number of observation, 100. While p is the number of parameters to be
estimated, that is 2 (1 coef for x and another coef for x**2)"

# (b) Create a scatterplot of X against Y . Comment on what you find.
plot(x,y)

"We already know that y is a quadratic function of x. The plot certainly
reflects this fact. The graph looks like any ordinary quadratic polynomial."

# (c) Set a random seed, and then compute the LOOCV errors that result from
# fitting the following four models using least squares:
# i. Y = β0 + β1X + error
# ii. Y = β0 + β1X + β2X***2 + error
# iii. Y = β0 + β1X + β2X***2 + β3X***3 + error
# iv. Y = β0 + β1X + β2X***2 + β3X***3 + β4X***4 + error
# Note you may find it helpful to use the data.frame() function
# to create a single data set containing both X and Y.

xy <- data.frame(x,y)
rm(x,y)

set.seed(1001)

loocv.error <- c()
for (i in 1:4) {
  glm.fit <- glm(y ~ poly(x, i), data = xy)
  loocv.error[i] <- cv.glm(data = xy, glm.fit)$delta[1]
}
rm(i)

# (d) Repeat (c) using another random seed, and report your results.
# Are your results the same as what you got in (c)? Why?

set.seed(1002)

loocv.error.2 <- c()
for (i in 1:4) {
  glm.fit <- glm(y ~ poly(x, i), data = xy)
  loocv.error.2[i] <- cv.glm(data = xy, glm.fit)$delta[1]
}
rm(i)

"There is no difference at all. It happens because in the loocv method all
observation will be left out exactly once. We didn't specify k, so the cv.glm
function immeaditely assumes we want to perform loocv, not a k-fold cv.

Since every single observation will have their turn to be left out, it doesnt
really matter which observation gets left out first. the estimated error will be
the same."

# (e) Which of the models in (c) had the smallest LOOCV error? Is
# this what you expected? Explain your answer.

"The smallest error comes from the quadratic model. Of course this is expected.
We defined y as a quadratic function of x after all. Thus adding higher degree
x does not help us"

# (f) Comment on the statistical significance of the coefficient estimates that
# results from fitting each of the models in (c) using
# least squares. Do these results agree with the conclusions drawn
# based on the cross-validation results?

for (i in 1:4) {
  xy_lm <- lm(y ~ poly(x, i), data = xy)
  matrix_coef <- summary(xy_lm)$coefficients
  print(matrix_coef[,4])
}
rm(i, xy_lm, matrix_coef)

"We can see that the p-values for x and x**2 are all very low, however the 
p-values for x**3 and x**4 is large, they are not statistically significant.
I completely agree that they should not be significant, we dont see any increase
in accuracy from our model either (the errors didn't went down)"
