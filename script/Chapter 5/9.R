# 9. We will now consider the Boston housing data set from the MASS library.
# (a) Based on this data set, provide an estimate for the population
# mean of medv. Call this estimate ˆμ.

library(MASS)
dim(Boston)
names(Boston)

mean(Boston$medv)

"The estimate is 22.53281"

# (b) Provide an estimate of the standard error of ˆμ. Interpret this
# result.

# Hint: We can compute the standard error of the sample mean by
# dividing the sample standard deviation by the square root of the
# number of observations.

sd(Boston$medv)/506

?Boston

"The estimated SD is 0.0181761. medv is the median value of owner-occupied
homes in thousands of dollars. This means that the average value is $22,532.
The SD is $181 which means that if we repeatedly take sample from the population
and then calculate the mean of medv, those means are estimated to have the SD of
$181."

# (c) Now estimate the standard error of ˆμ using the bootstrap. How
# does this compare to your answer from (b)?

boot.fn <- function(data, index) {
  return(mean(data$medv[index]))
}

set.seed(1001)
boot(Boston, boot.fn, R = 100)

"The SE is 0.4149978, this is higher than the sd we get before. But we get that
value buy examining only one sample, that is, all the observations."

# (d) Based on your bootstrap estimate from (c), provide a 95 % confidence
# interval for the mean of medv. Compare it to the results
# obtained using t.test(Boston$medv).

mean <- mean(Boston$medv)
lower <- mean - 1.96 * 0.4149978 #21.71941
upper <- mean + 1.96 * 0.4149978 #23.3462

t.test(Boston$medv) #[21.72953, 23.33608]

"The result is very similar"

# (e) Based on this data set, provide an estimate, ˆμmed, for the median
# value of medv in the population.

median(Boston$medv)

"The estimated median is 21.2"

# (f) We now would like to estimate the standard error of ˆμmed. Unfortunately,
# there is no simple formula for computing the standard error of the median.
# Instead, estimate the standard error of the median using the bootstrap.
# Comment on your findings.

#Creating the function that we want to measure (median)
boot.fn <- function(data, index) {
  return(median(data$medv[index]))
}

#Performing bootstrap
set.seed(1001)
boot(Boston, boot.fn, R = 100)

"The estimated SE of the median of medv is 0.3832276"

# (g) Based on this data set, provide an estimate for the tenth percentile of
# medv in Boston suburbs. (You can use the quantile() function

quantile(Boston$medv, 0.1)

"The estimate is 12.75"

# (h) Use the bootstrap to estimate the standard error of ˆμ0.1. Comment on your
# findings

boot.fn <- function(data, index) {
  return(quantile(data$medv[index], 0.1))
}

set.seed(1001)
boot(Boston, boot.fn, R = 100)

"The SE for the 0.1 quantile of medv is 0.5121698. The estimated quantile is of
course the exact same."
