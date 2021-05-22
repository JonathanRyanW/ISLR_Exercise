#Loading packages
library(ISLR)

#A look on the dataset
names(Wage)
dim(Wage)

#Building Local Regression Model with span 20% and 50%
wage_age_loess_20 <- loess(wage ~ age, data = Wage, span = 0.2)
wage_age_loess_50 <- loess(wage ~ age, data = Wage, span = 0.5)

#Creating the age grid for predictions
agelims = range(Wage$age)
age.grid = seq(from = agelims[1], to = agelims[2])

#Plotting the resulting Local Regression Models
plot(Wage$age, Wage$wage, xlim = agelims, cex = 0.5, col="darkgrey")
title("Local Regression ")
lines(age.grid, predict(wage_age_loess_20, data.frame(age=age.grid)),
        col="red",lwd=2)
lines(age.grid, predict(wage_age_loess_50, data.frame(age=age.grid)),
        col="blue",lwd=2)
legend ("topright", legend = c("Span=0.2"," Span=0.5"), 
        col = c("red","blue"),
        lty = 1, lwd = 2, cex = 0.8)
          
"We can see that the larger the span, the smoother the fit. The locfit library
can also be used for fitting local regression models in R."

#Calculating the mean of squared residuals (MSE)
mean(wage_age_loess_20$residuals**2) #1582.911
mean(wage_age_loess_50$residuals**2) #1586.792

"The MSE for the 20% span model is slightly smaller than the 50% span model."

residuals_df <- data.frame(age = Wage$age,
                           loess_20 = wage_age_loess_20$residuals,
                           loess_50 = wage_age_loess_50$residuals) 

library(ggplot2)
ggplot(data = residuals_df, aes(y = loess_20, x = age)) +
  geom_point(shape = 2, color = "grey") +
  theme_bw()

"No observeable pattern in the residuals for the 20% span Local Regression
Model. It looks like a null plot."

ggplot(data = residuals_df, aes(y = loess_50, x = age)) +
  geom_point(shape = 2, color = "grey") +
  theme_bw()

"The same as before, no observable systematic pattern in the residual plot for
the second model."

#Finding the components of the Local Regression Models
names(wage_age_loess_20)

"The Local Regression model does not output any estimated coefficients. This is
a memory based model. It is non parametric."