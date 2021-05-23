#Loading packages
library(gam)
library(ISLR)

#A look on the dataset
names(Wage)
dim(Wage)

# Building a GAM to predict wage using natural spline functions of year and age,
# treating education as a qualitative predictor.

wage_age_gam = lm(wage ~ ns(year,4)+ns(age,5)+education, data = Wage)

"In order to fit GAMs using smoothing splines or other components that cannot be
expressed in terms of basis functions and then fit the model using OLS, we need
to use the gam library in R."

# Building a GAM with Smoothing Splines Regression as the components
wage_age_gam_ss = gam(wage ~ s(year,4)+s(age,5)+education, data = Wage)

"The s() function is used to indicate a smoothing spline. We can specify the
degrees of freedom for each smoothing spline. Here we use 4 degrees of freedom
for year and 5 degrees of freedom for age."

#Plotting the resulting GAM
dev.new()
par(mfrow=c(1,3))
plot(wage_age_gam_ss, se=TRUE ,col = "blue")

"We can see that as time goes by, people are getting higher and higher wages.
Maybe this is due to inflation or maybe companies are giving more compensation
for their workers.

Second, the people with the highest wages are those who are 40-60 years old.
This is very intuitive as people are getting promoted to hold a strategic
position at their companies at around those ages. And then wage goes down as
some people are getting retired.

We can also see that the higher the education, the higher someone's wages."

class(wage_age_gam)
class(wage_age_gam_ss)

"The plot() function recognizes that wage_age_gam_ss is an object of class gam
and invokes the appropriate plot.gam() method. Conveniently, even though
class(wage_age_gam) is not of class gam but rather of class lm, we can still
use plot.gam() on it."

dev.new()
par(mfrow=c(1,3))
plot.Gam(wage_age_gam_ss, se = TRUE, color = "red")

# In these plots, the function of year looks rather linear. We can perform a
# series of ANOVA tests in order to determine which of these three models is
# best: a GAM that excludes year (M1), a GAM that uses a linear function
# of year (M2), or a GAM that uses a spline function of year (M3).

gam.m1 = gam(wage ~ s(age,5)+education, data = Wage)
gam.m2 = gam(wage ~ year+s(age,5)+education,data = Wage)
anova(gam.m1,gam.m2,wage_age_gam_ss,test="F")

"We find that a GAM with a linear function of year is better than a GAM without
the year variable (p-value = 0.00014). However, there is no evidence that a
smoothing splines regression of year is needed (p-value = 0.349). Therefore
we should use the model with the linear function of year"

summary(wage_age_gam_ss)

"The p-values for year and age correspond to a null hypothesis of a linear
relationship versus the alternative of a non-linear relationship. The large
p-value for year (0.3537) reinforces our conclusion from the ANOVA test that a
linear function is adequate for the year variable.

It is also important to note that there is strong evidence that a non-linear
term is required for age."

#Making predictions on the training data set with GAM
predictions = predict(gam.m2, newdata = Wage)
head(predictions)

# Building local regression model as a building block in a GAM
gam.lo = gam(wage ~ s(year,df=4)+lo(age,span =0.7)+education, data=Wage)

#Plotting the new GAM with Local Regression
dev.new()
par(mfrow = c(1,3))
plot.Gam(gam.lo, se = TRUE, color = "green")

# Creating interactions with the lo() function before calling the gam() function
gam.lo.i=gam(wage ~ lo(year, age, span = 0.5)+education, data = Wage)
               
"We fitted a two-term GAM, in which the first term is an interaction between
year and age, fit by a local regression surface. We can plot the resulting
two-dimensional surface with the akima package"

#Plotting the resulting GAM
library(akima)

dev.new()
par(mfrow = c(1,2))
plot(gam.lo.i)

# Building a Logistic Regression Model with GAM as the predictor function
gam.lr = gam(I(wage >250) ~ year+s(age,df=5)+education,
             family = binomial, data = Wage)

"The I() function returns 1 if the expression is TRUE and 0 if it is FALSE."

#Plotting the Logistic Regression GAM
dev.new()
par(mfrow=c(1,3))
plot(gam.lr,se=T,col="green ")

table(Wage$education, I(Wage$wage >250))

"Nobody earned >250K in the <HS category. This is why the confidence interval
is unusually large. We should fit a logistic regression GAM using all but this
category."

gam.lr.s=gam(I(wage >250) ~ year+s(age,df=5)+education, family = binomial,
              data = Wage, subset = (education != "1. < HS Grad"))

#Plotting the Logistic Regression GAM
dev.new()
par(mfrow = c(1,3))
plot(gam.lr.s, se = TRUE, color = "green")

"Better results."