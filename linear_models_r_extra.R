# Linear Modeling in R
# Extra material

# material I had to edit out for time


# uses the following packages:
install.packages("leaps")
install.packages("boot")
install.packages("stargazer")
install.packages("coefplot")
install.packages("broom")
install.packages("ggplot2")

# We'll load them as we need them so you'll know which functions go with which
# package.


# reload the data and model
pros <- read.csv("http://people.virginia.edu/~jcf2d/workshops/lmr/prostate.csv")
pros$logpsa <- log(pros$psa)
m1 <- lm(logpsa ~ volume + weight + age + bph + svi + 
           cap.pen + gleason.score, data=pros)



# simple linear regression using simulated data ---------------------------


# generate some data
x <- seq(1,10,length.out = 100)
set.seed(1) # so we all get same random numbers
# y = 3 + 2*x + error
y <- 3 + 2*x + rnorm(n=100,mean=0,sd=2)
plot(x,y, xlab="predictor", ylab="response")

# We know the process that gave rise to this data:
# y = 3 + 2*x + error
# beta_0 = 3 (intercept)
# beta_1 = 2 (slope)
# error = value from Normal dist'n with mean 0 and SD 2.

# But in real life we don't know this information,
# so we work backwards to figure it out:

# fit the model and save to "slr"
# Note: intercept assumed
slr <- lm(y ~ x) 
slr
# view model summary
summary(slr)
# plot fitted line (can only do for simple linear regression)
abline(slr,col="red")
# plot original line with intercept 3 and slope 2
abline(a = 3,b = 2,col="blue")


# Using broom with ggplot2 ------------------------------------------------

# Broom converts statistical analysis objects into Tidy Data Frames. This can be
# useful if you want to use model output with ggplot2, or do more analyses with 
# your model output. The broom package also works with other kinds of models, 
# such as mixed-effects, cox proportional hazards, quantile regression, and so
# on.

library(broom)

# tidy the summary output of a linear model into a data frame
tidy(m1)

# Or augment the original data with fitted values, SE of fitted values, residuals,
# and others. 
head(augment(m1))

# we can save this "tidy" data into a data frame and do further work with
# it in ggplot2:

library(ggplot2)

tout <- tidy(m1)
aout <- augment(m1)
names(aout)

# Create Scale - Location plot using aout
ggplot(aout, aes(x = .fitted, y = sqrt(abs(.std.resid)))) +  
  geom_point() +
  geom_smooth(se=F)

# compare to base R "Scale - Location" plot
plot(m1, which = 3)

# see ?lm_tidiers for how to reproduce the other base R diagnostic plots.


# Another example: create a coefficient plot to visualize magnitude and
# uncertainty of coefficients using tout.

ggplot(tout, aes(x= estimate, y = term, 
                 xmin = estimate - std.error, 
                 xmax = estimate + std.error)) +
  geom_point() +
  geom_vline(xintercept = 0) +
  geom_errorbarh(height = 0)

# leave out the intercept
ggplot(tout[-1,], aes(x= estimate, y = term, 
                      xmin = estimate - std.error, 
                      xmax = estimate + std.error)) +
  geom_point() +
  geom_vline(xintercept = 0) +
  geom_errorbarh(height = 0)


# see also the coefplot section next!


# Example of coefplot -----------------------------------------------------

# The coefplot package creates coefplots using ggplot2

library(coefplot)
coefplot(m1)
coefplot(m1, intercept = FALSE)

# show two models plotted in the same graph
m2 <- update(m1, . ~ . - age - weight - cap.pen, pros)
multiplot(m1, m2, intercept = FALSE)


# performing matrix calculations in R -------------------------------------



# Recall the matrix notation: Y = XB + e
# we can extract those matrices and vectors in R:
model.matrix(m1)
X <- model.matrix(m1)
coef(m1)
B <- coef(m1)
residuals(m1)
e <- residuals(m1)

# confirm the linear model formula:
# Note: %*% means matrix multiplication
X %*% B + e
Y <- X%*%B + e
# compare to original values
cbind(pros$logpsa,Y)



# More on influential values ----------------------------------------------

# The influence.measures() function helps us indentify influential values.

# check for influential values
im.out <- influence.measures(m1)
im.out
summary(im.out)
# observation 32 looks particularly influential in log transformed model

# re-run model without obs 32
m1b <- update(m1, subset = -32)
summary(m1b)
plot(m1b)

# check for collinearity
# use vif() from the car package
vif(m1)

# guide for interpreting VIF

# VIF = 1 	Not correlated
# 1 < VIF < 5 	Moderately correlated
# VIF > 5 to 10 	Highly correlated

# Everything looks good here.



# model validation --------------------------------------------------------


# fit a model
lm.all <- lm(logpsa ~ volume + bph + svi + gleason.score, data=pros)
summary(lm.all)

# calculate MSE (mean square error):
mean(residuals(lm.all)^2) 
# return to original units
sqrt(mean(residuals(lm.all)^2)) 

# it's too optimistic since it's based on same data to fit model


# divide data into two sets: training and test
# build model with training;
# predict response with test data;
# do 1000 times
# estimate MSE

mse <- numeric(1000) # empty vector to store MSE
for(i in 1:1000){
  # randomly sample row numbers
  train <- sample(nrow(pros), round(0.5*nrow(pros)))
  # build model using only "train" data
  lm.train <- lm(logpsa ~ volume + bph + svi + gleason.score, data=pros, 
                 subset=train)
  # test error estimate by using test data to predict
  mse[i] <- mean((pros$logpsa - predict(lm.train, pros))[-train]^2)
}
mean(mse)
sqrt(mean(mse))
hist(mse)
summary(mse)
IQR(mse)
quantile(mse,probs = c(0.025,0.975))

# higher than original MSE:
sqrt(mean(mse))

# compare to original:
sqrt(mean(residuals(lm.all)^2))

# This is a more releastic measure of model performance

# cross validation

# fit model using glm() and then use cv.glm() function in boot package

library(boot) # for cv.glm()
glm.all <-  glm(logpsa ~ volume + bph + svi + gleason.score, data=pros)
summary(glm.all)

# do 5-fold cross validation

# divide data into 5 sets, hold out 1 set, fit model with other 4 sets, measure
# MSE. Repeat with other 4 sets being held out.

cv.mse <- cv.glm(pros, glm.all, K=5)$delta 
cv.mse # the first number is mean MSE
sqrt(cv.mse[1])



# More on AIC -------------------------------------------------------------


# see AIC without using step function:
# second number is AIC; first is "equivalent degrees of freedom"
extractAIC(m1)
extractAIC(update(m1,.~.-weight -age))


# Model Selection by best subset, forward and backward selection ----------

library(leaps)

# best subset
regfit.sub <- regsubsets(logpsa ~ ., data=pros)
summary(regfit.sub)
# bottom line indicates selected variables
plot(regfit.sub, main="best subset") 
# top line indicates selected variables

# forward selection
regfit.fwd <- regsubsets(logpsa ~ ., data=pros, method="forward")
summary(regfit.fwd)
plot(regfit.fwd, main="forward selection")

# backward selection
regfit.bwd <- regsubsets(logpsa ~ ., data=pros, method="backward")
summary(regfit.bwd)
plot(regfit.bwd, main="backward selection")

# all select the same "best" model.

# extract coefficients for best model:
# first identify model with lowest BIC
which.min(summary(regfit.sub)$bic)
which.min(summary(regfit.fwd)$bic)
which.min(summary(regfit.bwd)$bic)
# now use the model ID to extract coefficients of best model
coef(regfit.sub, id=4)
coef(regfit.fwd, id=4)
coef(regfit.bwd, id=4)


# one way to check fit:
# a good fit will lie close to line with slope 1 and intercept 0
plot(pros$logpsa, fitted(pros.lm2),
     xlim=c(0,5),ylim=c(0,5),
     xlab="Observed",ylab="Fitted")
abline(a=0,b=1,lty=2)
# biased predictions for values < 2 and > 4


# Example of Stargazer ----------------------------------------------------

# Stargazer allows you to format model output for LaTeX, HTML, or text.

library(stargazer)
m1 <- lm(logpsa ~ volume + weight + age, data = pros)
m2 <- lm(logpsa ~ volume + weight, data = pros)

# compare models via text output in the console
stargazer(m1, m2, type = "text")

# produce LaTeX code
stargazer(m1, m2)




# Logistic Regression -----------------------------------------------------

# low birth weight data (from Applied Logistic Regression, 2nd ed.)
# Selected variables:
# LOW = birth weight < 2500 g (1 = yes, 0 = no)
# SMOKE = smoking status during pregnancy
# AGE = age of mother in years
# RACE = race of mother (1 = white, 2 = black, 3 = other)
# LWT = weight of mother in pounds

# lowbirth <- read.csv("data/lowbirth.csv")
lowbirth <- read.csv("http://people.virginia.edu/~jcf2d/workshops/lmr/lowbirth.csv")
# define RACE and SMOKE as a factor (ie, a categorical variable)
lowbirth$RACE <- factor(lowbirth$RACE)
lowbirth$SMOKE <- factor(lowbirth$SMOKE)

# Is low birth weight related to the factors above?

# investigate data
summary(lowbirth)

# investigate relationships
with(lowbirth,table(RACE, LOW))
with(lowbirth,table(SMOKE, LOW))
with(lowbirth,table(RACE, LOW, SMOKE))
boxplot(LWT ~ LOW, data=lowbirth)
boxplot(AGE ~ LOW, data=lowbirth)

# fit logistic regression using glm(); note family=binomial
lb.glm <- glm(LOW ~ AGE + SMOKE + RACE + LWT, data=lowbirth, 
              family=binomial)
summary(lb.glm)

# fit logistic model without AGE
lb2.glm <- update(lb.glm, . ~ . - AGE)
summary(lb2.glm)

# compare models - hypothesis test
anova(lb2.glm, lb.glm, test = "Chisq")
# can also search for a model with step()
step(lb.glm)

# Result: smaller model fits just as well as larger model

# Interpretation of coefficients
coef(lb2.glm) # log odds
exp(coef(lb2.glm)) # odds ratios
exp(confint(lb2.glm)) # CI for odds ratios

# visualization of probabilities with visreg

# By default, conditional plots in visreg are constructed by filling in other 
# explanatory variables with the median (for numeric variables) or most common 
# category (for factors)

# median LWT = 121; most common SMOKE = 0; most common RACE = 1
par(mfrow=c(2,2))
visreg(lb2.glm, scale="response")
par(mfrow=c(1,1))

# change in probabilities over LWT by RACE
visreg(lb2.glm, "LWT", by="RACE", scale="response",
       main="SMOKE = 0") # SMOKE = 0
visreg(lb2.glm, "LWT", by="RACE", cond=list(SMOKE=1), scale="response",
       main="SMOKE = 1") # SMOKE = 1

# change in probabilities over LWT by SMOKE
visreg(lb2.glm, "LWT", by="SMOKE", scale="response", 
       main="RACE = white") # RACE = 1
visreg(lb2.glm, "LWT", by="SMOKE", cond=list(RACE=2), scale="response",
       main="RACE = black") # RACE = 2
visreg(lb2.glm, "LWT", by="SMOKE", cond=list(RACE=3), scale="response",
       main="RACE = other") # RACE = 3



# demonstrate odds ratio in logistic regression
# LWT odds ratio = 0.98
exp(coef(lb2.glm)[5]) 

# calculate odds ratio by hand
# predicted probs at LWT=130 and 131
p1 <- predict(lb2.glm, newdata = data.frame(SMOKE=factor(1), RACE=factor(1), LWT=130), 
              type = "response")
p2 <- predict(lb2.glm, newdata = data.frame(SMOKE=factor(1), RACE=factor(1), LWT=131), 
              type = "response")
# calculate odds
odds1 <- (p1/(1-p1))
odds2 <- (p2/(1-p2))

# odds ratio
odds2/odds1
exp(coef(lb2.glm)[5])

# odds decreases by about 1.3% for each 1 pound increase in LWT
(odds2-odds1)/odds1

