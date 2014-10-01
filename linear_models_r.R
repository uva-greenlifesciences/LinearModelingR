# Linear Modeling in R
# Clay Ford
# UVA StatLab
# Fall 2014

# Tips:
# 1. Ctrl + Enter to submit commands
# 2. Use Tab to complete a command

# packages used in this workshop
install.packages("car")
install.packages("arm")
install.packages("visreg")
install.packages("leaps")
install.packages("ggplot2")
install.packages("boot")


# simple linear regression ------------------------------------------------

# let's use a data set that comes with R: cars
# Speed and Stopping Distances of Cars

# investigate data
str(cars)
summary(cars)

# scatter plot
plot(dist ~ speed, data=cars)

# scatterplot from the car package
library(car)
scatterplot(dist ~ speed, data=cars)

# distribution of response
hist(cars$dist)

# regress distance on speed using lm() function
cars.lm <- lm(dist ~ speed, data=cars)
summary(cars.lm)

# Interpretation: every 1 mph increase in speed leads to about 4 additional feet
# in stopping distance. What about the intercept?

# Extractor functions:
# model coefficients (ie, the betas)
coef(cars.lm)

# fitted value (ie, what the model predicts)
fitted(cars.lm)

# see anything funny with the first two?

# residuals (ie, difference between observed and predicted values)
resid(cars.lm)

# plot fitted regression line
plot(dist ~ speed, data=cars)
abline(cars.lm)
# note: abline() only works for simple linear regression

# can also use scatterplot
scatterplot(dist ~ speed, data=cars, smooth = FALSE, boxplots = FALSE)

# multiple linear regression ----------------------------------------------

# prostate cancer data (from Applied Linear Statistical Models, 5th ed)
# A study on 97 men with prostate cancer who were due to receive a 
# radical prostatectomy.

# psa - prostate specific antigen
# volume - cancer volume
# weight - prostate weight
# age - age of patient
# bph - benign prostatic hyperplasia amount
# svi - seminal vesicle invasion
# cap.pen - capsular penetration
# gleason.score - Gleason score

# can we model PSA as a linear function of other variables?
pros <- read.csv("data/prostate.csv")
summary(pros)
names(pros)
str(pros)
pairs(pros)

# scatterplotMatrix from the car package
scatterplotMatrix(pros)
scatterplotMatrix(pros[,c(1:4)])

# some observations:
# svi appears to be a factor with levels of 0 and 1
# gleason score is discrete
# weight has an outlier (data entry error?)
summary(pros$weight)
# bph has a lot of 0's
summary(pros$bph)
hist(pros$bph)

# check distribution of PSA, our response
hist(pros$psa) # skewed right
# recall assumption that response is Normally distributed;
# a log transformation may fix this:
hist(log(pros$psa))

# add log transformed psa to data frame:
pros$logpsa <- log(pros$psa)

# drop psa from data frame
pros$psa <- NULL

# volume, weight, bph, cap.pen might benefit from log transformation.

# fit model using all variables
# the plus (+) sign means "include" in model
m1 <- lm(logpsa ~ volume + weight + age + bph + svi + 
           cap.pen + gleason.score, data=pros)
summary(m1)
# Note: the response is log transformed, so interpretation of coefficients
# changes. To interpret volume coefficient, exponentiate: exp(0.07) = 1.07, so
# an increase in one unit of volume corresponds to about a 7% increase in PSA.

# use extractor functions to see model results:
coef(m1)
fitted(m1) # on log sacle
exp(fitted(m1)) # transformed back to original scale

# what about standard errors, R-squared? How to extract those?
# save the summary
m1s <- summary(m1)
names(m1s)
m1s$coefficients
m1s$r.squared
m1s$sigma # residual standard error


# confidence intervals ----------------------------------------------------


# confidence intervals for coefficients
confint(m1) # 95%
confint(m1, level=0.90)
confint(m1,"volume")

# plot of 1 and 2 SE confidence intervals of coefficients
library(arm)
coefplot(m1)
# include intercept and draw a box around the plot
coefplot(m1, frame.plot=TRUE)

# confidence intervals for predictions

# first need to generate new data.
# the predict() function requires that new data be in data frame.
# here we create a data frame with one row (ie, one person)
pros.new <- with(pros, data.frame(volume=median(volume),
                                  weight=median(weight),
                                  age=mean(age), 
                                  bph=median(bph),
                                  svi=0, 
                                  cap.pen=median(cap.pen),
                                  gleason.score=6))
pros.new

# predict logpsa using "new" data:
# predicted mean with interval
pred.mean <- predict(m1, newdata=pros.new, interval = "confidence")
pred.mean
exp(pred.mean)
# predicted value with interval
pred.val <- predict(m1, newdata=pros.new, interval = "prediction")
pred.val
exp(pred.val)

# In two-dimensions (1 predictor), we can plot these as confidence bands
plot(dist ~ speed, data=cars)
# visreg package makes this easy:
library(visreg)
visreg(cars.lm)


# model formula and specifications ----------------------------------------

# recall previous model
m1$call 
m1
# this fits the same model:
# drop psa and fit everything else in the data frame
m1 <- lm(logpsa ~ ., data=pros)
m1

# model with volume and weight and their interaction
m2 <- lm(logpsa ~ volume + weight + volume:weight, data=pros)
summary(m2)
# same thing
m2 <- lm(logpsa ~ volume*weight, data=pros)
summary(m2)

# model with 3 predictors and all 2-way interactions
m3 <- lm(logpsa ~ (volume + weight + age)^2, data=pros)
summary(m3)

# polynomial regression: need to use use I()
m4 <- lm(logpsa ~ volume + I(volume^2), data=pros)
summary(m4)

# plot polynomial regression
visreg(m4)


# factors and contrasts ---------------------------------------------------

# Gleason score and svi could be treated as factors instead of numbers
pros$gleason.score
summary(pros$gleason.score)
pros$svi
summary(pros$svi)
# let's change gleason.score and svi to factors (categorical variables)
pros$gleason.score <- factor(pros$gleason.score)
pros$svi <- factor(pros$svi)

summary(pros$gleason.score) 
summary(pros$svi)

# Note: when a variable is all zeros and ones, making it a factor won't change
# its model coefficient. 

# summary stats
# mean psa for each gleason.score group and boxplots
aggregate(logpsa ~ gleason.score, pros, mean)
boxplot(logpsa ~ gleason.score, pros)
# mean psa for each svi group and boxplots
aggregate(logpsa ~ svi, pros, mean)
boxplot(logpsa ~ svi, pros)

# verify gleason.score is factor
class(pros$gleason.score)
is.factor(pros$gleason.score)

# linear models with gleason.score (aka, ANOVA)
fm1 <- lm(logpsa ~ gleason.score, pros)
summary(fm1)
anova(fm1)


# linear model where gleason.score interacted with svi (factor:factor)

# plot interaction of factors:
boxplot(logpsa ~ gleason.score * svi, data=pros)
interaction.plot(x.factor = pros$gleason.score,
                 trace.factor = pros$svi,
                 response = pros$logpsa)
# does not appear to be evidence of interaction

# means of interacted factors
aggregate(logpsa ~ gleason.score * svi, data=pros, mean)
# counts of interacted factors
table(pros$gleason.score, pros$svi)

# fit linear model with gleason score and svi (aka, 2-factor ANOVA)
fm3 <- lm(logpsa ~ gleason.score * svi, pros)
summary(fm3)
anova(fm3)

# linear model where gleason.score interacted with volume (factor:numeric)

# plot interaction of factor and numeric:
scatterplot(logpsa ~ volume | gleason.score, data=pros, smooth=F)
# conditioning plot
coplot(logpsa ~ volume | gleason.score, data=pros, rows = 1)
# conditioning plot with smooth line
coplot(logpsa ~ volume | gleason.score, data=pros, rows = 1, panel=panel.smooth)

# now fit the model (difference in slopes for each level of gleason.score?)
fm4 <- lm(logpsa ~ gleason.score*volume, pros)
summary(fm4)
# slope not 0, but no significant difference in slopes relationship between
# logpsa and volume doesn't appear to change for different levels of gleason
# score.

# confidence interval for volume
confint(fm4,"volume")

# plot results
visreg(fm4, xvar = "volume", by = "gleason.score")


# regression diagnostics --------------------------------------------------

# let's go back to model m1 (all explanatory variables numeric)
# visual diagnostics
plot(m1)

# make all plots fit in one graphic
par(mfrow=c(2,2)) 
plot(m1)
par(mfrow=c(1,1)) # restore to previous setting

# what's up with observation 32?
pros[32,]
# very high weight

# check independence assumption (or try to anyway since we don't have time variables)
# plot residuals against all variables and look for patterns
plot(residuals(m1) ~ ., data=pros)


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


# Model Selection ---------------------------------------------------------

# let's fit a new model
pros.lm <- lm(logpsa ~ weight + volume + svi + bph + age, data=pros) 

# updating models
# original model with all variables and observations
summary(pros.lm)

# create new model without weight and age
pros.lm2 <- update(pros.lm, . ~ . - weight - age)
summary(pros.lm2)

# compare models using anova()
# syntax: anova(smaller model, larger model)
anova(pros.lm2,pros.lm)

# Result: fail to reject null; smaller model fits just as well as larger model


# Using AIC to select a model
step.out <- step(pros.lm)
step.out
summary(step.out) # final model
# same as what we selected using anova()

step.out$anova # log of selection
extractAIC(pros.lm)
extractAIC(update(pros.lm,.~.-weight -age))

# check diagnostics
par(mfrow=c(2,2))
plot(pros.lm2)
# check for influential values
summary(influence.measures(pros.lm2))

# obs 94 looks interesting...
# fit model without obs 94
pros.lm2a <- update(pros.lm2, subset= -94) 
summary(pros.lm2a)
plot(pros.lm2a)
par(mfrow=c(1,1))

# Better? Worth it? What do we think? Judgment call.

# returning to updated model with all observations:
summary(pros.lm2)
# lots of stars, but examine effect size.
# practically or just statistically significant?
# use judgment. 



# Logistic Regression -----------------------------------------------------

# low birth weight data (from Applied Logistic Regression, 2nd ed.)
# Selected variables:
# LOW = birth weight < 2500 g (1 = yes, 0 = no)
# SMOKE = smoking status during pregnancy
# AGE = age of mother in years
# RACE = race of mother (1 = white, 2 = black, 3 = other)
# LWT = weight of mother in pounds

lowbirth <- read.csv("data/lowbirth.csv")
# define RACE and SMOKE as a factor (ie, a categorical variable)
lowbirth$RACE <- factor(lowbirth$RACE)
lowbirth$SMOKE <- factor(lowbirth$SMOKE)

# Is low birth weight related to the factors above?

# fit logistic regression using glm(); note family=binomial
lb.glm <- glm(LOW ~ AGE + SMOKE + RACE + LWT, data=lowbirth, family=binomial)
summary(lb.glm)

# predicted probabilities
fitted(lb.glm) 

# fit logistic model without AGE
lb2.glm <- update(lb.glm, . ~ . - AGE)
summary(lb2.glm)

# compare models
anova(lb2.glm, lb.glm, test = "Chisq")

# Result: smaller model fits just as well as larger model

# Interpretation of coefficients
coef(lb2.glm) # log odds
exp(coef(lb2.glm)) # odds ratios

# visualization
visreg(lb2.glm, scale="response")
visreg(lb2.glm, "LWT", by="RACE", scale="response")
visreg(lb2.glm, "LWT", by="SMOKE", scale="response")


# Bonus material ----------------------------------------------------------

# simple linear regression using simulated data

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

# A Note about missing data:
# Missing data means the case is dropped from the analysis.
# set 5th value of x to NA ("Not Available")
x[5] <- NA
summary(lm(y ~ x))
# note the message: "1 observation deleted due to missingness"

# performing matrix calculations in R

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


# model validation

# fit a model
lm.all <- lm(logpsa ~ volume + bph + svi + gleason.score, data=pros)
summary(lm.all)

# calculate MSE:
mean(residuals(lm.all)^2) # training error estimate; too optimistic
sqrt(mean(residuals(lm.all)^2)) # return to original units


# divide data into two sets: training and test
# build model with training;
# predict response with test data;
# do 1000 times

mse <- numeric(1000)
for(i in 1:1000){
  # randomly sample row numbers
  train <- sample(nrow(pros), round(0.5*nrow(pros)))
  # build model using only "train" data
  lm.train <- lm(logpsa ~ volume + bph + svi + gleason.score, data=pros, 
                 subset=train)
  # test error estimate
  mse[i] <- mean((pros$logpsa - predict(lm.train, pros))[-train]^2)
}
mean(mse)
sqrt(mean(mse))
hist(mse)
summary(mse)
IQR(mse)
quantile(mse,probs = c(0.025,0.975))

sqrt(mean(mse))

# cross validation

# fit model using glm() and then use cv.glm() function in boot package

library(boot) # for cv.glm()
glm.all <-  glm(logpsa ~ volume + bph + svi + gleason.score, data=pros)
summary(glm.all)
# do 5-fold cross validation
cv.mse <- cv.glm(pros, glm.all, K=5)$delta # the first number is mean MSE
cv.mse
sqrt(cv.mse[1])


# Model Selection by best subset, forward and backward selection
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


# END

