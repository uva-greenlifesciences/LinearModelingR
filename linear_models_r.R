# Linear Modeling in R
# Clay Ford
# UVA StatLab
# October 2014

# Tips:
# 1. Ctrl + Enter to submit commands
# 2. Use Tab to complete a command

# packages used in this workshop
# Only issue these commands if you haven't already installed these packages
install.packages("car")
install.packages("visreg")

# issue these commands to have access to their functions:
library(car)
library(visreg)

# simple linear regression ------------------------------------------------

# let's use a data set that comes with R: cars
# Speed and Stopping Distances of Cars

# investigate data
str(cars)
summary(cars)

# scatterplot
plot(dist ~ speed, data=cars)

# scatterplot() from the car package
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
# pros <- read.csv("prostate.csv")
pros <- read.csv("http://people.virginia.edu/~jcf2d/workshops/lmr/prostate.csv")
summary(pros)
str(pros)
pairs(pros)

# scatterplotMatrix() from the car package
scatterplotMatrix(pros)
# look at just first 4 columns of data
scatterplotMatrix(pros[,c(1:4)])

# some observations:
# svi appears to be a factor with levels of 0 and 1
# gleason score is discrete
# weight has an outlier (data entry error?)
summary(pros$weight)
# bph has a lot of 0's
summary(pros$bph)

# check distribution of PSA, our response
hist(pros$psa) # skewed right
# recall assumption that response is Normally distributed;
# a log transformation may fix this:
hist(log(pros$psa))

# add log transformed psa to data frame:
pros$logpsa <- log(pros$psa)

# drop psa from data frame
pros$psa <- NULL

# volume, weight, bph, cap.pen might benefit from log transformation,
# but we won't pursue further.

# fit model using all variables
# the plus (+) sign means "include" in model
m1 <- lm(logpsa ~ volume + weight + age + bph + svi + 
           cap.pen + gleason.score, data=pros)
summary(m1)
# Note: the response is log transformed, so interpretation of coefficients
# changes. To interpret volume coefficient, exponentiate: exp(0.07) = 1.07, so
# an increase in one unit of volume corresponds to about a 7% increase in PSA,
# assuming all other predictors are held constant.

# use extractor functions to see model results:
coef(m1)
fitted(m1) # on log scale
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
predict(m1, newdata=pros.new, interval = "confidence")

# predicted value with interval
predict(m1, newdata=pros.new, interval = "prediction")


# In two-dimensions (1 predictor), we can plot the confidence bands
plot(dist ~ speed, data=cars)
# visreg() from the visreg package makes this easy:
visreg(cars.lm)
# change appearance (cex = character expansion; pch=plotting character)
visreg(cars.lm, points=list(cex=1, pch=1), line=list(col="black"))


# model formula and specifications ----------------------------------------

# recall previous model
m1
# this fits the same model:
# drop psa and fit everything else in the data frame
m1 <- lm(logpsa ~ ., data=pros)
m1

# model with volume and weight and their interaction
m2 <- lm(logpsa ~ volume + weight + volume:weight, data=pros)
summary(m2)
# same thing
m2 <- lm(logpsa ~ volume * weight, data=pros)
summary(m2)

# polynomial regression: need to use use I()
m4 <- lm(logpsa ~ volume + I(volume^2), data=pros)
summary(m4)
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

# verify gleason.score is factor
class(pros$gleason.score)
is.factor(pros$gleason.score)

# Note: when a variable is all zeros and ones, making it a factor won't change
# its model coefficient. 

# summary stats
# mean psa for each gleason.score group and boxplots
aggregate(logpsa ~ gleason.score, pros, mean)
boxplot(logpsa ~ gleason.score, pros)
# mean psa for each svi group and boxplots
aggregate(logpsa ~ svi, pros, mean)
boxplot(logpsa ~ svi, pros)

# plot effects of factors
plot.design(logpsa ~ gleason.score + svi, data = pros)

# linear models with gleason.score (aka, ANOVA)
fm1 <- lm(logpsa ~ gleason.score, pros)
summary(fm1)
coef(fm1)
anova(fm1)

# one-way ANOVA with gleason.score
aov1 <- aov(logpsa ~ gleason.score, pros)
summary(aov1) # same as anova(fm1)
coef(aov1) # same as coef(lm1)
TukeyHSD(aov1) # multiple comparisons of means
plot(TukeyHSD(aov1))

# linear model where gleason.score interacted with svi (factor:factor)
# exploratory plots and summaries
boxplot(logpsa ~ gleason.score * svi, data=pros)

# plot interaction of factors:
interaction.plot(x.factor = pros$gleason.score,
                 trace.factor = pros$svi,
                 response = pros$logpsa)
# does not appear to be evidence of interaction

# means of interacted factors
aggregate(logpsa ~ gleason.score * svi, data=pros, mean)
# counts of interacted factors; table(row,column)
table(pros$gleason.score, pros$svi)

# fit linear model with gleason score and svi (aka, 2-factor ANOVA)
fm3 <- lm(logpsa ~ gleason.score * svi, pros)
summary(fm3)
anova(fm3)

# linear model where gleason.score interacted with volume (factor:numeric)
# (aka, Analysis of Covariance - ANCOVA)

# plot interaction of factor and numeric:
scatterplot(logpsa ~ volume | gleason.score, data=pros, smooth=F)

# let's log transform volume:
scatterplot(logpsa ~ log(volume) | gleason.score, data=pros, smooth=F)

# now fit the model
# Is there a difference in slopes for each level of gleason.score?
fm4 <- lm(logpsa ~ gleason.score*log(volume), pros)
summary(fm4)
# slope not 0, but the relationship between logpsa and volume doesn't appear to
# change for different levels of gleason score.


# regression diagnostics --------------------------------------------------

# let's go back to model m1 (all explanatory variables numeric)
# visual diagnostics
plot(m1)

# make all plots fit in one graphic
par(mfrow=c(2,2)) 
plot(m1)
par(mfrow=c(1,1)) # restore to previous setting

# observation 32 appears to be very influential
pros[32,]
# very high weight

# check independence assumption
# plot residuals against predictor variables and look for patterns;
# there should be no pattern.
# don't plot residuals against your response;
# Example
plot(residuals(m1) ~ volume , data=pros)
plot(residuals(m1) ~ weight , data=pros)


# Model Selection ---------------------------------------------------------

# let's fit a new model with svi and gleason.score as factors
m1 <- lm(logpsa ~ ., data=pros) 
summary(m1)
plot(m1) 

# 32 very influential

# update model without obs 32 using update() function
m2 <- update(m1, subset = -32)
summary(m2)
plot(m2)

summary(m1)
summary(m2)

# let's fit a new model
pros.lm <- lm(logpsa ~ weight + volume + svi + bph + age, data=pros) 
summary(pros.lm)
# weight and age don't seem to add anything

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

# let's check diagnostics of original model
par(mfrow=c(2,2))
plot(pros.lm)

# obs 32 is very influential
# fit model without obs 32
pros.lm1 <- update(pros.lm, subset= -32) 
summary(pros.lm1)
# now weight is signficant at 0.05 level and bph marginally significant 
# at 0.10 level
plot(pros.lm1)
par(mfrow=c(1,1))

# model selection with obs 32 removed
step(pros.lm1)

# AIC selects a different model based on 1 observation being removed.

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
lb.glm <- glm(LOW ~ AGE + SMOKE + RACE + LWT, data=lowbirth, family=binomial)
summary(lb.glm)

# predicted probabilities
fitted(lb.glm) 

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


# Bonus material ----------------------------------------------------------

# material I had to edit out for time
# this material makes use of the following packages:
install.packages("leaps")
install.packages("boot")

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

# influential values

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


# Using AIC to select a model
step.out <- step(pros.lm)
step.out
summary(step.out) # final model
# same as what we selected using anova()

step.out$anova # log of selection

# see AIC without using step function:
extractAIC(pros.lm)
extractAIC(update(pros.lm,.~.-weight -age))

# check diagnostics
par(mfrow=c(2,2))
plot(pros.lm2)

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

# wait a minute...
# let's check diagnostics of original model
par(mfrow=c(2,2))
plot(pros.lm)


# obs 32 is very influential
# fit model without obs 32
pros.lm1 <- update(pros.lm, subset= -32) 
summary(pros.lm1)
# now weight is signficant at 0.05 level and bph marginally significant 
# at 0.10 level
plot(pros.lm1)
par(mfrow=c(1,1))

# model selection with obs 32 removed
step(pros.lm1)

# AIC selects a different model based on 1 observation being removed.


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

