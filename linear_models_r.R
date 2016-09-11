# Linear Modeling in R
# Clay Ford
# UVA StatLab
# Fall 2016

# Ctrl + Enter to submit commands


# packages used in this workshop
# Only issue these commands if you haven't already installed these packages
install.packages("car")
install.packages("visreg")

# issue these commands to have access to their functions:
library(car)
library(visreg)

# simple linear regression ------------------------------------------------

# let's use a data set that comes with R: cars
# Speed and Stopping Distances of Cars in the 1920s
cars
# investigate data
str(cars)
summary(cars)

# scatterplot: y ~ x
plot(dist ~ speed, data=cars)

# scatterplot() from the car package
scatterplot(dist ~ speed, data=cars)

# distribution of response
hist(cars$dist)

# regress distance on speed using lm() function
cars.lm <- lm(dist ~ speed, data=cars)

# view summary of regression results
summary(cars.lm)

# Interpretation: every 1 mph increase in speed leads to about 4 additional feet
# in stopping distance. What about the intercept?

# save summary
sm1 <- summary(cars.lm)
names(sm1)
sm1$sigma
sm1$coefficients

# In RStudio, typing sm1$ will reveal all the elements in the object. Try it!


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

# psa - prostate specific antigen (PSA)
# volume - cancer volume
# weight - prostate weight
# age - age of patient
# bph - benign prostatic hyperplasia amount
# svi - seminal vesicle invasion
# cap.pen - capsular penetration
# gleason.score - Gleason score

# can we model PSA as a linear function of other variables?
pros <- read.csv("http://people.virginia.edu/~jcf2d/workshops/lmr/prostate.csv")
str(pros)
summary(pros)

# pairwise scatterplots
pairs(pros)

# scatterplotMatrix() from the car package
scatterplotMatrix(pros)
# look at just first 4 columns of data
scatterplotMatrix(pros[1:4])
scatterplotMatrix(pros[5:8])

# closer look at psa and volume
scatterplot(psa ~ volume, data = pros)

# identify the extreme points automatically
scatterplot(psa ~ volume, data = pros, id.n = 3)

# view those observations
pros[c(94,96,97),]

# closer look at psa and volume by gleason.score
scatterplot(psa ~ volume | gleason.score, data = pros)

# remove the smoother lines
scatterplot(psa ~ volume | gleason.score, data = pros, smoother = FALSE)

# some observations:
# psa and volume are skewed right
# svi appears to be a factor with levels of 0 and 1
# gleason score is discrete
# weight has an outlier (data entry error?)
# bph and cap.pen both have a lot of 0's

# check distribution of PSA, our response
hist(pros$psa) # skewed right
hist(pros$psa, breaks = 20) 
# a log transformation may be appropriate:
hist(log(pros$psa))

# add log transformed psa to data frame:
pros$logpsa <- log(pros$psa)

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

# save the summary
m1s <- summary(m1)
m1s$coefficients
m1s$r.squared
m1s$sigma # residual standard error

# plotting fitted values versus observered values can give us some indication of model "fit"
plot(x = pros$logpsa, y = fitted(m1))
abline(0,1)

# YOUR TURN!

# The trees data set that comes with R provides measurements of the girth,
# height and volume of timber in 31 felled black cherry trees.
str(trees)
summary(trees)

# (1) Use scatterplotMatrix to visualize the trees data.
# (2) Model Volume as a function of Height and Girth and save the model as "tree.mod" 
# (3) View the model summary. 



# back to presentation

# confidence intervals ----------------------------------------------------


# confidence intervals for coefficients
confint(m1) # 95%
confint(m1, level=0.90)
confint(m1,"volume")


# confidence intervals for predictions using new data

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
exp(predict(m1, newdata=pros.new, interval = "confidence")) # original scale

# predicted value with interval
predict(m1, newdata=pros.new, interval = "prediction")


# In two-dimensions (1 predictor), we can plot the confidence bands
plot(dist ~ speed, data=cars)
# visreg() from the visreg package makes this easy:
visreg(cars.lm)

# what happens if we try this with our prostate linear model?
visreg(m1)

# Answer: shows the value of the variable on the x-axis and the change in 
# response on the y-axis, holding all other variables constant (by default, 
# median for numeric variables and most common category for factors). The 
# plotted points are partial residuals, which can help us detect nonlinearity.

# We can specify the "constant" values we wish to use:
visreg(m1, cond = list(svi=1, 
                       gleason.score=8, 
                       volume=mean(pros$volume)))


# see upcoming workshop, Visualizing Model Effects, for more on this topic.
# (Thurs, Oct 13, 2016)

# YOUR TURN!

# (1) Find 95% confident intervals for the coefficients in the model you fit 
# earlier, tree.mod:


# (2) Use visreg to viualize the model, tree.mod:




# back to presentation!

# model formula and specifications ----------------------------------------

# recall previous model
m1
# this fits the same model:
# drop psa and fit everything else in the data frame
pros$psa <- NULL
# logpsa ~ . means fit the model using all remaining variables
m1 <- lm(logpsa ~ ., data=pros)
m1

# model with volume and weight and their interaction
m2 <- lm(logpsa ~ volume + weight + volume:weight, data=pros)
summary(m2)
# same thing using *
m2 <- lm(logpsa ~ volume * weight, data=pros)
summary(m2)

# model with 3 predictors and all 2-way interactions
m3 <- lm(logpsa ~ volume + weight + svi + volume:weight + 
           volume:svi + weight:svi, data=pros)
summary(m3)
# same thing using ^
m3 <- lm(logpsa ~ (volume + weight + svi)^2, data=pros)
summary(m3)

# polynomial regression: need to use use I()
m4 <- lm(logpsa ~ volume + I(volume^2), data=pros)
summary(m4)


# YOUR TURN!

# Fit a model using the trees data with Volume modeled as a function of Height,
# Girth and the interaction of Height and Girth.


# back to presentation.



# factors, contrasts and interactions -------------------------------------

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

# Note 1: when a variable is all zeros and ones, making it a factor won't change
# its model coefficient.

# Note 2: we could make gleason.score an "ordered" factor, to indicate 6 < 7 < 8. 
# R treats ordered factors differently from regular unordered factors when it
# comes to modeling. Instead of dummy variables it creates polynomial scores.

# summary stats
# mean psa for each gleason.score group and boxplots
aggregate(logpsa ~ gleason.score, pros, mean)
plot(logpsa ~ gleason.score, pros)
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

# Are the predictors "significant"? Do they explain variance in logpsa?
# Type I tests: test predictors in order
anova(fm3) 

# Type II tests: test predictors after all others (except interactions)
Anova(fm3) # car package function

# The interaction is reported the same in both. There appears to be no interaction.

# Since we can't directly interpret the effects of svi and gleason.score, we can
# use visreg to see how they interact:
visreg(fm3, xvar = "svi", by = "gleason.score")
visreg(fm3, xvar = "gleason.score", by = "svi")

# This supports the insignificant interaction result.

# linear model where gleason.score interacted with volume (factor:numeric)
# (aka, Analysis of Covariance - ANCOVA)

# plot interaction of factor and numeric:
scatterplot(logpsa ~ volume | gleason.score, pros, smooth=F)

# let's log transform volume:
scatterplot(logpsa ~ log(volume) | gleason.score, pros, smooth=F)

# now fit the model
# Is there a difference in slopes for each level of gleason.score?
fm4 <- lm(logpsa ~ gleason.score*log(volume), pros)
summary(fm4)
# slope not 0, but the relationship between logpsa and volume doesn't appear to
# change for different levels of gleason score.

# Again we can evaluate effects using anova() or Anova()
anova(fm4)
Anova(fm4)

# Again the interaction is reported the same in each and is not significant.

# we can visualize the model using visreg as well
visreg(fm4, xvar = "volume", by = "gleason.score")
# using the log transformation
visreg(fm4, xvar = "volume", by = "gleason.score", xtrans = log)


# YOUR TURN!

# Use visreg to visualize the interaction in the tree.mod2 model. 

# Set xvar = "Height" and by = "Girth". 

# This is a numeric:numeric interaction. How do we interpret the plot?


# back to presentation

# regression diagnostics --------------------------------------------------

# let's go back to model m1 (all explanatory variables numeric)
# visual diagnostics
plot(m1)

# make all plots fit in one graphic
op <- par(mfrow=c(2,2)) 
plot(m1)
par(op) # restore to previous setting

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

# observations appear indepdendent, but weight clearly has a large influential
# observation.

# YOUR TURN!

# Use plot() on the tree.mod2 model object to assess the model assumptions. How
# does everything look?


# back to presentation


# Updating Models and Model Selection -------------------------------------


# let's fit a new model with all predictors, including svi and gleason.score as
# factors
m1 <- lm(logpsa ~ ., data=pros) 
summary(m1)

op <- par(mfrow = c(2,2))
plot(m1) 

# 32 very influential

# update model without obs 32 using update() function
m2 <- update(m1, subset = -32)
plot(m2)


summary(m1)
summary(m2) # weight now appears marginally significant


# create new model without cap.pen, weight and age
pros.lm2 <- update(m1, . ~ . - cap.pen - weight - age)
summary(pros.lm2)

# compare models using anova()
# syntax: anova(smaller model, larger model)
anova(pros.lm2,m1)

# Result: fail to reject null; smaller model fits just as well as larger model

# Using AIC to select a model
step.out <- step(m1)
step.out
summary(step.out) # final model
# same as what we selected using anova()

# recall diagnostics of original model
plot(m1)

# obs 32 is very influential

# recall we fit model without obs 32
summary(m2)
plot(m2)


# create new model without cap.pen, weight and age AND obs 32 removed
# notice we're updating model m2
pros.lm2 <- update(m2, . ~ . - cap.pen - weight - age)
summary(pros.lm2)

# compare models using anova()
anova(pros.lm2,m2)

# AIC model selection with obs 32 removed
step(m2)

# AIC selects a different model based on 1 observation being removed. It kept
# weight.

par(op) # reset graphic parameters



# END

