# Clay Ford
# UVA StatLab
# Fall 2014

# Linear Modeling in R

# Tips:
# 1. Ctrl + Enter to submit commands
# 2. Use Tab to complete a command

# simple linear regression ------------------------------------------------


# generate some data
x <- seq(1,10,length.out = 100)
set.seed(1) # so I always get same random numbers
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

# see the fitted values (ie, what our model predicts)
fitted(slr)
# plot fitted points if you want
points(x,fitted(slr),pch=19,col="red") 
# see the residuals (observed y - predicted y)
residuals(slr)
# plot distance between fitted and observed
segments(x,y,x,fitted(slr),col="red") 

# calculate the first residual
# residual = observed response - predicted response
y[1] - fitted(slr)[1]

# Missing data means the case is dropped from the analysis:
# set 5th value of x to NA ("Not Available")
x[5] <- NA
summary(lm(y ~ x))
# note the message: "1 observation deleted due to missingness"


# a more realistic example:
# prostate cancer data (from Applied Linear Statistical Models, 5th ed)
pros <- read.csv("data/prostate.csv")
summary(pros)
names(pros)
str(pros)
pairs(pros)
# some observations:
# svi appears to be a factor with levels of 0 and 1
# gleason score is discrete
# weight has an outlier (data entry error?)
summary(pros$weight)
# bph has a lot of 0's
summary(pros$bph)
hist(pros$bph)

# check distribution of response
hist(pros$psa) # skewed right
# recall assumption that response is Normally distributed;
# a log transformatio may fix this:
hist(log(pros$psa))

# update data frame so response is log transformed:
pros$psa <- log(pros$psa)
pairs(pros)

# fit model using all variables
# the plus (+) sign means "include" in model
m1 <- lm(psa ~ volume + weight + age + bph + svi + cap.pen + gleason.score, data=pros)
m1

# Note: the response is log transformed, so interpretation of coefficients changes.
# To interpret volume coefficient, exponentiate: exp(0.07) = 1.07,
# so an increase in one unit of weight corresponds to about a 7% increase in PSA.

# use extractor functions to see model results:
summary(m1)
coef(m1)
fitted(m1)
residuals(m1)
deviance(m1) # RSS
df.residual(m1) # degrees of freedom (n - p)

# calculate RSS "by hand"
# = sqrt(RSS/(n-p))
sum(residuals(m1)^2)

# what about standard errors, R-squared? How to extract those?
# save the summary
m1s <- summary(m1)
names(m1s)
m1s$coefficients
m1s$coefficients[,3] # standard errors
m1s$r.squared
m1s$sigma # residual standard error

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
cbind(pros$psa,Y)



# confidence intervals ----------------------------------------------------


# confidence intervals for coefficients
confint(m1) # 95%
confint(m1, level=0.90)
confint(m1,"volume")

# plot of 1 and 2 SE confidence intervals of coefficients
library(arm)
coefplot(m1)
# include intercept and draw a box around the plot
coefplot(m1, frame.plot=TRUE, intercept=TRUE)


# confidence intervals for predictions
# first need to generate "new" data;
# create new data set consisting of means of original data
new.data <- data.frame(lapply(pros,mean))
predict(m1, newdata=new.data, interval = "confidence")
predict(m1, newdata=new.data, interval = "prediction")

# In two-dimensions (1 predictor), we can plot these as confidence bands
m2d <- lm(psa ~ volume, data=pros)
new.data <- data.frame(volume=seq(min(pros$volume),
                                  max(pros$volume),
                                  length.out = 100))
p.conf <- predict(m2d, newdata=new.data, interval = "confidence")
p.pred <- predict(m2d, newdata=new.data, interval = "prediction")
plot(psa ~ volume, data=pros)
matlines(new.data$volume, p.conf, lty=c(1,2,2), col="black")
matlines(new.data$volume, p.pred, lty=c(1,3,3), col="black")



# model formula and specifications ----------------------------------------


# recall previous model
m1 

# this does the same thing:
m1 <- lm(psa ~ ., data=pros)
m1

# model with volume and weight and their interaction
m2 <- lm(psa ~ volume + weight + volume:weight, data=pros)
summary(m2)
# same thing
m2 <- lm(psa ~ volume*weight, data=pros)
summary(m2)

# model with 3 predictors and all 2-way interactions
m3 <- lm(psa ~ (volume + weight + age)^2, data=pros)
summary(m3)

# ploynomial regression
m4 <- lm(psa ~ volume + I(volume^2), data=pros)
summary(m4)

# plot polynomial regression
plot(psa ~ volume, data=pros)
new.data <- data.frame(volume=seq(min(pros$volume),
                                  max(pros$volume),
                                  length.out = 100))
outy <- predict(m4, newdata = new.data)
lines(new.data$volume, outy, lty=1, col="blue")



# regression diagnostics --------------------------------------------------

# visual diagnostics
plot(m1)

# make all plots fit in one graphic
par(mfrow=c(2,2)) 
plot(m1)
par(mfrow=c(1,1)) # restore to previous setting

# what's up with observation 32?
pros[32,]

# check independence assumption (or try to anyway since we don't have time variables)
# plot residuals against all variables and look for patterns
plot(residuals(m1) ~ ., data=pros)


# check for influential values
im.out <- influence.measures(m1)
im.out
summary(im.out)
# observation 32 looks particularly influential in log transformed model

# check for collinearity
# use vif() from the faraway package
library(faraway)
vif(m1)



# Model Selection ---------------------------------------------------------


# updating models
summary(m1)
# create new model without weight
m2 <- update(m1, . ~ . - weight)
# compare models using anova()
# syntax: anova(smaller model, larger model)
anova(m2,m1)

# Result: fail to reject null; smaller model fits just as well as larger model
# Note the p-value matches the summary output

# Usually use anova for something like this,
# drop all variables except volume and svi
m2 <- update(m1, . ~ . - weight - age - cap.pen)
summary(m2)
anova(m2, m1)

# Result: fail to reject null; smaller model fits just as well as larger model


# Using AIC to select a model
step.out <- step(m1)
step.out
summary(step.out) # final model
# same as what we selected using anova()
step.out$anova # log of selection
extractAIC(m1)
extractAIC(update(m1,.~.-weight))

# check diagnostics
par(mfrow=c(2,2))
plot(m2)
# check for influential values
summary(influence.measures(m2))
# obs 32 no longer influential but 94 looks interesting...


# fit model without obs 94
m2a <- update(m2, . ~ . , subset= -94) 
summary(m2)
summary(m2a)
plot(m2a)

# Better? Worth it? What do we think? Judgment call.
# my call: leave it in

# returning to updated model with all observations:
summary(m2)
# lots of stars, but examine effect size.
# practically or just statistically significant?
# use judgment. 


# best subset, forward and backward selection
library(leaps)
# best subset
regfit.sub <- regsubsets(psa ~ ., data=pros)
summary(regfit.sub)
plot(regfit.sub, main="best subset")

# forward selection
regfit.fwd <- regsubsets(psa ~ ., data=pros, method="forward")
summary(regfit.fwd)
plot(regfit.fwd, main="forward selection")

# backward selection
regfit.bwd <- regsubsets(psa ~ ., data=pros, method="backward")
summary(regfit.bwd)
plot(regfit.bwd, main="backward selection")

# all select the same "best" model;
# extract coefficients for best model:
which.min(summary(regfit.sub)$bic)
which.min(summary(regfit.fwd)$bic)
which.min(summary(regfit.bwd)$bic)

coef(regfit.sub, id=4)
coef(regfit.fwd, id=4)
coef(regfit.bwd, id=4)


# one way to check fit:
# a good fit will lie close to line with slope 1 and intercept 0
par(mfrow=c(1,1))
plot(pros$psa, fitted(m2),
     xlim=c(0,5),ylim=c(0,5),
     xlab="Observed",ylab="Fitted")
abline(a=0,b=1,lty=2)
# biased predictions for small values 



# factors and contrasts ---------------------------------------------------


pros$gleason.score
summary(pros$gleason.score)
pros$svi
summary(pros$svi)
# let's change gleason.score and svi to factors (categorical variables)
pros$gleason.score <- factor(pros$gleason.score)
pros$svi <- factor(pros$svi)

summary(pros$gleason.score) 
summary(pros$svi)
# summary stats
# mean psa for each gleason.score group and boxplots
aggregate(psa ~ gleason.score, pros, mean)
boxplot(psa ~ gleason.score, pros)
# mean psa for each svi group and boxplots
aggregate(psa ~ svi, pros, mean)
boxplot(psa ~ svi, pros)

# verify gleason.score is factor
class(pros$gleason.score)
is.factor(pros$gleason.score)

# linear models with gleason.score (aka, ANOVA)
fm1 <- lm(psa ~ gleason.score, pros)
summary(fm1)
anova(fm1)


# linear model where gleason.score interacted with svi (factor:factor)

# plot interaction of factors:
boxplot(psa ~ gleason.score * svi, data=pros)
interaction.plot(x.factor = pros$gleason.score,
                 trace.factor = pros$svi,
                 response = pros$psa)
aggregate(psa ~ gleason.score * svi, data=pros, mean)
table(pros$gleason.score, pros$svi)

# fit linear model (aka, 2-factor ANOVA)
fm3 <- lm(psa ~ gleason.score * svi, pros)
summary(fm3)


# linear model where gleason.score interacted with volume (factor:numeric)

# plot interaction of factor and numeric:
plot(psa ~ volume, data=pros, col=as.integer(gleason.score))
coplot(psa ~ volume | gleason.score, data=pros, rows = 1)
# or using ggplot (makes it easy to include smooth regression lines)
library(ggplot2)
ggplot(pros, aes(x=volume,y=psa)) + 
  geom_point() + geom_smooth() + facet_wrap(~gleason.score)
# plot both smooth and linear lines
ggplot(pros, aes(x=volume,y=psa)) + 
  geom_point() + geom_smooth(se=F) + geom_smooth(method="lm",se=F,color="red") + 
  facet_wrap(~gleason.score)


# now fit the model (difference in slopes for each level of gleason.score?)
fm4 <- lm(psa ~ gleason.score*volume, pros)
summary(fm4)
# slope not 0, but no significant difference in slopes
# confidence interval for volume
confint(fm4,"volume")

# plot results
plot(psa ~ volume, data=pros, col=as.integer(gleason.score))
abline(coef(fm4)[1],b=coef(fm4)[4])
abline(coef(fm4)[1]+coef(fm4)[2],b=coef(fm4)[4]+coef(fm4)[5],col=2)
abline(coef(fm4)[1]+coef(fm4)[3],b=coef(fm4)[4]+coef(fm4)[6],col=3)
legend("bottomright",legend = levels(pros$gleason.score), lty=1, col=1:3, title = "gleason score")

# a little easier with ggplot
ggplot(pros, aes(x=volume, y=psa, col=gleason.score)) + geom_point() +
  geom_smooth(method="lm", se=F)


# model validation --------------------------------------------------------


# all data
lm.all <- lm(psa ~ volume + bph + svi + gleason.score, data=pros)
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
  lm.train <- lm(psa ~ volume + bph + svi + gleason.score, data=pros, 
                 subset=train)
  # test error estimate
  mse[i] <- mean((pros$psa - predict(lm.train, pros))[-train]^2)
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
glm.all <-  glm(psa ~ volume + bph + svi + gleason.score, data=pros)
summary(glm.all)
# do 5-fold cross validation
cv.mse <- cv.glm(pros, glm.all, K=5)$delta # the first number is mean MSE
cv.mse
sqrt(cv.mse[1])


# END