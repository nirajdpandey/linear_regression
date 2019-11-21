#Author - Niraj Dev Pandey
#Email - pandey@uni-hildesheim.de

library(usdm) # for testing collinearity
library(car) # for testing outliers
library(MASS) # for testing studentized residuals
library(DAAG) # for cross validation of model
library(lmtest) # for checking homoskedasticity / heteroskedasticity


Boston


data(Boston) 
help(Boston)
attach(Boston)

chas <- as.factor(chas)

boston.df <- data.frame(crim, zn, indus, chas, nox, rm, age, dis, rad, tax, ptratio, black, lstat, medv)

str(boston.df)

#####

str(boston.df)

pairs(boston.df)

#####

fit1 <- lm(medv ~ ., data = boston.df)
summary(fit1)

bptest(fit1)

vif(fit1)

fit2 <- update(fit1, ~ . - tax)
vif(fit2)
summary(fit2)

fit3 <- update(fit2, ~ . - age - indus - rad)
summary(fit3)
outlierTest(fit3, cutoff = Inf, n.max = 15)

boston.df <- boston.df[-c(369, 372, 373, 370, 413, 365, 371, 366, 187, 375),]

fit4 <- lm(medv ~ . - tax - age - indus - rad, data = boston.df)
summary(fit4)
par(mfrow = c(2,2))
plot(fit4)

studentizedResiduals <- studres(fit4)
par(mfrow = c(1,1))
hist(studentizedResiduals, freq = FALSE, main = "distribution of Studentized Residuals")
xfit <- seq(min(studentizedResiduals), max(studentizedResiduals), length = 40)
yfit <- dnorm(xfit)
lines(xfit, yfit)

fit5 <- lm(medv ~ . - tax - age - indus - rad + I(lstat^2), data = boston.df)
summary(fit5)
par(mfrow = c(2,2))
plot(fit5)

fit6 <- lm(medv ~ . - tax - age - indus - rad + I(lstat^2) + I(lstat^3), data = boston.df)
summary(fit6)

####

par(mfrow = c(1,1))
fit4_CV <- CVlm(data = boston.df, form.lm = formula(fit4), m = 10, dots = FALSE, seed = 1, plotit = c("Observed", "Residual"), main = "Cross-validation for fit4", legend.pos="topleft")

attributes(fit4_CV)
# Here we will get mean squared: 15.8 

par(mfrow = c(1,1))
fit5_CV <- CVlm(data = boston.df, form.lm = formula(fit5), m = 10, dots = FALSE, seed = 1, plotit = c("Observed", "Residual"), main = "Cross-validation for fit5", legend.pos="topleft")

attributes(fit5_CV)
# Here we will get mean squared: 13.7

par(mfrow = c(1,1))
fit6_CV <- CVlm(data = boston.df, form.lm = formula(fit6), m = 10, dots = FALSE, seed = 1, plotit = c("Observed", "Residual"), main = "Cross-validation for fit6", legend.pos="topleft")

attributes(fit6_CV)
# Here we will get mean squared: 13.7

