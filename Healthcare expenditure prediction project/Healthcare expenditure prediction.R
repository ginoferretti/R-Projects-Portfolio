rm(list=ls())

library(tidyverse)
library(dplyr)
library(cowplot)
library(quantreg)
library(qgam)

set.seed(123)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))#Brings the session directly in file path folder
data <- read.csv("healthexpenditure.csv")#healthexpenditure.csv
attach(data)

head(data)

str(data)

colSums(is.na(data)) #counts how many NAs we have (1 if TRUE, summed for each column): no NAs here

#Build in and out of sample data sets
train <- sample(1:nrow(data), nrow(data)*0.6)
test <- (1:nrow(data))[-train]
data.is <- data[train,]    #in sample
data.os <- data[test,]     #out of sample
n <- nrow(data)

dim(data.is)
dim(data.os)

# Define variables
#Y (totexp)
#X (suppins, totchr, age, female, white)

par(mfcol=c(2,2),mar=c(4,4,3,2), cex.lab=1.2, cex.axis=1.2, cex.main=1.2)

plot(density(data$totexp), main="totexp")
lines(density(data$totexp)$x, dnorm(density(data$totexp)$x,mean(data$totexp),sd(data$totexp)),col=2)

plot(density(data$ltotexp), main="log(totexp)")
lines(density(data$ltotexp)$x, dnorm(density(data$ltotexp)$x,mean(data$ltotexp),sd(data$ltotexp)),col=2)

plot(density(data$age), main="age")
lines(density(data$age)$x, dnorm(density(data$age)$x,mean(data$age),sd(data$age)),col=2)

plot(data$age, data$totexp, xlab="age", ylab = "totexp")

par(mfcol=c(2,1))
plot(lm(totexp~age, data = data), 1, caption="Lreg totexp/age")   #residuals over fitted values: this seems to suggest heteroskedasticity

plot(lm(totexp~age, data=data), 2, caption="Lreg totexp/age")     #qq plot: errors are NOT normally distributed

dev.off()

summary(age)
summary(white)
summary(female)


# Descriptive statistics

# OLS regression
train.lr <- lm(totexp ~ suppins+ totchr+age+female+ white, data=data.is)  #suppins, totchr, and female are significant at 5%
summary(train.lr)

lw <-0.025
up <-0.975

# Quantile regression

train.qrlw <- rq(totexp ~ suppins+ totchr+age+female+ white, tau = lw, data = data.is)
summary(train.qrlw)

train.qrup <- rq(totexp ~ suppins+ totchr+age+female+ white, tau = up, data = data.is)
summary(train.qrup)


par(mfcol=c(1,2))
plot(train.lr, 1)
plot(train.lr, 2)
dev.off()
#Bad performance by OLS lr model: heteroskedasticity + non-normality of residuals

# Prediction for out of sample, to see model performance
# predlw.lr <- predict(train.lr, data.os) + sigma(train.lr)*qnorm(lw)  #We can't use qnorm since the error is NOT normal!
# predup.lr <- predict(train.lr, data.os) + sigma(train.lr)*qnorm(up)  #Same thing here
predlw.qr <- predict(train.qrlw, data.os)  
predup.qr <- predict(train.qrup, data.os)

plot(density(train.lr$residuals))        # distribution of residuals compared to normal distribution: NOT similar
lines(density(train.lr$residuals)$x, dnorm(density(train.lr$residuals)$x,mean(train.lr$residuals),sd(train.lr$residuals)),col=2)

qnorm(c(lw,up))
(quantile(train.lr$residuals, c(0.025,0.975))-mean(train.lr$residuals))/sigma(train.lr)
#quantiles for the residual distribution at level 0.025 and 0.975, minus the mean, all divided by SE gives us how many
#SE we are away from the mean
qLW <- (quantile(train.lr$residuals, 0.025)-mean(train.lr$residuals))/sigma(train.lr)
qUP <- (quantile(train.lr$residuals, 0.975)-mean(train.lr$residuals))/sigma(train.lr)

predlw.lr <- predict(train.lr, data.os) + sigma(train.lr)*qLW
predup.lr <- predict(train.lr, data.os) + sigma(train.lr)*qUP



sigma(train.lr)
sqrt(sum((train.lr$residuals)^2)/1767)     #SE of the linear regression (residuals) = square root of SSR/dF
sigma(train.lr) == sqrt(sum((train.lr$residuals)^2)/1767)   # TRUE


#Now let's repeat with the log of totexp to see if the error distribution changes

plot(density(data$ltotexp), main="log(totexp)")
lines(density(data$ltotexp)$x, dnorm(density(data$ltotexp)$x,mean(data$ltotexp),sd(data$ltotexp)),col=2)

plot(density(data$age), main="age")
lines(density(data$age)$x, dnorm(density(data$age)$x,mean(data$age),sd(data$age)),col=2)

plot(data$age, data$ltotexp, xlab="age", ylab = "log(totexp)")

plot(lm(ltotexp~age, data = data), 1, caption="Lreg log(totexp)/age")
plot(lm(totexp~age, data=data), 2, caption="Lreg log(totexp)/age")   #AGAIN non-normality of errors

train.lrLOG <- lm(ltotexp ~ suppins+ totchr+age+female+ white, data=data.is)  #intercept, suppins, totchr, and age are significant at 5%
summary(train.lrLOG)

par(mfrow=c(1,2))
plot(train.lrLOG,1)  #still slightly heteroskedastic but much better than the other model
plot(train.lrLOG,2)  #almost normal!

# we probably need to implement non-linearities or interactions between regressors
# to get a normal error but we don't have time for that now



# ANOVA test for coefficient differences

#The interpretation of the result of a joint test of equality of slopes is that overall,
#the effect of the entire set of coefficients is uniform across quantiles.
#tests whether slope coefficients of those models, from several quantiles, 
#can be considered not different. Rejecting the null given the p-value.
anova(train.qrlw, train.qrup)

#Computing the coverage to see the goodness of the models
#Likelihood ratio test for coverage
#F information
infopredlw.lr <- 1*(data.os$totexp < predlw.lr)  #  we get 1 if the real value of y is below the 2.5th percentile of the prediction
infopredup.lr <- 1*(data.os$totexp > predup.lr)  # 1 if it is above the 97.5th percentile of predictions
infopredlw.qr <- 1*(data.os$totexp < predlw.qr)  # same thing but for the quantile regression
infopredup.qr <- 1*(data.os$totexp > predup.qr)  # same here

uc.predlw.lr <- mean(infopredlw.lr) # if we are correct we expect 0.025 in all of these 4
uc.predup.lr <- mean(infopredup.lr)
uc.predlw.qr <- mean(infopredlw.qr)
uc.predup.qr <- mean(infopredup.qr) # we are very close to the ideal value, seems good!

#P-value calculus
pval.predlw.lr <- 1-pchisq(-2*(sum((infopredlw.lr)*log(lw)+(1-infopredlw.lr)*log(up))-sum((infopredlw.lr)*log(uc.predlw.lr)+(1-infopredlw.lr)*log(1-uc.predlw.lr))), 1)
pval.predup.lr <- 1-pchisq(-2*(sum((infopredup.lr)*log(lw)+(1-infopredup.lr)*log(up))-sum((infopredup.lr)*log(uc.predup.lr)+(1-infopredup.lr)*log(1-uc.predup.lr))), 1)
pval.predlw.qr <- 1-pchisq(-2*(sum((infopredlw.qr)*log(lw)+(1-infopredlw.qr)*log(up))-sum((infopredlw.qr)*log(uc.predlw.qr)+(1-infopredlw.qr)*log(1-uc.predlw.qr))), 1)
pval.predup.qr <- 1-pchisq(-2*(sum((infopredup.qr)*log(lw)+(1-infopredup.qr)*log(up))-sum((infopredup.qr)*log(uc.predup.qr)+(1-infopredup.qr)*log(1-uc.predup.qr))), 1)



# Plotting data to see how the models do on out of sample data
pdf("quantreg.pdf")
quantreg.all <- rq(totexp ~ suppins+ totchr+age+female+ white, tau = seq(0.05, 0.975, by = 0.05), data=data.is)
quantreg.plot <- summary(quantreg.all)
plot(quantreg.plot)
dev.off()


#Totchr gets the best representation with qr
#So let's try something else
lw <-0.025
up <-0.975

train.qr2lw <- rq(totexp ~ totchr, tau = lw, data = data.is)
summary(train.qr2lw)

train.qr2up <- rq(totexp ~ totchr, tau = up, data = data.is)
summary(train.qr2up)

predlw.qr2 <- predict(train.qr2lw, data.os)
predup.qr2 <- predict(train.qr2up, data.os)

#Computing the coverage to see the goodness of the models
#Likelihood ratio test for coverage
#F information

infopredlw.qr2 <- 1*(data.os$totexp < predlw.qr2)
infopredup.qr2 <- 1*(data.os$totexp > predup.qr2)


uc.predlw.qr2 <- mean(infopredlw.qr2)
uc.predup.qr2 <- mean(infopredup.qr2)

#P-value calculus
pval.predlw.qr2 <- 1-pchisq(-2*(sum((infopredlw.qr2)*log(lw)+(1-infopredlw.qr2)*log(up))-sum((infopredlw.qr2)*log(uc.predlw.qr2)+(1-infopredlw.qr2)*log(1-uc.predlw.qr2))), 1)
pval.predup.qr2 <- 1-pchisq(-2*(sum((infopredup.qr2)*log(lw)+(1-infopredup.qr2)*log(up))-sum((infopredup.qr2)*log(uc.predup.qr2)+(1-infopredup.qr2)*log(1-uc.predup.qr2))), 1)

# The null hypotheses of the tests state that the smaller models predict proportions equal to the real ones:
# high p-values therefore indicate a good fit, statistically as good as the larger models.


