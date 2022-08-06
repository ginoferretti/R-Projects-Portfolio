rm(list=ls())

library(timeSeries)
library(quantreg)
library(lmtest)
library(pracma)
library(tidyverse)
library(dplyr)
library(tseries)

#Using this because weekday and months uses local time setting, 
#and code won't work at some point
Sys.getlocale("LC_TIME")
Sys.setenv(LANG = "en")
Sys.setlocale("LC_TIME","english")
Sys.getlocale("LC_TIME")


set.seed(123)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))#Brings the session directly in file path folder

volume_df<- read_csv("MRNA.csv")#MRNA.csv
head(volume_df)

names(volume_df)[7]<-'volume'   #change "Volume" to "volume" because the former leads to problems with white spaces in the name
head(volume_df)

str(volume_df)

colSums(is.na(volume_df)) #no NAs
nrow(unique(volume_df))==nrow(volume_df)  #TRUE: no full-row duplicates
length(unique(volume_df$Date))==nrow(volume_df) #TRUE: no Date duplicates

volume <- ts(volume_df$volume)  #set volume to a time series
volume

time <- volume_df$Date  # time is the Date
time


#we use the log transformation to stabilize the variance and then decompose to detrend
# down here we can observe the decomposition of the time series: we have a trend component, a seasonal component, and finally a random component (error)
pdf("time_volume_plot.pdf")
plot(time,volume,type="l")
dev.off()


pdf("time_log(volume)plot.pdf")
plot(time,log(volume),type="l")
dev.off()

#divide by weeks
volumeW <- ts(volume, frequency=7)
volumeWlog <- log(volumeW)
volumeWlogdec <- decompose(volumeWlog)
plot(volumeWlogdec)

#months
volumeM <- ts(volume, frequency=30)
volumeMlog <- log(volumeM)
volumeMlogdec <- decompose(volumeMlog)
plot(volumeMlogdec)

#quarters
volumeQ <- ts(volume, frequency=90)
volumeQlog <- log(volumeQ)
volumeQlogdec <- decompose(volumeQlog)
plot(volumeQlogdec)

#years
volumeY <- ts(volume, frequency=365)
volumeYlog <- log(volumeY)
volumeYlogdec <- decompose(volumeYlog)
plot(volumeYlogdec)

#compare
plot(volumeWlogdec)

pdf("log(volume)decomposed_by_month.pdf")
plot(volumeMlogdec)#months seem like the best compromise, let's keep this one
dev.off()

plot(volumeQlogdec)
plot(volumeYlogdec)



#ACF - PACF, A trend behaviour (non-stationary) 
#pdf
par(mfrow=c(1,3), mar=c(4,4.5,1,1), cex.lab=1.5, cex.main=1.5, cex.axis=1.5)
plot(log(volume),type="l",xlab="Time", main="", axes=F);box()
axis(1, at=axTicks(1), labels = format(as.Date(time), format="%Y")[axTicks(1)+1])
axis(2)

pdf("ACF_logvolume.pdf")
acf(log(volume), main="log(volume) ACF")#the ACF plot drops very slowly with time, suggesting a possible non-stationarity
dev.off()

pdf("PACF_logvolume.pdf")
pacf(log(volume), main="log(volume) PACF")
dev.off()
adf.test(log(volume))  #we can test for the presence of a unit root (non-stationarity) with the ADF test
# the p-value is 0.278, not providing enough evidence to reject the null hypothesis of non-stationarity and confirming the suspicion that arose with the ACF plot
kpss.test(log(volume))  #p-value of 0.01 so we reject H0 of stationarity
#the two tests prove that log(volume) is NOT stationary

pdf("ACF_volume.pdf")
acf(volume,main="volume ACF")   # the ACF still drops slowly over time but it's nowhere near the level of the previous ACF: now it goes down more rapidly since the beginning
dev.off()

pdf("PACF_volume.pdf")
pacf(volume,main="volume PACF")
dev.off()
# since the ACF is still not ideal and raises some doubts, let's test again for stationarity
adf.test(volume)  #it  seems to be stationary
kpss.test(volume) #but the KPSS test tells us the opposite: we reject stationarity
# we don't know what to believe here: the two tests on volume are contradictory, so let's check if the error has to do with this

pdf("decomposedvolume.pdf")
plot(decompose(ts(volume, frequency=30))) # let's check the decomposed volume (f=30)
dev.off()

summary(decompose(ts(volume, frequency=30)))
decompose(ts(volume, frequency=30))[[4]]   #let's get the 4th element which is the time series of the residuals
decompose(ts(volume, frequency=30))[[4]][is.na(decompose(ts(volume, frequency=30))[[4]])==0]  #let's keep only the non-NA values
adf.test(as.ts(decompose(ts(volume, frequency=30))[[4]][is.na(decompose(ts(volume, frequency=30))[[4]])==0]))  #and now we run the adf test on this residual time series
kpss.test(decompose(ts(volume, frequency=30))[[4]][is.na(decompose(ts(volume, frequency=30))[[4]])==0])  #kpss test here instead

# the ADF test on the residuals rejects non-stationarity with p-value at 0.01
# and the KPSS test fails to reject stationarity with 0.1, so both show that the residuals of the decomposed
# time series of volume are STATIONARY, so the contradictory tests on stationarity of volume might be due to some other
# reasons like for example a structural break




# Dickey-Fuller test by hand

dts <- diff(volume)  #let's differentiate the volume time series to manually compute the DF test

summary(lm(dts ~ volume[-length(volume)]))  # we regress deltaYt (the differentiated volume) on volume at t-1
# the p-value for the regressor coefficient is very low, so we reject that it's 0 therefore we have evidence for the volume's stationarity (no presence of unit root)

summary(lm((diff(log(volume)) ~ log(volume)[-length(log(volume))])))
#same result of rejection here, evidence of stationarity even though we got the opposite result earlier


#Dickey fuller test by code
adf.test(volume) #stationary
kpss.test(volume) #non-stationary

adf.test(log(volume)) #non-stationary
kpss.test(log(volume)) #non-stationary


adf.test(diff(volume)) #stationary
kpss.test(diff(volume)) #stationary

adf.test(diff(log(volume))) #stationary
kpss.test(diff(log(volume))) #stationary


# the volume returns contradictory results on the two tests, while the log(volume) is always non-stationary
# both the differentiated volume and log(volume) seem to be stationary, so differentiating by 1 order is enough


#pdf
#stationary by differentiation
plot(dts)  #differentiated volume: stationary as proven earlier
plot(diff(log(volume))) #differentiated log(volume): also stationary as already proven

par(mfrow=c(1,1))
plot(dts, type="l")
lines(movavg(dts, 10), col=2, lwd=2)   #simple moving average of previous 10 observations

par(mfrow=c(1,2))
acf(dts)   #ACF drops significantly very early at spike 1
pacf(dts)  #PACF drops more slowly
# perhaps an MA(1) model would perform well

par(mfrow=c(1,1))
plot(abs(dts), type="l")   #absolute value of differentiated volume
adf.test(abs(dts))  #stationary
kpss.test(abs(dts)) #non-stationary
lines(movavg(abs(dts), 10), col=2, lwd=2)   #new moving average
lines(movavg(dts, 10), col=3, lwd=2)  #previous moving average
  
par(mfrow=c(1,2))
acf(abs(dts))  #ACF seems to be exponential
pacf(abs(dts)) #PACF spikes at 2
# we could try an AR(2) model

dev.off()


# Moving averages and variance for volume and log(volume) with 21 total periods (k = 10)

n <- length(volume)
k <- 10
ma <- mv <- lma <- lmv <- c()
for (i in (k+1):(n-k)){
  ma[i] <- mean(volume[(i-k):i+k])
  mv[i] <- mean((volume[(i-k):i+k]-ma[i])^2)
  lma[i] <- mean(log(volume[(i-k):i+k]))
  lmv[i] <- mean((log(volume[(i-k):i+k])-lma[i])^2)
}


#pdf
par(mfrow=c(2,2), mar=c(2,4.2,4,1), cex.lab=1.5, cex.main=1.5, cex.axis=1.5)

plot(volume, main="Moving avg volume k=10", xlab="Time", axes=F);box()
axis(1, at=axTicks(1), labels = format(as.Date(time), format="%Y")[axTicks(1)+1])
axis(2)
lines(ma, col=2, lwd=2)

plot(mv, type="l", col=4, lwd=2,  main="Moving variance volume k=10", xlab="Time", axes=F);box()
axis(1, at=axTicks(1), labels = format(as.Date(time), format="%Y")[axTicks(1)+1])
axis(2)

plot(log(volume), main="Moving avg log(volume) k=10", xlab="Time", axes=F);box()
axis(1, at=axTicks(1), labels = format(as.Date(time), format="%Y")[axTicks(1)+1])
axis(2)
lines(lma, col=2, lwd=2)

plot(lmv, type="l", col=4, lwd=2, main="Moving variance log(volume) k=10", xlab="Time", axes=F);box()
axis(1, at=axTicks(1), labels = format(as.Date(time), format="%Y")[axTicks(1)+1])
axis(2)

dev.off()

# Trying to fit a trend model within given quantiles
train <- 1:round(n*0.7)  #train with first 70% of data
test <- (round(n*0.7)+1):n   #test with last 30%
t <- train
fit.m <- lm(log(volume[train])~t)   #linear regression of log(volume) on time t; log(volume) is non-stationary so its expectation should depend on time
fit.0025 <- rq(log(volume[train])~t, tau=0.025)
fit.005 <- rq(log(volume[train])~t, tau=0.05)
fit.095 <- rq(log(volume[train])~t, tau=0.95)
fit.0975 <- rq(log(volume[train])~t, tau=0.975)
pred.m <- cbind(1,test)%*%coef(fit.m)   #linear predictions of expected log(volume) of testing set based on t
pred.0025 <- cbind(1,test)%*%coef(fit.0025) #linear predictions of level 0.025 quantile of log(volume) of testing set based on t
pred.005 <- cbind(1,test)%*%coef(fit.005) #0.05 quantile and so on...
pred.095 <- cbind(1,test)%*%coef(fit.095)
pred.0975 <- cbind(1,test)%*%coef(fit.0975)

pred.0025  #lower bound at 95% confidence for linear predictions of log(volume)
pred.0975  #upper bound

summary(fit.m)
summary(fit.005)
summary(fit.095)

mse.m  <- mean((log(volume)[test]-pred.m)^2) #MSE of model on testing set

#pdf
par(mfrow=c(1,1), mar=c(4,4.5,4,1), cex.lab=1.5, cex.main=1.5, cex.axis=1.5)

plot(log(volume),type="n",xlab="Time", main="Forecast", axes=F);box()
axis(1, at=axTicks(1), labels = format(as.Date(time), format="%Y")[axTicks(1)+1])
axis(2)
points(train, log(volume[train]), col="grey")  # log(volume) in training set
points(test, log(volume[test]))    # log(volume) in testing set
lines(fitted(fit.m), col=2, lwd=2)  # linear prediction line of expected log(volume)
lines(fitted(fit.0025), col=3, lwd=1) # linear prediction line of level 0.025 quantile of log(volume)
lines(fitted(fit.005), col=3, lwd=2) # 0.05 quantile and so on...
lines(fitted(fit.095), col=3, lwd=2)
lines(fitted(fit.0975), col=3, lwd=1)
lines(test, pred.m, col=4, lwd=2)  # extension of linear prediction line of expected log(volume) for testing set
lines(test, pred.0025, col=6, lwd=1) # extension of quantile regression lines...
lines(test, pred.005, col=6, lwd=2)
lines(test, pred.095, col=6, lwd=2)
lines(test, pred.0975, col=6, lwd=1)

abline(v=end(train), lty=2)


# this regression model seems terrible for the testing set, we suspect a structural break in mid 2019-2020 where the parameters change, we should run a Chow test for that
# if we were to remodel from ~2020 to 2021 we would probably get much better predictions by judging the plot

dev.off()



#The exploratory analysis suggests that it's possible to fit an ARIMA model for the log(volume) using an integration order of 1

#Fit model to train sample, using fit name instead of train to not repeat it every time
library(forecast)

acf(log(volume))  # once again log(volume) looks non-stationary so we need to differentiate it or use directly an ARIMA
pacf(log(volume))

acf(diff(log(volume)))
pacf(diff(log(volume)))
# the plots for the differentiated log(volume) seem to show that a MA(2) model could work on diff(log(volume)) (NOT on log(volume))


fit.ar    <- arima(log(volume)[train], c(1,0,0))
fit.ma    <- arima(log(volume)[train], c(0,0,1))
fit.arma  <- arima(log(volume)[train], c(1,0,1))
fit.arima11 <- arima(log(volume)[train], c(1,1,1))
fit.arima21 <- arima(log(volume)[train], c(2,1,1))
fit.arima12 <- arima(log(volume)[train], c(1,1,2))
fit.arima22 <- arima(log(volume)[train], c(2,1,2))


#pdf
par(mfrow=c(4,2), mar=c(2,4.5,3,1), cex.lab=1.5, cex.main=1.5, cex.axis=1.5)

plot(log(volume)[train], ylab="AR(1)", axes=F);box()
axis(1, at=axTicks(1), labels = format(as.Date(time), format="%Y")[axTicks(1)+1])
axis(2)
lines(fitted(fit.ar), col=4, lwd=2)
acf(residuals(fit.ar))   #let's check for residual serial correlation
pacf(residuals(fit.ar))
# the errors seem to be very weakly correlated with previous lags


plot(log(volume)[train], ylab="MA(1)", axes=F);box()
axis(1, at=axTicks(1), labels = format(as.Date(time), format="%Y")[axTicks(1)+1])
axis(2)
lines(fitted(fit.ma), col=4, lwd=2)
acf(residuals(fit.ma))
pacf(residuals(fit.ma))
# errors seem very correlated (possible non-stationarity), we could try AR(4) to model them


plot(log(volume)[train], ylab="ARMA(1,1)", axes=F);box()
axis(1, at=axTicks(1), labels = format(as.Date(time), format="%Y")[axTicks(1)+1])
axis(2)
lines(fitted(fit.arma), col=4, lwd=2)
acf(residuals(fit.arma))
pacf(residuals(fit.arma))
# residuals are uncorrelated to each other


plot(log(volume[train]), ylab="ARIMA(1,1,1)", axes=F);box()
axis(1, at=axTicks(1), labels = format(as.Date(time), format="%Y")[axTicks(1)+1])
axis(2)
lines(fitted(fit.arima11), col=4, lwd=2)
acf(residuals(fit.arima11))
pacf(residuals(fit.arima11))
# residuals have the least autocorrelation so far, best model for now (as we predicted, since diff(log(volume)) is stationary)


plot(log(volume[train]), ylab="ARIMA(2,1,1)", axes=F);box()
axis(1, at=axTicks(1), labels = format(as.Date(time), format="%Y")[axTicks(1)+1])
axis(2)
lines(fitted(fit.arima21), col=4, lwd=2)
acf(residuals(fit.arima21))
pacf(residuals(fit.arima21))
# seems to be very similar to the previous one in terms of residuals


plot(log(volume[train]), ylab="ARIMA(1,1,2)", axes=F);box()
axis(1, at=axTicks(1), labels = format(as.Date(time), format="%Y")[axTicks(1)+1])
axis(2)
lines(fitted(fit.arima12), col=4, lwd=2)
acf(residuals(fit.arima12))
pacf(residuals(fit.arima12))
# once again very similar


plot(log(volume[train]), ylab="ARIMA(2,1,2)", axes=F);box()
axis(1, at=axTicks(1), labels = format(as.Date(time), format="%Y")[axTicks(1)+1])
axis(2)
lines(fitted(fit.arima22), col=4, lwd=2)
acf(residuals(fit.arima22))
pacf(residuals(fit.arima22))
# same thing

# the arima models seem to be the best ones in terms of residuals, and they all look similar to each other
# so we might want to use the simpler ARIMA(1,1,1) for the parsimony principle, but let's see what the numbers tell us

auto.arima(log(volume)[train]) # ARIMA(1,1,1) is actually the best one

dev.off()


#Forecasts for short term

forc.ar <- fitted(Arima(y=log(volume)[test],model=fit.ar))
mse.ar  <- mean((log(volume)[test]-forc.ar)^2)

forc.ma <- fitted(Arima(y=log(volume)[test],model=fit.ma))
mse.ma  <- mean((log(volume)[test]-forc.ma)^2) 

forc.arma <- fitted(Arima(y=log(volume)[test],model=fit.arma))
mse.arma  <- mean((log(volume)[test]-forc.arma)^2) 

forc.arima11 <- fitted(Arima(y=log(volume)[test],model=fit.arima11))
mse.arima11 <- mean((log(volume)[test]-forc.arima11)^2)

forc.arima21 <- fitted(Arima(y=log(volume)[test],model=fit.arima21))
mse.arima21 <- mean((log(volume)[test]-forc.arima21)^2)

forc.arima12 <- fitted(Arima(y=log(volume)[test],model=fit.arima12))
mse.arima12 <- mean((log(volume)[test]-forc.arima12)^2)

forc.arima22 <- fitted(Arima(y=log(volume)[test],model=fit.arima22))
mse.arima22 <- mean((log(volume)[test]-forc.arima22)^2)


# ARIMA(2,1,2) performs best based on MSE of the testing set (0.17067)
# the linear regression model (MSE=3.852) and MA(1) (MSE=0.439) are the worst ones as expected, given the non-stationarity

summary(fit.ar)
summary(fit.ma)
summary(fit.arma)
summary(fit.arima11)
summary(fit.arima21)
summary(fit.arima12)
summary(fit.arima22)

# according to the AIC, ARIMA(2,1,2) is the best model also in the training set, with MA(1) being the worst by far
# this contradicts auto.arima which picks ARIMA(1,1,1) as the best, so maybe that function is checking another
# value like the BIC or the AICc


# Given the different MSEs, it makes sense to only plot ARIMAs and that is understandable 
# as we are running models for stationary time series on a non-stationary one, and this demonstrates it!

auto.arima(log(volume))
# The best model on the entire dataset seems to be ARIMA(2,1,2) again, which is good! It means we did a good analysis

par(mfrow=c(1,3), mar=c(2,4.5,3,1), cex.lab=1.5, cex.main=1.5, cex.axis=1.5)

#pdf
plot(log(volume), main="AR(1)",sub="MSE: 0.196", type="l", col="grey", axes=F);box()
axis(1, at=axTicks(1), labels = format(as.Date(time), format="%Y")[axTicks(1)+1])
axis(2)
lines(log(volume), col="grey")
lines(test, forc.ar, col=2)  #AR(1) predictions on the testing set plotted
abline(v=max(train),lty=2)

plot(log(volume), main="MA(1)",sub="MSE: 0.439", type="l", col="grey", axes=F);box()
axis(1, at=axTicks(1), labels = format(as.Date(time), format="%Y")[axTicks(1)+1])
axis(2)
lines(log(volume), col="grey")
lines(test, forc.ma, col=2)
abline(v=max(train),lty=2)

plot(log(volume), main="ARMA(1,1)",sub="MSE: 0.179", type="l", col="grey", axes=F);box()
axis(1, at=axTicks(1), labels = format(as.Date(time), format="%Y")[axTicks(1)+1])
axis(2)
lines(log(volume), col="grey")
lines(test, forc.arma, col=2)
abline(v=max(train),lty=2)

pdf("ARIMA111st.pdf")
plot(log(volume), main="ARIMA(1,1,1)",sub="MSE: 0.172",type="l", col="grey", axes=F);box()
axis(1, at=axTicks(1), labels = format(as.Date(time), format="%Y")[axTicks(1)+1])
axis(2)
lines(log(volume), col="grey")
lines(test, forc.arima11, col=2)
abline(v=max(train),lty=2)
dev.off()

pdf("ARIMA112st.pdf")
plot(log(volume), main="ARIMA(1,1,2)",sub="MSE: 0.17071", type="l", col="grey", axes=F);box()
axis(1, at=axTicks(1), labels = format(as.Date(time), format="%Y")[axTicks(1)+1])
axis(2)
lines(log(volume), col="grey")
lines(test, forc.arima12, col=2)
abline(v=max(train),lty=2)
dev.off()

pdf("ARIMA211st.pdf")
plot(log(volume), main="ARIMA(2,1,1)",sub="MSE: 0.1709", type="l", col="grey", axes=F);box()
axis(1, at=axTicks(1), labels = format(as.Date(time), format="%Y")[axTicks(1)+1])
axis(2)
lines(log(volume), col="grey")
lines(test, forc.arima21, col=2)
abline(v=max(train),lty=2)
dev.off()

pdf("ARIMA212st.pdf")
plot(log(volume), main="ARIMA(2,1,2)",sub="MSE: 0.17067", type="l", col="grey", axes=F);box()
axis(1, at=axTicks(1), labels = format(as.Date(time), format="%Y")[axTicks(1)+1])
axis(2)
lines(log(volume), col="grey")
lines(test, forc.arima22, col=2)
abline(v=max(train),lty=2)
dev.off()

# Long term forecast based off short term MSEs

fore.ar  <- forecast(fit.ar, h=(n-max(train)))
fore.ma  <- forecast(fit.ma, h=(n-max(train)))
fore.arma  <- forecast(fit.arma, h=(n-max(train)))
fore.arima11 <- forecast(fit.arima11, h=(n-max(train)))
fore.arima12 <- forecast(fit.arima12, h=(n-max(train)))
fore.arima21 <- forecast(fit.arima21, h=(n-max(train)))
fore.arima22 <- forecast(fit.arima22, h=(n-max(train)))

#pdf

par(mfrow=c(1,2), mar=c(2,4.5,3,1), cex.lab=1.5, cex.main=1.5, cex.axis=1.5)

plot(log(volume), main="AR(1)", type="l", col="grey", axes=F);box()
axis(1, at=axTicks(1), labels = format(as.Date(time), format="%Y")[axTicks(1)+1])
axis(2)
polygon(c(test,rev(test)), c(fore.ar$lower[,2],rev(fore.ar$upper[,2])), col="pink")
lines(log(volume), col="grey")
lines(test, fore.ar$mean, col=2, lwd=2)
lines(test, fore.ar$lower[,2], col=3, lwd=2)
lines(test, fore.ar$upper[,2], col=3, lwd=2)
abline(v=max(train),lty=2)

plot(log(volume), main="MA(1)", type="l", col="grey", axes=F);box()
axis(1, at=axTicks(1), labels = format(as.Date(time), format="%Y")[axTicks(1)+1])
axis(2)
polygon(c(test,rev(test)), c(fore.ma$lower[,2],rev(fore.ma$upper[,2])), col="pink")
lines(log(volume), col="grey")
lines(test, fore.ma$mean, col=2, lwd=2)
lines(test, fore.ma$lower[,2], col=3, lwd=2)
lines(test, fore.ma$upper[,2], col=3, lwd=2)
abline(v=max(train),lty=2)

plot(log(volume), main="ARMA(1,1)", type="l", col="grey", axes=F);box()
axis(1, at=axTicks(1), labels = format(as.Date(time), format="%Y")[axTicks(1)+1])
axis(2)
polygon(c(test,rev(test)), c(fore.arma$lower[,2],rev(fore.arma$upper[,2])), col="pink")
lines(log(volume), col="grey")
lines(test, fore.arma$mean, col=2, lwd=2)
lines(test, fore.arma$lower[,2], col=3, lwd=2)
lines(test, fore.arma$upper[,2], col=3, lwd=2)
abline(v=max(train),lty=2)

pdf("ARIMA111lt.pdf")
plot(log(volume), main="ARIMA(1,1,1)", type="l", col="grey", axes=F);box()
axis(1, at=axTicks(1), labels = format(as.Date(time), format="%Y")[axTicks(1)+1])
axis(2)
polygon(c(test,rev(test)), c(fore.arima11$lower[,2],rev(fore.arima11$upper[,2])), col="pink")
lines(log(volume), col="grey")
lines(test, fore.arima11$mean, col=2, lwd=2)
lines(test, fore.arima11$lower[,2], col=3, lwd=2)
lines(test, fore.arima11$upper[,2], col=3, lwd=2)
abline(v=max(train),lty=2)
dev.off()

pdf("ARIMA112lt.pdf")
plot(log(volume), main="ARIMA(1,1,2)", type="l", col="grey", axes=F);box()
axis(1, at=axTicks(1), labels = format(as.Date(time), format="%Y")[axTicks(1)+1])
axis(2)
polygon(c(test,rev(test)), c(fore.arima12$lower[,2],rev(fore.arima12$upper[,2])), col="pink")
lines(log(volume), col="grey")
lines(test, fore.arima12$mean, col=2, lwd=2)
lines(test, fore.arima12$lower[,2], col=3, lwd=2)
lines(test, fore.arima12$upper[,2], col=3, lwd=2)
abline(v=max(train),lty=2)
dev.off()

pdf("ARIMA211lt.pdf")
plot(log(volume), main="ARIMA(2,1,1)", type="l", col="grey", axes=F);box()
axis(1, at=axTicks(1), labels = format(as.Date(time), format="%Y")[axTicks(1)+1])
axis(2)
polygon(c(test,rev(test)), c(fore.arima21$lower[,2],rev(fore.arima21$upper[,2])), col="pink")
lines(log(volume), col="grey")
lines(test, fore.arima21$mean, col=2, lwd=2)
lines(test, fore.arima21$lower[,2], col=3, lwd=2)
lines(test, fore.arima21$upper[,2], col=3, lwd=2)
abline(v=max(train),lty=2)
dev.off()

pdf("ARIMA212lt.pdf")
plot(log(volume), main="ARIMA(2,1,2)", type="l", col="grey", axes=F);box()
axis(1, at=axTicks(1), labels = format(as.Date(time), format="%Y")[axTicks(1)+1])
axis(2)
polygon(c(test,rev(test)), c(fore.arima22$lower[,2],rev(fore.arima22$upper[,2])), col="pink")
lines(log(volume), col="grey")
lines(test, fore.arima22$mean, col=2, lwd=2)
lines(test, fore.arima22$lower[,2], col=3, lwd=2)
lines(test, fore.arima22$upper[,2], col=3, lwd=2)
abline(v=max(train),lty=2)
dev.off()

# The assumption is that there is a possibility, given the limited data, that the process could result in a stationary one, so we use MAs, ARs, and ARMAs 
# We used the log of the volume to get better and smoother estimates, and thanks to that we can benefit from a more stable variance over time


# the MA model seems like the worst one at dealing with non-stationarity as it has zero consideration for possible
# deviations/trends from the current mean and has two parallel lines (also parallel to the x-axis) as confidence intervals.

# The AR and ARMA models deal with this problem slightly better but their expected values are still quite off.

# All the ARIMAs work great and provide much better estimates and confidence intervals as they are designed to
# deal with non-stationarity and transform it into stationarity, and we can clearly see that from the plots.


