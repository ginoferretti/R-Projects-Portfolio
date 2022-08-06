rm(list=ls())

library(tidyverse)
library(dplyr)
library(cowplot)
library(pROC)
library(corrplot)

set.seed(123)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))#Brings the session directly in file path folder

df.heart_disease <- read.csv("framingham.csv")#framingham.csv
data<-df.heart_disease 

head(data) # you see data

#Description of variables


#Demographics

#male: refers to the gender (we don't know if it's biological or self-assigned), with 1 being male and 0 otherwise (binary)

#age: age of the patient, continuous. Although the recorded ages have been truncated to whole numbers, the concept of age is continuous

#education: education level (categorical), with possible levels being 1 (high school), 2 (Bachelor's), 3 (Master's), 4 (PhD)


#Behavioural

#currentSmoker: whether or not the patient is a current smoker (binary)

#cigsPerDay: the number of cigarettes that the person smoked on average in one day (can be considered continuous as one can have any number of cigarettes, even half of one)


#Medical (history)

#BPMeds: whether or not the patient was on blood pressure medication (binary)

#prevalentStroke: whether or not the patient had previously had a stroke (binary)

#prevalentHyp: whether or not the patient was hypertensive, meaning having a consistently-high blood pressure (binary)

#diabetes: whether or not the patient had diabetes (binary)


#Medical (current)

#totChol: total cholesterol level (continuous)

#sysBP: systolic blood pressure (continuous)

#diaBP: diastolic blood pressure (continuous)

#BMI: Body Mass Index (continuous)

#heartRate: heart rate (continuous). In medical research, variables such as heart rate (though in fact discrete) are considered continuous because of a large number of possible values

#glucose: glucose level (continuous)


#Variable to predict (desired target)

#TenYearCHD: 10 year risk of coronary heart disease "CHD" (binary: "1" means "Yes", "0" means "No")


str(data) # this shows data types

#bringing out duplicates and NAs
data <- distinct(data)
nrow(distinct(df.heart_disease))==nrow(df.heart_disease) #TRUE: there were no duplicates to begin with, good

colSums(is.na(data))   # 7 variables have at least 1 NA, the worst case being glucose with 388 NAs


sum(rowSums(is.na(df.heart_disease))>0)  #counts how many rows have at least 1 NA (and should therefore be removed)
data <- na.omit(data)  #removes the entire rows where there is at least 1 NA

colSums(is.na(data))  #now we check and have 0 NA cases, we have cleaned our dataset (removed 582 rows because of NAs and 0 duplicates, went from 4238 to 3656 rows)

cmat<-cor(data) # correlation matrix for the 16 variables
corrplot(cmat, main="Correlation plot")  #correlation plot with coloured circles
corrplot.mixed(cmat)  #this one has more information but is much more confusing


# Convert binary variables to characters for better visualization
data <- data %>%
  mutate(male = as.character(male),
         currentSmoker = as.character(currentSmoker),
         # BPMeds = as.character(BPMeds),
         # prevalentStroke = as.character(prevalentStroke),    #in case we ever want to convert these two as well
         prevalentHyp = as.character(prevalentHyp),
         diabetes = as.character(diabetes),
         TenYearCHD = as.character(TenYearCHD))

str(data) #conversion successful
#Boxplots to see some relationships between variables
#Bp1
pdf("Boxplot1.pdf")
x <- ggplot(data = data, mapping = aes(x = as.factor(TenYearCHD), y = age, fill = TenYearCHD)) +
  geom_boxplot()
y <- ggplot(data = data, mapping = aes(x = as.factor(TenYearCHD), y = totChol, color = TenYearCHD)) +
  geom_boxplot()
p <- plot_grid(x, y)  #arrange multiple plots into a grid
title <- ggdraw() + draw_label("Relationship between TenYearCHD and age / totChol", fontface='bold')
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))
dev.off()

#Bp2
pdf("Boxplot2.pdf")
x <- ggplot(data = data, mapping = aes(x = as.factor(TenYearCHD), y = BMI, fill = TenYearCHD)) +
  geom_boxplot()
y <- ggplot(data = data, mapping = aes(x = as.factor(TenYearCHD), y = heartRate, color = TenYearCHD)) +
  geom_boxplot()
p <- plot_grid(x, y) 
title <- ggdraw() + draw_label("Relationship between TenYearCHD and BMI / heartRate", fontface='bold')
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))
dev.off()

#Bp3
pdf("Boxplot3.pdf")
x <- ggplot(data = data) +
  geom_count(mapping = aes(x = male, y = TenYearCHD))
y <- ggplot(data = data) +
  geom_count(mapping = aes(x = diabetes, y = TenYearCHD))
p<- plot_grid(x, y) 
title <- ggdraw() + draw_label("Relationship between TenYearCHD and male / diabetes", fontface='bold')
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))
dev.off()

#Bp4
pdf("Boxplot4.pdf")
x <- ggplot(data = data, mapping = aes(x = as.factor(TenYearCHD), y = cigsPerDay, fill = TenYearCHD)) +
  geom_boxplot()
y <- ggplot(data = data, mapping = aes(x = as.factor(TenYearCHD), y = sysBP, color = TenYearCHD)) +
  geom_boxplot()
p <- plot_grid(x, y) 
title <- ggdraw() + draw_label("Relationship between TenYearCHD and cigsPerDay / sysBP", fontface='bold')
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))
dev.off()


pdf("Corrplot.pdf")
corrplot(cmat)
dev.off()
#Start modeling

data <- data %>% 
  mutate(TenYearCHD = as.integer(TenYearCHD))   #let's transform TenYearCHD back to an integer variable

# Build in and out of sample data sets
xis <- sample(1:nrow(data), nrow(data)*0.7)  #training set = 70% of total sample
xos <- (1:nrow(data))[-xis]  #testing set = the remaining 30%
data.is <- data[xis,]  #we select the extracted rows for both cases
data.os <- data[xos,]
nis <- nrow(data.is)  # 2559 obs to train the model
nos <- nrow(data.os)  # 1097 obs to test the model

dim(data.is)
dim(data.os)

#Trying to test some independence hypothesis
tab.education <- table(data.is$education, data.is$TenYearCHD)  #contingency table for education and CHD risk in training set (how many obs for each combination of the categorical variables)
exp.education <- (margin.table(tab.education,1))%*%t(margin.table(tab.education,2))/nis
chisq.test(tab.education)
#p-value is extremely small so we have very convincing statistical evidence (even though not all the cells have more than 5 observations) to reject the null hypothesis of independence
#there seems to be dependence between education and CHD risk

tab.male <- table(data.is$male, data.is$TenYearCHD)
exp.male <- (margin.table(tab.male,1))%*%t(margin.table(tab.male,2))/nis
chisq.test(tab.male)
#very small p-value once again so we can safely assume there is dependence between gender and CHD risk


#####################################

# In-sample fit
log.fit1 <- glm(TenYearCHD ~ ., data=data.is, family="binomial")  #glm with binomial family for link function
round(summary(log.fit1)$coefficients, 2) #as we have tested earlier, gender and age are statistically significant in determining CHD risk

p1.is <- predict(log.fit1, newdata=data.is, type="response") #predicted probabilities of CHD risk in sample
d1.is <- 1*(p1.is>0.5)   #we are assigning 1 when the predicted probability of CHD given our model is greater than 50%, 0 otherwise
summary(log.fit1)  #AIC is 1944.2, seems pretty high

log.preds1 = rep("No CHD", 2559)
log.preds1[d1.is==1] = "CHD"     #vector with "CHD" for all people at risk (p>0.5), and "No CHD" otherwise



attach(data.is)
ct <- table(log.preds1,TenYearCHD)   #contingency table for our predictions and the real outcomes
acc_ct <- (ct[1,2]+ct[2,1])/sum(ct)  #accuracy for our model within the training set is 85.7%


#Removing the insignificant variables and retraining the model

log.fit2 <- glm(TenYearCHD ~ male + age + cigsPerDay + sysBP + glucose, data=data.is, family="binomial")
round(summary(log.fit2)$coefficients, 2)

p2.is <- predict(log.fit2, newdata=data.is, type="response")
d2.is <- 1*(p2.is>0.5)
summary(log.fit2)  #AIC = 1942.3

log.preds2 = rep("No CHD", 2559)
log.preds2[d2.is==1] = "CHD"


ct2 <- table(log.preds2,TenYearCHD)
acc_ct2 <- (ct2[1,2]+ct2[2,1])/sum(ct2)  #accuracy is now 85.42%, a tiny bit worse

#so for in sample predictions the two models seem to be very similar (almost same AIC and accuracy),
#but what we are really interested in is how they perform out of sample


# Out-of-sample predictions
p1.os <- predict(log.fit1, newdata=data.os, type="response")  #forecast of probability of CHD risk out of sample
d1.os <- 1*(p1.os>0.5)
cm.1 <- table(data.os$TenYearCHD, d1.os)
#accuracy measure
acc.1 <- (cm.1[1,1] + cm.1[2,2])/nos   # 84.87%


p2.os <- predict(log.fit2, newdata=data.os, type="response")
d2.os <- 1*(p2.os>0.5)
cm.2 <- table(data.os$TenYearCHD, d2.os)
#accuracy measure
acc.2 <- (cm.2[1,1] + cm.2[2,2])/nos   # 84.96%


# Hosmer-Lemeshow test


# g <- 5   #number of groups we are creating based on sorted predicted probabilities
# p1.os.s <- sort(p1.os, index.return=T)   #sorting probabilities in ascending order
# p1.g <- apply(matrix(p1.os.s$x,floor(nos/g),g), 2, mean)   #calculating the mean for each group containing 219 observations: this will leave out 2 people
# n1.g <- floor(nos/g)
# E1.g <- floor(n1.g*p1.g)
# O1.g <- apply(matrix(data.os$TenYearCHD[p1.os.s$ix],floor(nos/g),g), 2, sum)
# H1 <- sum((O1.g-E1.g)^2/floor(n1.g*p1.g*(1-p1.g)), na.rm = T)
# pv1 <- 1-pchisq(H1, g-2)
#the test is equal to 0.57, so we accept the null hypothesis of equality between the observed
#and expected frequencies.
#The results obtained by these analysis confirm the goodness of the model, according to
#the company's goal.

# p2.os.s <- sort(p2.os, index.return=T)
# p2.g <- apply(matrix(p2.os.s$x,floor(nos/g),g), 2, mean)
# n2.g <- floor(nos/g)
# E2.g <- floor(n2.g*p2.g)
# O2.g <- apply(matrix(data.os$TenYearCHD[p2.os.s$ix],floor(nos/g),g), 2, sum)
# H2 <- sum((O2.g-E2.g)^2/floor(n2.g*p2.g*(1-p2.g)), na.rm = T)
# pv2 <- 1-pchisq(H2, g-2)
# #the test is equal to 0.66, so we accept the null hypothesis of equality between the observed
# #and expected frequencies.
# #The results obtained by these analysis confirm the goodness of the model, according to
# #the company's goal.


#this process  eliminates 2 observations (nos%%g=2), we don't like that (we know it's much faster but we want to be as precise as possible)



#let's create 10 more-or-less similar groups based on sorted predicted probabilities and deciles


s_os.1 <- sort(p1.os, index.return=T)  #sort predicted probabilities keeping index referring to rowID in p1.os group
qnt.1 <- quantile(c(s_os.1$x),seq(0,1,0.1))  #deciles for the predicted probabilities to discriminate the groups

group1.1 <- list(s_os.1$x[s_os.1$x <= qnt.1[2]],s_os.1$ix[s_os.1$x <= qnt.1[2]]) #group1.1 is composed by the first decile
obs_s1.1 <- sum(data.os$TenYearCHD[group1.1[[2]]]) #observed successes in group1.1
exp_s1.1 <- sum(group1.1[[1]]) #expected successes in group1.1
obs_f1.1 <- length(group1.1[[1]]) - obs_s1.1 #observed failures
exp_f1.1 <- length(group1.1[[1]]) - exp_s1.1 #expected failures
HL1.1 <- ((obs_s1.1-exp_s1.1)^2/exp_s1.1 + (obs_f1.1-exp_f1.1)^2/exp_f1.1) #Hosmer-Lemeshow statistic for group1.1

group2.1 <- list(s_os.1$x[s_os.1$x > qnt.1[2] & s_os.1$x <= qnt.1[3]],s_os.1$ix[s_os.1$x > qnt.1[2] & s_os.1$x <= qnt.1[3]])
obs_s2.1 <- sum(data.os$TenYearCHD[group2.1[[2]]])
exp_s2.1 <- sum(group2.1[[1]])
obs_f2.1 <- length(group2.1[[1]]) - obs_s2.1
exp_f2.1 <- length(group2.1[[1]]) - exp_s2.1
HL2.1 <- ((obs_s2.1-exp_s2.1)^2/exp_s2.1 + (obs_f2.1-exp_f2.1)^2/exp_f2.1)

group3.1 <- list(s_os.1$x[s_os.1$x > qnt.1[3] & s_os.1$x <= qnt.1[4]],s_os.1$ix[s_os.1$x > qnt.1[3] & s_os.1$x <= qnt.1[4]])
obs_s3.1 <- sum(data.os$TenYearCHD[group3.1[[2]]])
exp_s3.1 <- sum(group3.1[[1]])
obs_f3.1 <- length(group3.1[[1]]) - obs_s3.1
exp_f3.1 <- length(group3.1[[1]]) - exp_s3.1
HL3.1 <- ((obs_s3.1-exp_s3.1)^2/exp_s3.1 + (obs_f3.1-exp_f3.1)^2/exp_f3.1)

group4.1 <- list(s_os.1$x[s_os.1$x > qnt.1[4] & s_os.1$x <= qnt.1[5]],s_os.1$ix[s_os.1$x > qnt.1[4] & s_os.1$x <= qnt.1[5]])
obs_s4.1 <- sum(data.os$TenYearCHD[group4.1[[2]]])
exp_s4.1 <- sum(group4.1[[1]])
obs_f4.1 <- length(group4.1[[1]]) - obs_s4.1
exp_f4.1 <- length(group4.1[[1]]) - exp_s4.1
HL4.1 <- ((obs_s4.1-exp_s4.1)^2/exp_s4.1 + (obs_f4.1-exp_f4.1)^2/exp_f4.1)

group5.1 <- list(s_os.1$x[s_os.1$x > qnt.1[5] & s_os.1$x <= qnt.1[6]],s_os.1$ix[s_os.1$x > qnt.1[5] & s_os.1$x <= qnt.1[6]])
obs_s5.1 <- sum(data.os$TenYearCHD[group5.1[[2]]])
exp_s5.1 <- sum(group5.1[[1]])
obs_f5.1 <- length(group5.1[[1]]) - obs_s5.1
exp_f5.1 <- length(group5.1[[1]]) - exp_s5.1
HL5.1 <- ((obs_s5.1-exp_s5.1)^2/exp_s5.1 + (obs_f5.1-exp_f5.1)^2/exp_f5.1)

group6.1 <- list(s_os.1$x[s_os.1$x > qnt.1[6] & s_os.1$x <= qnt.1[7]],s_os.1$ix[s_os.1$x > qnt.1[6] & s_os.1$x <= qnt.1[7]])
obs_s6.1 <- sum(data.os$TenYearCHD[group6.1[[2]]])
exp_s6.1 <- sum(group6.1[[1]])
obs_f6.1 <- length(group6.1[[1]]) - obs_s6.1
exp_f6.1 <- length(group6.1[[1]]) - exp_s6.1
HL6.1 <- ((obs_s6.1-exp_s6.1)^2/exp_s6.1 + (obs_f6.1-exp_f6.1)^2/exp_f6.1)

group7.1 <- list(s_os.1$x[s_os.1$x > qnt.1[7] & s_os.1$x <= qnt.1[8]],s_os.1$ix[s_os.1$x > qnt.1[7] & s_os.1$x <= qnt.1[8]])
obs_s7.1 <- sum(data.os$TenYearCHD[group7.1[[2]]])
exp_s7.1 <- sum(group7.1[[1]])
obs_f7.1 <- length(group7.1[[1]]) - obs_s7.1
exp_f7.1 <- length(group7.1[[1]]) - exp_s7.1
HL7.1 <- ((obs_s7.1-exp_s7.1)^2/exp_s7.1 + (obs_f7.1-exp_f7.1)^2/exp_f7.1)

group8.1 <- list(s_os.1$x[s_os.1$x > qnt.1[8] & s_os.1$x <= qnt.1[9]],s_os.1$ix[s_os.1$x > qnt.1[8] & s_os.1$x <= qnt.1[9]])
obs_s8.1 <- sum(data.os$TenYearCHD[group8.1[[2]]])
exp_s8.1 <- sum(group8.1[[1]])
obs_f8.1 <- length(group8.1[[1]]) - obs_s8.1
exp_f8.1 <- length(group8.1[[1]]) - exp_s8.1
HL8.1 <- ((obs_s8.1-exp_s8.1)^2/exp_s8.1 + (obs_f8.1-exp_f8.1)^2/exp_f8.1)

group9.1 <- list(s_os.1$x[s_os.1$x > qnt.1[9] & s_os.1$x <= qnt.1[10]],s_os.1$ix[s_os.1$x > qnt.1[9] & s_os.1$x <= qnt.1[10]])
obs_s9.1 <- sum(data.os$TenYearCHD[group9.1[[2]]])
exp_s9.1 <- sum(group9.1[[1]])
obs_f9.1 <- length(group9.1[[1]]) - obs_s9.1
exp_f9.1 <- length(group9.1[[1]]) - exp_s9.1
HL9.1 <- ((obs_s9.1-exp_s9.1)^2/exp_s9.1 + (obs_f9.1-exp_f9.1)^2/exp_f9.1)

group10.1 <- list(s_os.1$x[s_os.1$x > qnt.1[10]],s_os.1$ix[s_os.1$x > qnt.1[10]])
obs_s10.1 <- sum(data.os$TenYearCHD[group10.1[[2]]])
exp_s10.1 <- sum(group10.1[[1]])
obs_f10.1 <- length(group10.1[[1]]) - obs_s10.1
exp_f10.1 <- length(group10.1[[1]]) - exp_s10.1
HL10.1 <- ((obs_s10.1-exp_s10.1)^2/exp_s10.1 + (obs_f10.1-exp_f10.1)^2/exp_f10.1)


totHL.1 <- sum(HL1.1,HL2.1,HL3.1,HL4.1,HL5.1,HL6.1,HL7.1,HL8.1,HL9.1,HL10.1)  #total HL statistic
pval.1 <- 1-pchisq(totHL.1,10-2) #final p-value of HL statistic
# the p-value is 0.758, indicating that the model is very likely to be well calibrated 
# and that we don't have reason to believe that our predictions are different from the real values



# now let's repeat this process but testing the goodness of fit of the second model (on the same testing set)

s_os.2 <- sort(p2.os, index.return=T)
qnt.2 <- quantile(c(s_os.2$x),seq(0,1,0.1))

group1.2 <- list(s_os.2$x[s_os.2$x <= qnt.2[2]],s_os.2$ix[s_os.2$x <= qnt.2[2]])
obs_s1.2 <- sum(data.os$TenYearCHD[group1.2[[2]]])
exp_s1.2 <- sum(group1.2[[1]])
obs_f1.2 <- length(group1.2[[1]]) - obs_s1.2
exp_f1.2 <- length(group1.2[[1]]) - exp_s1.2
HL1.2 <- ((obs_s1.2-exp_s1.2)^2/exp_s1.2 + (obs_f1.2-exp_f1.2)^2/exp_f1.2)

group2.2 <- list(s_os.2$x[s_os.2$x > qnt.2[2] & s_os.2$x <= qnt.2[3]],s_os.2$ix[s_os.2$x > qnt.2[2] & s_os.2$x <= qnt.2[3]])
obs_s2.2 <- sum(data.os$TenYearCHD[group2.2[[2]]])
exp_s2.2 <- sum(group2.2[[1]])
obs_f2.2 <- length(group2.2[[1]]) - obs_s2.2
exp_f2.2 <- length(group2.2[[1]]) - exp_s2.2
HL2.2 <- ((obs_s2.2-exp_s2.2)^2/exp_s2.2 + (obs_f2.2-exp_f2.2)^2/exp_f2.2)

group3.2 <- list(s_os.2$x[s_os.2$x > qnt.2[3] & s_os.2$x <= qnt.2[4]],s_os.2$ix[s_os.2$x > qnt.2[3] & s_os.2$x <= qnt.2[4]])
obs_s3.2 <- sum(data.os$TenYearCHD[group3.2[[2]]])
exp_s3.2 <- sum(group3.2[[1]])
obs_f3.2 <- length(group3.2[[1]]) - obs_s3.2
exp_f3.2 <- length(group3.2[[1]]) - exp_s3.2
HL3.2 <- ((obs_s3.2-exp_s3.2)^2/exp_s3.2 + (obs_f3.2-exp_f3.2)^2/exp_f3.2)

group4.2 <- list(s_os.2$x[s_os.2$x > qnt.2[4] & s_os.2$x <= qnt.2[5]],s_os.2$ix[s_os.2$x > qnt.2[4] & s_os.2$x <= qnt.2[5]])
obs_s4.2 <- sum(data.os$TenYearCHD[group4.2[[2]]])
exp_s4.2 <- sum(group4.2[[1]])
obs_f4.2 <- length(group4.2[[1]]) - obs_s4.2
exp_f4.2 <- length(group4.2[[1]]) - exp_s4.2
HL4.2 <- ((obs_s4.2-exp_s4.2)^2/exp_s4.2 + (obs_f4.2-exp_f4.2)^2/exp_f4.2)

group5.2 <- list(s_os.2$x[s_os.2$x > qnt.2[5] & s_os.2$x <= qnt.2[6]],s_os.2$ix[s_os.2$x > qnt.2[5] & s_os.2$x <= qnt.2[6]])
obs_s5.2 <- sum(data.os$TenYearCHD[group5.2[[2]]])
exp_s5.2 <- sum(group5.2[[1]])
obs_f5.2 <- length(group5.2[[1]]) - obs_s5.2
exp_f5.2 <- length(group5.2[[1]]) - exp_s5.2
HL5.2 <- ((obs_s5.2-exp_s5.2)^2/exp_s5.2 + (obs_f5.2-exp_f5.2)^2/exp_f5.2)

group6.2 <- list(s_os.2$x[s_os.2$x > qnt.2[6] & s_os.2$x <= qnt.2[7]],s_os.2$ix[s_os.2$x > qnt.2[6] & s_os.2$x <= qnt.2[7]])
obs_s6.2 <- sum(data.os$TenYearCHD[group6.2[[2]]])
exp_s6.2 <- sum(group6.2[[1]])
obs_f6.2 <- length(group6.2[[1]]) - obs_s6.2
exp_f6.2 <- length(group6.2[[1]]) - exp_s6.2
HL6.2 <- ((obs_s6.2-exp_s6.2)^2/exp_s6.2 + (obs_f6.2-exp_f6.2)^2/exp_f6.2)

group7.2 <- list(s_os.2$x[s_os.2$x > qnt.2[7] & s_os.2$x <= qnt.2[8]],s_os.2$ix[s_os.2$x > qnt.2[7] & s_os.2$x <= qnt.2[8]])
obs_s7.2 <- sum(data.os$TenYearCHD[group7.2[[2]]])
exp_s7.2 <- sum(group7.2[[1]])
obs_f7.2 <- length(group7.2[[1]]) - obs_s7.2
exp_f7.2 <- length(group7.2[[1]]) - exp_s7.2
HL7.2 <- ((obs_s7.2-exp_s7.2)^2/exp_s7.2 + (obs_f7.2-exp_f7.2)^2/exp_f7.2)

group8.2 <- list(s_os.2$x[s_os.2$x > qnt.2[8] & s_os.2$x <= qnt.2[9]],s_os.2$ix[s_os.2$x > qnt.2[8] & s_os.2$x <= qnt.2[9]])
obs_s8.2 <- sum(data.os$TenYearCHD[group8.2[[2]]])
exp_s8.2 <- sum(group8.2[[1]])
obs_f8.2 <- length(group8.2[[1]]) - obs_s8.2
exp_f8.2 <- length(group8.2[[1]]) - exp_s8.2
HL8.2 <- ((obs_s8.2-exp_s8.2)^2/exp_s8.2 + (obs_f8.2-exp_f8.2)^2/exp_f8.2)

group9.2 <- list(s_os.2$x[s_os.2$x > qnt.2[9] & s_os.2$x <= qnt.2[10]],s_os.2$ix[s_os.2$x > qnt.2[9] & s_os.2$x <= qnt.2[10]])
obs_s9.2 <- sum(data.os$TenYearCHD[group9.2[[2]]])
exp_s9.2 <- sum(group9.2[[1]])
obs_f9.2 <- length(group9.2[[1]]) - obs_s9.2
exp_f9.2 <- length(group9.2[[1]]) - exp_s9.2
HL9.2 <- ((obs_s9.2-exp_s9.2)^2/exp_s9.2 + (obs_f9.2-exp_f9.2)^2/exp_f9.2)

group10.2 <- list(s_os.2$x[s_os.2$x > qnt.2[10] & s_os.2$x <= qnt.2[11]],s_os.2$ix[s_os.2$x > qnt.2[10] & s_os.2$x <= qnt.2[11]])
obs_s10.2 <- sum(data.os$TenYearCHD[group10.2[[2]]])
exp_s10.2 <- sum(group10.2[[1]])
obs_f10.2 <- length(group10.2[[1]]) - obs_s10.2
exp_f10.2 <- length(group10.2[[1]]) - exp_s10.2
HL10.2 <- ((obs_s10.2-exp_s10.2)^2/exp_s10.2 + (obs_f10.2-exp_f10.2)^2/exp_f10.2)


totHL.2 <- sum(HL1.2,HL2.2,HL3.2,HL4.2,HL5.2,HL6.2,HL7.2,HL8.2,HL9.2,HL10.2)
pval.2 <- 1-pchisq(totHL.2,10-2)
# p-value is 0.736, still quite high which reinforces our beliefs about the model being well calibrated!





# ROC curve (in sample)

# the ROC curve plots Sensitivity (TPR = TP/(TP+FN)) over Specificity (TNR = TN/(TN+FP))
pdf("ROCfit.pdf")
roc(data.is$TenYearCHD, log.fit1$fitted.values, plot=TRUE,main='log.fit1')  #Receiver Operating Curve for first model predicting CHD risk in training sample

roc(data.is$TenYearCHD, log.fit2$fitted.values, plot=TRUE,main="log.fit2")  #same thing but for the second model
dev.off()
#these two above are both in-sample training and testing



#ROC curve on out of sample data

#Model testing

#Full model
log.fitos1<- glm(TenYearCHD ~., data=data.os, family="binomial")  #remodel on out-of-sample data this time
summary(log.fitos1)

pdf("ROCtest1.pdf")
roc(data.os$TenYearCHD, log.fitos1$fitted.values, plot=TRUE, main="Model log.fitos1 performance out of sample")
#out-of-sample training and testing
dev.off()

#Model with important variables
log.fitos2<- glm(TenYearCHD ~ male + age + cigsPerDay + sysBP + glucose, data=data.os, family="binomial")
summary(log.fitos2)

pdf("ROCtest2.pdf")
roc(data.os$TenYearCHD, log.fitos2$fitted.values, plot=TRUE, main="Model log.fitos2 performance out of sample")
#again out-of-sample training and testing
dev.off()



# Now let's take a look at the ROC curve for both models when we fit on the training set and test on the testing set (which makes the most sense)


#Full model

p1.os <- predict(log.fit1, newdata=data.os, type="response")

pdf("ROCcurve1.pdf")
roc(data.os$TenYearCHD, p1.os, plot=TRUE,main="Model log.fit1 performance out of sample")
dev.off()


#Second (short) model

p2.os <- predict(log.fit2, newdata=data.os, type="response")

pdf("ROCcurve2.pdf")
roc(data.os$TenYearCHD, p2.os, plot=TRUE,main="Model log.fit2 performance out of sample")
dev.off()


