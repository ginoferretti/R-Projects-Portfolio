#household

#Importing data set
#getwd() if we do not know the current working directory

household=read.table(file.choose(), header=T)
household
plot(household)
summary(household)
str(household)


#from data frame to tables to work with data for males and females
dfm<-as.matrix(household); dft<-as.table(dfm)#doing this here if we need the whole set of data in the future

household_female<- household[household$gender=="female",]
household_female
females<-as.matrix(household_female[,1:4]); female_exp<-as.table(females) #did this series of assignments in order to work properly with numeric values and using directly labels
female_exp

household_male<- household[household$gender=="male",]
household_male
males<- as.matrix(household_male[,1:4]); male_exp<-as.table(males) #did this series of assignments in order to work properly with numeric values and using directly labels
male_exp


#Pie charts for males and females in order to see a "practical"(or more handy) distribution of their expenses

par(mfcol=c(1,2))
pie(colSums(female_exp),main = "Female expenses")
pie(colSums(male_exp),main = "Male expenses")

#We can easily detect some good information by the graphical representation above

#But who spends more money between men and women in total?
total_female_exp<-sum(colSums(female_exp))
total_female_exp

total_male_exp<-sum(colSums((male_exp)))
total_male_exp

if (total_female_exp > total_male_exp) {
  print("Females spend more money than males in this data set! Their expenses are equal to : ",quote = F)
  print(total_female_exp)
}else {
  print("Males spend more money than females in this data set! Their expenses are equal to (in HK$): ",quote = F)
  print(total_male_exp)
  print("Females spend instead (in HK$) : ", quote = F)
  print(total_female_exp)
}

#Averages

#How much money does every woman of the set spend on average in general and for each different cause?
tot_avg_female<-mean(female_exp)#general
tot_avg_female

housingf<-female_exp[,"housing"]
hf<-mean(housingf)#avg housing expenses

foodf<-female_exp[,"food"]
ff<-mean(foodf)#avg food expenses

goodsf<-female_exp[,"goods"]
gf<-mean(goodsf)#avg goods expenses

servicef<-female_exp[,"service"]
sf<-mean(servicef)#avg service expenses

labels_avg<-c("avg_housing","avg_food","avg_goods","avg_service") #labels for the averages
avgf<-c(hf,ff,gf,sf)
female_avg_exp<-rbind(labels_avg,avgf)
female_avg_exp #binding together outputs to get a clear picture of the values
 

#How much money does every man of the set spend on average in general and for each different cause?

tot_avg_male<-mean(male_exp)#general
tot_avg_male

housingm<-male_exp[,"housing"]
hm<-mean(housingm) #avg housing expenses

foodm<-male_exp[,"food"]
fm<-mean(foodm) #avg food expenses

goodsm<-male_exp[,"goods"]
gm<-mean(goodsm) #avg goods expenses

servicem<-male_exp[,"service"]
sm<-mean(servicem) #avg service expenses

avgm<-c(hm,fm,gm,sm)
male_avg_exp<-rbind(labels_avg,avgm)
male_avg_exp #binding together outputs to get a clear picture of the values


#So now we have verified that the conclusions we did before are true (by using the means of the categories)

#Hypothesis testing
#mu1=female's mu
#mu2=male's mu

# H0: mu1 = mu2
# H1: mu1 != mu2

hist(females)
qqnorm(females)#normality check
summary(females)
boxplot(females)

hist(males)
qqnorm(males)#normality check
summary(males)
boxplot(males)

t.test(female_exp,male_exp)#rejecting null hypo, alpha = 5%

#H1: mu1>mu2

t.test(female_exp, male_exp, alternative = "grater")#alpha is 5%

#Using regression models

library(car)#package already installed on my computer, so I do not wrote a line of code do install it

mod <- lm(housing  ~  . -gender , data= df)
avPlots(mod)



#Calculate the correlation between the variables to identify which variables are correlated and how.
cor(household[-5])
#The most high correlations are: service-housing(0.85),service-goods(0.88),housing-goods(0.72)

plot(household$goods,household$sevice)
plot(household$housing,household$sevice)
plot(household$housing,household$goods)

#Now we can produce some simple regression models between the variables.
reg1=lm(housing~goods, data=household) 
plot(household$service,household$housing)
abline(reg1, col = "red")
summary(reg1)
summary(reg1)$adj.r.squared
#The value of the adjusted r.squared isn't an ideal value to consider this one a good model. 
#Moreover, the plot represent the same result.

housing=a+b*service
reg2=lm(housing~service, data=household)
plot(household$housing,household$service)
abline(reg2, col = "red")
summary(reg2)
summary(reg2)$adj.r.squared
#This is a better model than the previous one.

reg3=lm(service~goods, data=household) 
plot(household$goods, household$service)
abline(reg3, col = "red")
summary(reg3)
summary(reg3)$adj.r.squared
#This is the most efficient model because the value of the adjusted r.squared is more high that the others one.

# Now, we have chosen the model called reg3. 
#We can calculate the fitted values.
cbind(reg3$fitted.values,
      cbind(1, household$goods) %*% summary(reg3)$coefficients[,1])
reg3$fitted.values
#We can see the linear model and the fitted values in a plot.
plot(household$goods, household$service,xlim=c(0,3000))
abline(reg3, col = "red")
points(household$goods, reg3$fitted.values, col = "red")

#The residuals of the model can be seen in the plot.
cbind(residuals(reg3), household$service - reg3$fitted.values) #or
residuals(reg3) 
segments(household$goods, reg3$fitted.values,
         household$goods, household$service, lty = 3) 
#We can see that for the points more far from the other points (outliers), the value of the residual is higher. 

#We can calculate the predicted value of the variable service 
#if for example the value of the variable goods is equal to 500. 
reg3$coefficients[1] + 500 * reg3$coefficients[2] #(y=a+x*b)
predict(reg3, newdata = rbind(household, c(NA,NA, 500,NA,NA))[nrow(household)+1,])
points(500, predict(reg3, newdata = rbind(household, c(NA,NA, 500,NA,NA))[nrow(household)+1,]),
       col = "blue",xlim=c(0,1000), ylim=c(0,500))

# T test
summary(reg3)$coefficients
summary(reg3)$coefficients[,1] / summary(reg3)$coefficients[,2] #the t-values
2 * pt(abs(summary(reg3)$coefficients[,3]), 
       nrow(household) - length(reg3$coefficients),lower.tail = F) #the probabilities

#Only the variable food can be explained by the variable gender.
reg=lm(food~gender, data=household)
summary(reg)

summary(lm(goods~gender, data=household))
summary(lm(service~gender, data=household))
summary(lm(housing~gender, data=household))

#Multiple regression.
reg4=lm(household$service~household$goods+ household$gender) 
reg5=update(reg4, . ~ . + household$housing)
reg6=update(reg5, . ~ . + household$food)
summary(reg4)$adj.r.squared
summary(reg5)$adj.r.squared #the previous model reg4 can be improve by the model reg5 adding the variable housing.
summary(reg6)$adj.r.squared #after adding the variable food in the model, the adj.r squared improve only by 0.0001 and the '***' 
summary(reg4)
summary(reg5)
summary(reg6)

reg7=lm(household$housing ~household$goods+ household$gender+ household$service + household$food)
reg8=lm(household$goods~household$housing + household$gender+ household$service)
reg9=lm(household$goods~ household$gender+ household$service)
summary(reg7)
summary(reg8)
summary(reg9)

ls(reg5)
reg5$coefficients
reg5$fitted.values

e.y=reg5$residuals

#avPlots
install.packages("car")
library(car)
avPlots(reg5)

#avPlots for service and goods 
goods.others=update(reg5, .~. - household$goods)
pi.goods.others=lm(household$goods~ household$gender+ household$housing)
goods.mod=lm(goods.others$residuals ~ pi.goods.others$residuals)
goods.mod
plot(pi.goods.others$residuals, goods.others$residuals)
abline(goods.mod, col="blue")

#Chisq test
#Start with the function found in the first part

female_avg=avgf
male_avg=avgm


#create a new tab for testing
household.tab=rbind(female_avg,male_avg)
colnames(household.tab) <- c("housing","food","goods","service")
rownames(household.tab) <- c("Female", "Male")
household.tab

#graphically
barplot(prop.table(t(household.tab), 2), beside= T,
        legend.text = colnames(household.tab),
        col = c("black","grey80","grey50","white"))

chisq.test(household.tab) #reject hyp of statistical independence
X2_test <- chisq.test(household.tab)

ls(X2_test)

# Observed counts
X2_test$observed

# Expected counts
X2_test$expected

# dimostration
nij_exp <- matrix(NA, 2, 4)
ni <- rowSums(household.tab)
nj <- colSums(household.tab)
n <- sum(household.tab)


# For loop


for(i in 1:nrow(household.tab)){
  for(j in 1:ncol(household.tab)){
    nij_exp[i,j] <- ni[i]*nj[j]/n  
  }
}

# Chi-squared test statistic
X2_test$statistic
sum((X2_test$observed - X2_test$expected)^2/X2_test$expected)

# Degrees of freedom
X2_test$parameter
prod(dim(household.tab)-1)

# p-value
X2_test$p.value # reject 
pchisq(X2_test$statistic, df = 3, lower.tail = F)

x <- seq(0, 350, 0.01)
plot(x, dchisq(x, df = prod(dim(household.tab)-1)), type = "l") 
abline(h = 0) # horizontal line in zero
segments(qchisq(0.95, 3),0,
         qchisq(0.95, 3), dchisq(qchisq(0.95, 3), 3), col = "red") # rejection region
points(X2_test$statistic, 0, col = "red")
