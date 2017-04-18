#Read Vitamin D dataset in
data<- read.csv("http://files.figshare.com/1542043/vitaminD.csv",header=T,na.strings=" ")
#Descriptive statistics
library(psych)
names(data)
str(data)
#dont display skewness and kurtosis
describe(data,skew=F)
attach(data)
#Descriptive stats by sex / No p-value provided
describeBy(data,sex,skew=F)
#Plot data
par(mfrow=c(2,2))
hist(vitd)
hist(pth)
hist(bmi)
hist(age)
#What is vitD deficency?
def = vitd
def= replace(def,vitd<20.0,1)
def= replace(def,vitd>=20.0,0)
#Descriptive stats for categorical vars
library(gmodels)
CrossTable(def,sex,digits=3,chisq=T,fisher=T)
CrossTable(def,season,digits=3,chisq=T,fisher=T)
CrossTable(def,ruralurban,digits=3,chisq=T,fisher=T)
#Risk factors
library(epicalc)
logistic.display(glm(def~ sex,family=binomial))
logistic.display(glm(def~ ruralurban,family=binomial))
logistic.display(glm(def~ bmi,family=binomial))
logistic.display(glm(def~ factor(season),family=binomial))
logistic.display(glm(def~ age + sex + bmi+ ruralurban+ factor(season),family=binomial))
#regression
library(MASS)
summary(glm.nb(def~age+sex+ruralurban,data=data))
#stepwise regression
fit= lm(vitd~ age+bmi+sex+factor(season),data=data)
step= stepAIC(fit,direction="both")
step$anova
#Calculate relative importance for each predictor variable
library(relaimpo)
fit=lm(vitd~ age+bmi+sex+factor(season),data=data)
calc.relimp(fit,rela=T)
#Bootstrap measure of relative importance (1000 samples)
library(boot)
boot=boot.relimp(fit,b=1000,rank=T,diff=T,rela=T)
booteval.relimp(boot)
#print result
plot(booteval.relimp(boot,sort=T))
#the choosen model
fit=lm(vitd~ age+sex+factor(season),data=data)
summary(fit)
#VitD and PTH
library(car)
scatterplot(vitd~pth|sex, pch=c(1,15),xlab="PTH",ylab="25(OH)D")
fit=lm(vitd~pth,data=data)
summary(fit)
fit=lm(vitd~pth+sex+pth:sex,data=data)
summary(fit)
#OVERVIEW
library(psych)
dd=cbind(age,bmi,vitd,pth)
pairs.panels(dd)
#Error plot
error.bars.by(vitd,sex,bars=T,labels=c("Men","Women"),ylab="25(OH)D",xlab=" ",ylim=c(0,30))
