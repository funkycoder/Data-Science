#Read data
setwd("E:/DropBox/R")
data <- read.csv("ORA.csv")
data$group<- as.factor(data$group)
data$ch_diff<- data$ch_2y-data$ch_pre
data$crf_diff<- data$crf_2y-data$crf_pre
head(data)
summary(data)
library(psych)
describeBy(data,data$group)
describe(data)
hist(data$ad,breaks=16,main="Biểu đồ tần suất AD",xlab="AD")
hist(data$ch_pre,breaks=16,main="Biểu đồ tần suất CH trước mổ",xlab="CH")
hist(data$ch_2y,breaks=16,main="Biểu đồ tần suất CH sau mổ 2 năm",xlab="CH")
hist(data$crf_pre,breaks=16,main="Biểu đồ tần suất CRF trước mổ",xlab="CRF")
hist(data$crf_2y,breaks=16,main="Biểu đồ tần suất CRF sau mổ 2 năm",xlab="CRF")


plot(data$age,data$ch_diff,xlab="Tuổi",ylab="Khác biệt CH trước và sau mổ 2 năm",main="Khác biệt CH trước và sau mổ 2 năm theo tuổi")
abline(v=21.5,lty=2,col="blue")
abline(h=0,lty=2,col="red")

plot(data$age,data$crf_diff,xlab="Tuổi",ylab="Khác biệt CRF trước và sau mổ 2 năm",main="Khác biệt CRF trước và sau mổ 2 năm theo tuổi")
abline(v=21.5,lty=2,col="blue")
abline(h=0,lty=2,col="red")

boxplot(ad~group,data=data,varwidth=TRUE,names=c("Dưới 21 tuổi","Từ 21 tuổi trở lên"),col=c("gold","green"),main="AD theo nhóm tuổi")
t.test(data$ad~data$group)

hist(data$ch_diff,breaks=10,main="Biểu đồ tần suất khác biệt CH trước và sau mổ 2 năm",xlab="Khác biệt CH")
boxplot(ch_diff~group,data=data,varwidth=TRUE,names=c("Dưới 21 tuổi","Từ 21 tuổi trở lên"),col=c("gold","green")
        ,ylab="Khác biệt CH trước và sau mổ 2 năm",main="Khác biệt CH trước và sau mổ 2 năm theo nhóm tuổi")
t.test(data$ch_diff~data$group)


hist(data$crf_diff,breaks=10,main="Biểu đồ tần suất khác biệt CRF trước và sau mổ 2 năm",xlab="Khác biệt CRF")
boxplot(crf_diff~group,data=data,varwidth=TRUE,names=c("Dưới 21 tuổi","Từ 21 tuổi trở lên"),col=c("gold","green")
        ,ylab="Khác biệt CRF trước và sau mổ 2 năm",main="Khác biệt CRF trước và sau mổ 2 năm theo nhóm tuổi")
t.test(data$crf_diff~data$group)


library(HH)
ancova(ch_diff ~ ad + group,data=data,main="Quan hệ giữa khác biệt CH với AD và nhóm tuổi")
#####################
# Regression analysis
#####################
data<- read.csv("ora2.csv")
data<- data[,c("DCRF2y","DIOPg2y","DIOPcc2y","DCH2y","AGE","PACHY","WTW","AD","SE","K","RSB")]
library(car)
scatterplotMatrix(data,spread=FALSE,lty.smooth=2,main="Scatter Plot Matrix")
library(leaps)


leaps<- regsubsets(DCRF2y~AGE+PACHY+WTW+AD+SE+K+RSB,data=data,nbest=4)
plot(leaps,scale="adjr2")
fit <-lm(DCRF2y ~ PACHY+WTW+AD+K+RSB,data=data)
summary(fit)

leaps<- regsubsets(DIOPg2y~AGE+PACHY+WTW+AD+SE+K+RSB,data=data,nbest=4)
plot(leaps,scale="adjr2")
fit <-lm(DIOPg2y ~ PACHY+RSB,data=data)
summary(fit)

leaps<- regsubsets(DIOPcc2y~AGE+PACHY+WTW+AD+SE+K+RSB,data=data,nbest=4)
plot(leaps,scale="adjr2")

fit <-lm(DIOPcc2y ~ AGE+PACHY+WTW+AD+SE+K+RSB,data=data)
summary(fit)

leaps<- regsubsets(DCH2y~AGE+PACHY+WTW+AD+SE+K+RSB,data=data,nbest=4)
plot(leaps,scale="adjr2")
fit <-lm(DCH2y ~ AGE+SE,data=data)
summary(fit)
fit <-lm(DCH2y ~ AGE+SE,data=data)
summary(fit)
scatterplot(DIOPg2y~PACHY,data=data)
scatterplot(DIOPcc2y~PACHY,data=data)
scatterplot(DCRF2y~AD,data=data)
scatterplot(DCH2y~SE,data=data)