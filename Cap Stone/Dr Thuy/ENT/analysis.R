#Read data
data <- read.csv("cleaning_data.csv")
surg <- read.csv("surgery_time.csv")

#Baseline
noseR.0<- data$noseR[data$follow_up=="0"]
noseL.0<- data$noseL[data$follow_up=="0"]
nose.0<-c(noseR.0,noseL.0)
AngR.0<- data$AngR[data$follow_up=="0"]
AngL.0<- data$AngL[data$follow_up=="0"]
Ang.0<-c(AngR.0,AngL.0)
CSAminR.0<- data$CSAminR[data$follow_up=="0"]
CSAminL.0<- data$CSAminL[data$follow_up=="0"]
CSAmin.0<-c(CSAminR.0,CSAminL.0)

#First month
noseR.1<- data$noseR[data$follow_up=="1"]
noseL.1<- data$noseL[data$follow_up=="1"]
nose.1<-c(noseR.1,noseL.1)

CSAminR.1<- data$CSAminR[data$follow_up=="1"]
CSAminL.1<- data$CSAminL[data$follow_up=="1"]
CSAmin.1<-c(CSAminR.1,CSAminL.1)

#Sixth month
noseR.6<- data$noseR[data$follow_up=="6"]
noseL.6<- data$noseL[data$follow_up=="6"]
nose.6<-c(noseR.6,noseL.6)
AngR.6<- data$AngR[data$follow_up=="6"]
AngL.6<- data$AngL[data$follow_up=="6"]
Ang.6<-c(AngR.6,AngL.6)
CSAminR.6<- data$CSAminR[data$follow_up=="6"]
CSAminL.6<- data$CSAminL[data$follow_up=="6"]
CSAmin.6<-c(CSAminR.6,CSAminL.6)

#Surgery time no L
time.0 <- surg$time[surg$group=="0"]
time.1 <- surg$time[surg$group=="1"]
#Exploratory analysis
library(psych)
describe(nose.0)
describe(nose.1)
describe(nose.6)
describe(Ang.0)
describe(Ang.6)
describe(CSAmin.0)
describe(CSAmin.1)
describe(CSAmin.6)
describe(time.0)
describe(time.1)

##############
nose.data <- data[,c("id","nose","follow_up")]
boxplot(nose.data$nose ~ nose.data$follow_up, names=c("Trước mổ","Sau mổ 1 tháng","Sau  mổ 6 tháng"),xlab="Thời gian",ylab="Độ nghẹt mũi"
        ,main= "Độ nghẹt mũi theo thời gian")
ang.data <- na.omit(data[,c("id","Ang","follow_up")])
boxplot(ang.data$Ang ~ ang.data$follow_up, names=c("Trước mổ","Sau  mổ 6 tháng"),xlab="Thời gian",ylab="Góc van mũi"
        ,main= "Góc van mũi theo thời gian")
cs.data <- na.omit(data[,c("id","CSAmin","follow_up")])
boxplot(cs.data$CSAmin ~ cs.data$follow_up, names=c("Trước mổ","Sau mổ 1 tháng","Sau  mổ 6 tháng"),xlab="Thời gian",ylab="CSAmin"
        ,main= "CSAmin theo thời gian")
boxplot(surg$time ~ surg$group, names=c("Không ghép mảnh L", "Có ghép mảnh L"),xlab="Nhóm",ylab="Thời gian (phút)"
        ,main= "Thời gian phẫu thuật theo nhóm",col=c("gold","green"))
######################################
aov.nose = aov(nose ~ follow_up + Error(id/follow_up), data=nose.data)
summary(aov.nose)
with(nose.data, pairwise.t.test(nose,follow_up,p.adjust.method="holm", paired=T))

with(ang.data,t.test(Ang~follow_up,paired=T))

aov.cs = aov(CSAmin ~ follow_up + Error(id/follow_up), data=cs.data)
summary(aov.cs)
with(cs.data, pairwise.t.test(CSAmin,follow_up,p.adjust.method="holm", paired=T))

with(surg,t.test(time~group))
#################################################################
# Update data set 
#################################################################
data<- read.csv("new_data.csv")
head(data)
library(psych)
vars<- data[,c("nose","ang","csamin")]
groups<- data [,c("side","follow_up")]
describeBy(vars,groups)
describeBy(vars,data$follow_up)

#####################
library(gplots)
fit <- aov(nose~side*follow_up + Error(id/follow_up),data)
summary(fit)
with(data,interaction.plot(follow_up,side,nose,type="b",col=c("red","blue"),pch=c(16,18),lwd=2,xlab="Tháng",ylab="Độ nghẹt mũi trung bình"))

csamin.data<- na.omit(data[,c("id","csamin","side","follow_up")])
fit <- aov(csamin~side*follow_up + Error(id/follow_up),csamin.data)
summary(fit)
with(csamin.data,interaction.plot(follow_up,side,csamin,type="b",col=c("red","blue"),pch=c(16,18),ylim=range(c(0,80)),lwd=2,xlab="Tháng",ylab="CSAmin trung bình"))
with(csamin.data,plotmeans(csamin~interaction(side,follow_up,sep=" "),col=c("red","darkgreen"),connect=list(c(1,3,5)
    ,c(2,4,6)),main="Interaction plot with 95% CI",xlab="Bên mũi * thời gian theo dõi"))


ang.data<- na.omit(data[,c("id","ang","side","follow_up")])
fit <- aov(ang~side*follow_up + Error(id/follow_up),ang.data)
summary(fit)
with(ang.data,interaction.plot(follow_up,side,ang,type="b",col=c("red","blue"),pch=c(16,18),lwd=2,ylab="Góc van mũi trong trung bình",xlab="Tháng"))
with(ang.data,plotmeans(ang~interaction(side,follow_up,sep=" "),col=c("red","darkgreen"),connect=list(c(1,3,5)
    ,c(2,4,6)),main="Interaction plot with 95% CI",xlab="Bên mũi * thời gian theo dõi",mean.labels=TRUE,digits=2))


ang.0 <- subset(ang.data,follow_up=="0")
with(ang.0,t.test(ang~side))
ang.6 <- subset(ang.data,follow_up=="6")
with(ang.6,t.test(ang~side))


boxplot(nose ~ side*follow_up,data=data,col=c("gold","green"))
boxplot(csamin ~ side*follow_up,data=csamin.data,col=c("gold","green"))
boxplot(ang ~ side*follow_up,data=ang.data,col=c("gold","green"))

## Final analysis
boxplot(data$nose ~ data$follow_up, names=c("Trước mổ","Sau mổ 1 tháng","Sau  mổ 6 tháng"),xlab="Thời gian",ylab="Độ nghẹt mũi"
        ,main= "Độ nghẹt mũi theo thời gian")
aov.nose = aov(nose ~ follow_up + Error(id/follow_up), data=data)
summary(aov.nose)
with(data, pairwise.t.test(nose,follow_up,p.adjust.method="holm", paired=T))

with(ang.data,t.test(ang~follow_up,paired=T))
boxplot(ang.data$ang ~ ang.data$follow_up, names=c("Trước mổ","Sau  mổ 6 tháng"),xlab="Thời gian",ylab="Góc van mũi"
        ,main= "Góc van mũi theo thời gian",col=c("gold","green"))


aov.cs = aov(csamin ~ follow_up + Error(id/follow_up), data=csamin.data)
summary(aov.cs)
with(csamin.data, pairwise.t.test(csamin,follow_up,p.adjust.method="holm", paired=T))
boxplot(csamin.data$csamin ~ csamin.data$follow_up, names=c("Trước mổ","Sau mổ 1 tháng","Sau  mổ 6 tháng"),xlab="Thời gian",ylab="CSAmin"
        ,main= "CSAmin theo thời gian",col=c("gold","green","red"))
##########################################################################
#data4
data <- read.csv("data4.csv")
library(psych)
vars<- data[,c("nose","ang","csamin")]
groups<- data [,c("group","follow_up")]
describeBy(vars,groups)

fit <- aov(nose~group*follow_up + Error(id/follow_up),data)
summary(fit)
with(data,interaction.plot(follow_up,group,nose,type="b",col=c("red","blue"),pch=c(16,18),lwd=2,xlab="Th?ng",ylab="D??? ngh???t mui trung b?nh"))
ang.data<- na.omit(data[,c("id","ang","group","follow_up")])
fit <- aov(ang~group*follow_up + Error(id/follow_up),ang.data)
summary(fit)
with(ang.data,interaction.plot(follow_up,group,ang,type="b",col=c("red","blue"),pch=c(16,18),lwd=2,ylab="G?c van mui trong trung b?nh",xlab="Th?ng"))

csamin.data<- na.omit(data[,c("id","csamin","group","follow_up")])
fit <- aov(csamin~group*follow_up + Error(id/follow_up),csamin.data)
summary(fit)
with(csamin.data,interaction.plot(follow_up,group,csamin,type="b",col=c("red","blue"),pch=c(16,18),ylim=range(c(0,80)),lwd=2,xlab="Th?ng",ylab="CSAmin trung b?nh"))
##################################

data <- read.csv("data5.csv")
data <- data[,c("group","snsm","mcgm","tmvn","nt","snm","sx","dhm")]
data1<-data[,c(2,3)]
names(data1[2]) <- 'swelling_time'
colnames(data1)[2]<-"swelling_time"
