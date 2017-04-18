#--------------------------------------------------------------------------
#   ANALYSIS OF VARIANCE
#--------------------------------------------------------------------------
# ANOVA
library(multcomp)
attach(cholesterol)
table(trt)
aggregate(response,by=list(trt),FUN=mean)
aggregate(response,by=list(trt),FUN=sd)
fit<- aov(response ~ trt)
summary(fit)
library(gplots)
plotmeans(response~trt,xlab="Treatment",ylab="Response",main="Mean Plot\n with 95% CI")
detach(cholesterol)
#Multiple comparison
TukeyHSD(fit)
par(las=2)
par(mar=c(5,8,4,2))
plot(TukeyHSD(fit))
#Use multcomp::glht() for more comprehensive set of methods for multicomparison
par(mar=c(5,4,6,2))
tuk<-glht(fit,linfct=mcp(trt="Tukey")) #glht: general linear hypotheses
summary(tuk)
plot(cld(tuk,level=.05),col="lightgrey") #cld: setup a compact letter display of all pair-wise comparison
#Assessing test asumptions
#----------------------------------------------------------------------------
#Normality assumptions
library(car)
qqPlot(lm(response~trt,data=cholesterol),simulate=TRUE,main="Q-Q Plot",labels=FALSE)
#Homogeinity test
bartlett.test(response~trt,data=cholesterol)
fligner.test(response~trt,data=cholesterol) #  Fligner-Killeen test of homogeneity of variances
library(HH)
hov(response~trt,data=cholesterol)#Brown-Forsyth test
#Test for outlier (car library)
outlierTest(aov(response ~ trt)) #NA when p>1
#------------------------------------------------------------------------------
# One way ANCOVA
#-----------------------------------------------------------------------------
data(litter,package="multcomp")
attach(litter)
table(dose)
aggregate(weight,by=list(dose),FUN=mean)
fit<-aov(weight ~  gesttime + dose) #gesttime is covariate
summary(fit)
#Calculate adjusted group means (partialing out the effects of covariate)
#The effects package provide a powerful method of adjusted means for complex research
#design and presenting them visually. See documentation on CRAN!
library(effects)
effect("dose",fit)
#Compare the first group with the average of the other three
library(multcomp)
contrast<- rbind("no drug vs drug"= c(3,-1,-1,-1)) #Multiple Comparisons of Means: User-defined Contrasts
summary(glht(fit,linfct=mcp(dose=contrast)))

#Use multcomp::glht() for more comprehensive set of methods for multicomparison
par(mar=c(5,4,6,2))
tuk<-glht(fit,linfct=mcp(dose="Tukey")) #glht: general linear hypotheses
summary(tuk)
plot(cld(tuk,level=.05),col="lightgrey") #cld: setup a compact letter display of all pair-wise comparison
# Testing nornality and homogeineity are the same with ANOVA
# Testing homogeneity of regression slopes
library(multcomp)
fit2<- aov(weight ~ gesttime*dose,data= litter)
summary(fit2)
# Visualizing the result
library(HH)
ancova(weight ~ gesttime + dose, data= litter)
#!if homogeneity of regression doesn hold (not in this case). Try
ancova(weight ~ gesttime*dose,data=litter)
#--------------------------------------------------------------------------
# Two-way factorial ANOVA
#--------------------------------------------------------------------------
attach(ToothGrowth)
table(supp,dose)
aggregate(len,by=list(supp,dose),FUN=mean)
aggregate(len,by=list(supp,dose),FUN=sd)
fit<- aov(len~supp*dose)
summary(fit)
#Visualization
interaction.plot(dose,supp,len,type="b",col=c("red","blue"),pch=c(16,18),main="Interaction between Dose and Supplement Type")
#gplots::plotmeans
library(gplots)
plotmeans(len~interaction(supp,dose,sep=" "),connect=list(c(1,3,5),c(2,4,6)),
          col=c("red","darkgreen"),main="Interaction Plot with 95%CIs",
          xlab="Treatment and Dose Combination")
#HH:interaction2wt(): plot of both main effects and two-way interactions for any
#factorial design of any order !COOL
library(HH)
interaction2wt(len~supp*dose)
#-----------------------------------------------------------------------------
# REPEATED MEASURES ANOVA
#-----------------------------------------------------------------------------
#Repeated measures ANOVA with one btw and within groups factor
w1b1 <- subset(CO2,Treatment=="chilled")
fit <-aov(uptake ~ conc*Type + Error(Plant/conc),w1b1)
summary(fit)
par(las=2)
par(mar=c(10,4,4,2))
with(w1b1,interaction.plot(conc,Type,uptake,type="b",
                           col=c("red","blue"),pch=c(16,18),
                           main="Interaction Plot for Plant Type and Concentration"))
boxplot(uptake~Type*conc,data=w1b1,col=(c("gold","green")),
        main="Chilled Quebec and Mississipi Plants",
        ylab="Carbon dioxide uptake rate (umol/m^2 sec)")
#------------------------------------------------------------------
# Multivariate analysis of variace (MANOVA)
#---------------------------------------------------------------------------
library(MASS)
attach(UScereal)
y<- cbind(calories,fat,sugars)
aggregate(y,by=list(shelf),FUN=mean)
cov(y)
fit<- manova(y~shelf)
summary(fit)
#univariate one-way ANOVA
#you can use a mean comparision procedure TukeyHSD to determine which shelf differ
summary.aov(fit)
#Assessing test asumption
#-------------------------------
# Multivariate normality
center <- colMeans(y)
n<- nrow(y)
p<- ncol(y)
cov<- cov(y)
d<- mahalanobis(y,center,cov)
coord<- qqplot(qchisq(ppoints(n),df=p),d,
               main="Q-Q plot Assessing Multivariate Normality",
               ylab="Mahalanobis D2")
abline(a=0,b=1)
identify(coord$x,coord$y,labels=row.names(UScereal)) 
#Homogeneicity of variance-covariance: Box's M test <- not available in R
#Multivariate outliers
library(mvoutlier)
outliers<- aq.plot(y)
outliers
#------------------------------------------------------------------------------
# Robust MANOVA
#------------------------------------------------------------------------------
library(rrcov)
Wilks.test(y,shelf,method="mcd")
#-----------------------------------------------------------------------------
# ANOVA as regression
#-----------------------------------------------------------------------------
library(multcomp)
levels(cholesterol$trt)
fit.aov<- aov(response~trt,data=cholesterol)
summary(fit.aov)
fit.lm<- lm(response~trt,data=cholesterol)
summary(fit.lm)
#You can specify a  contrast
fit.lm<- lm(response~trt,data=cholesterol,contrasts="contr.helmert")
#You can change the default contrast used during an R session
#unordered factor to contr.SAS and ordered factor to ontr.helmert
options(contrasts=c("contr.SAS","contr.helmert"))