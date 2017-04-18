# DESCRIPTIVE STATISTICS
# Explore single variables
#---------------------------------------------------------------
# miles per gallon, horsepower, weight
vars <- c("mpg","hp","wt") 
head(mtcars[vars])
#summary : min,max,quartiles, mean or frequencies
summary(mtcars[vars])

# sapply(x,FUN,options)
#x: matrix or dataframe,
#FUN: arbitrary functions mean,sd,var,min,max,median,length,range,quantile,
#options: FUN's arguments

mystats <- function(x,na.omit=FALSE){
  if(na.omit)
    x <- x[!is.na(x)]
  m <- mean(x)
  n <- length(x)
  s <- sd(x)
  skew <- sum((x-m)^3/s^3)/n
  kurt <- sum((x-m)^4/s^4)/n - 3
  return (c(n=n, mean=m, stdev=s,skew=skew,kurtosis=kurt))
}
#testing for fivenum : the results similar to summary
#fivenum()provides Tukey's five-number summary (min,lower-hinge,median,upper-hinge,max)
sapply(mtcars[vars],fivenum)

#mpg mean is 20.1, distribution skewed to the right (+0.61), flatter than a normal distribution (-0.37)
sapply(mtcars[vars],mystats)

#{Hmisc}describe
#return number of variables and observations, missing and unique values
#mean,quantiles and five highest and lowest values
library(Hmisc)
describe(mtcars[vars])

#{pastecs}stat.desc
#stat.desc(x,basic=TRUE,desc=TRUE,norm=FALSE,p=0.95)
#basic=TRUE(default) number of values, null and missing values,min,max,range,sum
#desc=TRUE(default) mean,median,se,95ci,var,sd,coefficient of variation
#norm=TRUE(not default) skewness, kurtosis, Shapiro-Wilk test of normality
#p: confidence interval of the mean (0.95) by default
library(pastecs)
stat.desc(mtcars[vars])

#{psych}describe
# If {Hmisc}describe is masked then you could invoke it by Hmisc::describe()
library(psych)
describe(mtcars[vars])
#---------------------------------------------------------------
# DESCRIPTIVE STATISTICS
# Explore by group
#---------------------------------------------------------------

#aggregate only allow to use single value functions : mean,sd... -> by()
aggregate(mtcars[vars],by=list(am=mtcars$am),mean)
aggregate(mtcars[vars],by=list(am=mtcars$am,cyl=mtcars$cyl),sd)

#by(data,INDICES,FUN)
#INDICES: a factor or list of factors
dstats <- function(x)(c(mean=mean(x),sd=sd(x)))
by(mtcars[vars],mtcars$am,dstats) #!ERROR: by(mtcars$mpg,mtcars$am,dstats) is OK

#{doBy}summaryBy
#summaryBy(formula,data=dataframe,FUN=function)
#formula : var1+var2+...~groupVar1+groupVar2... var:analyzed variables, groupVar1: grouping variables
library(doBy)
summaryBy(mpg+hp+wt~am,data=mtcars,FUN=mystats)

#{psych}describeBy
library(psych)
describeBy(mtcars[vars],mtcars$am)
describeBy(mtcars[vars],list(mtcars$am,mtcars$cyl))
with(mtcars,describeBy(mtcars[vars],list(am,cyl)))

#{reshape} !INTERESTING
#dfm<- melt(dataframe,mesure.vars=y,id.vars=g) y: analyzed variables, g:grouping variables
#cast(dfm,groupvar1+groupvar2... +variable~.,FUN)
library(reshape)
dstats<- function(x)(c(n=length(x),mean=mean(x),sd=sd(x)))
dfm<- melt(mtcars,measure.vars=c("mpg","hp","wt"),id.vars=c("am","cyl"))
cast(dfm,am+cyl+variable~.,dstats)
#---------------------------------------------------------------
# DESCRIPTIVE STATISTICS
# Frequency and contingency table
#---------------------------------------------------------------
#A double-blind RCT of new treatments for arthritis
library(vcd)
head(Arthritis)
#table(var1,var2,...,varN) creates N-way contingency table from N categorical vars (factors)
#table() function ignores missing values (NAs). To include NAs as a valid category useNA="ifany"
mytable <- with(Arthritis,table(Improved))
mytable
#turn these frequencies into proportions with prop.table
prop.table(mytable)
#into percentage
prop.table(mytable)*100
#Two-way tables
#mytable<-table(A,B) : A row vars, B column vars
with(Arthritis,table(Treatment,Improved))
#or mytable<-xtabs(~A+B,data=mydata) : left side of the formula is a vector of frequencies (table already tabulated)
mytable <- xtabs(~Treatment+Improved,data=Arthritis)
mytable
#marginal frequencies and proportions
margin.table(mytable,1) #index 1 : first variable (Row)
prop.table(mytable,1)
#for column sums and sums proportions
margin.table(mytable,2)
prop.table(mytable,2)
#cell proportions
prop.table(mytable)
#marginal sums
addmargins(mytable)
addmargins(prop.table(mytable))
#add a sum column alone
addmargins(prop.table(mytable,1),2)
#add a sum row
addmargins(prop.table(mytable,2),1)
#{gmodels}CrossTable
#help(CrossTable) for more details on chi-square, McNemar, residual values (Pearson,standardized)...
library(gmodels)
CrossTable(Arthritis$Treatment,Arthritis$Improved)
#Multidimensional tables
mytable <- xtabs(~Treatment+Sex+Improved,data=Arthritis)
#print multidimensional tables in a compact and attractive manner
ftable(mytable)
margin.table(mytable)
margin.table(mytable,1)
margin.table(mytable,2)
margin.table(mytable,3)
margin.table(mytable,c(1,3))
ftable(prop.table(mytable,c(1,2)))
ftable(addmargins(prop.table(mytable,c(1,2)),3))
ftable(addmargins(prop.table(mytable,c(1,2)),3))*100
#-------------------------------------------------
# TEST OF INDEPENDENCE
#-------------------------------------------------
#chi-square test
library(vcd)
mytable<- xtabs(~Treatment + Improved, data=Arthritis)
chisq.test(mytable)
#Fisher's exact test (could be applied to any 2 way table nxm)
fisher.test(mytable)
#Cochran-Manten_Haenszel test
#Test whether Treatment and Improved variables are independent within each level of sex
#this test assume there's no 3 way interaction (treatment,improved,sex)
mytable<- xtabs(~Treatment + Improved+Sex, data=Arthritis)
mantelhaen.test(mytable)
#Measure association
#!Take a look also at the kappa() function
mytable<- xtabs(~Treatment + Improved, data=Arthritis)
assocstats(mytable) #this will provide contigency,Phi, Cramers'v
#--------------------------------------
# Correlation
# cor/cov(x=[matrix,dataframe], use=[all.obs,everything,complete.obs,pairwise.obs]),method=[pearson,spearman,kendall]
states<- state.x77[,1:6]
cov(states)
cor(states,method="spearman")
#Now apply to non-square matrices
x <- states[,c("Population","Income","Illiteracy","HS Grad")]
y <- states[,c("Life Exp","Murder")]
cor(x,y) #Result does not tell whether the correlation is difference from 0
#Partial correlation
#Correlation between 2 vars controlling for 1 or more quantitative vars
#ggm:pcor(u,S) u is a vector of indices (first 2 for correlated vars, the remaining are conditioning vars)
# s is covariane matrix
#! See also polycor::hetcor for other type of correlations
library(ggm)
pcor(c(1,5,2,3,6),cov(states)) #pulation ~ murder rate controlling for incom,illiterary,HS Grad
#----------------------------------------
# Testing for correlatin for significance
#cor.test(x,y,alternative=[two.side,less,greater],method=[pearson,kendall,spearman])
#one correlation at a time!
cor.test(states[,3],states[,5])
#Multiple correlation at the same time
library(psych)
corr.test(states,use="complete")
#partial correlation
#psych::pcor.test(r,q,n)
#r:partial correlation produced by pcor()
#q:number of variable to be controlled
#n:sample size