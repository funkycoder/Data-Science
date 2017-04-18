#--------------------------------------------------------------------------------------------------------
#          PERMUTATION / BOOTSTRAPPING
#--------------------------------------------------------------------------------------------------------
#Independent two-sample and k-sample tests
#--------------------------------------------------------------------------------------------------------
library(coin)
score <- c(40,57,45,55,58,57,64,55,62,65)
treatment<- factor(c(rep("A",5),rep("B",5)))
mydata<- data.frame(treatment,score)
head(mydata)
t.test(score~treatment,data=mydata,var.equal=TRUE)
#now permutation test
oneway_test(score~treatment,data=mydata,distribution="exact")

#more code
library(MASS)
UScrime <- transform(UScrime, So= factor(So))
wilcox_test (Prob ~ So,data=UScrime,distribution = "exact")

#k-sample test
library(multcomp)
set.seed(1234)
oneway_test(response~trt,data=cholesterol,distribution=approximate(B=9999))

#-----------------------------------------------------------------------------------------------------------
# Independence in contingency tables
#---------------------------------------------------------------------------------------------------------
library(coin)
library(vcd)
#convert Improved from orederd factor to categorical factor
Arthritis <- transform(Arthritis,Improved= as.factor(as.numeric(Improved)))
set.seed(1234)
chisq_test(Treatment~Improved, data = Arthritis, distribution = approximate(B=9999))

#-----------------------------------------------------------------------------------------------------------
# Independence between numeric variables
#---------------------------------------------------------------------------------------------------------
states<- as.data.frame(state.x77)
set.seed(1234)
spearman_test(Illiteracy~Murder, data= states, distribution=approximate(B=9999))

#-----------------------------------------------------------------------------------------------------------
# Dependence two-sample and k-sample tests
#---------------------------------------------------------------------------------------------------------
library(coin)
library(MASS)
wilcoxsign_test(U1~U2, data=UScrime, distribution= "exact")
#---------------------------------------------------------------------
# Permutation with the lmPerm package
#------------------------------------------------------------------------
#Simple and polynomial regression
#Iter : how many iteration required to reach stopping rule
library(lmPerm)
set.seed(1234)
fit <- lmp(weight ~height,data=women,perm="Prob")
summary(fit)
#fit quadratic equation
fit<- lmp(weight~height + I(height^2),data=women,perm="Prob")
summary(fit)
#Multiple regression
#Looking back to chapter 8, both Population and Illiteracy are significant
#when normal theory is used. Assumption of normality is untenable or outliers?
library(lmPerm)
set.seed(1234)
states<- as.data.frame(state.x77)

#One way ANOVA
library(lmPerm)
library(multcomp)
set.seed(1234)
fit<- aovp(response~trt,data=cholesterol,perm="Prob")
summary(fit)
fit<- lmp(Murder~Population + Illiteracy + Income+ Frost,data=states,perm="Prob")
summary(fit)

#one way ANCOVA
library(lmPerm)
set.seed(1234)
fit <- aovp(weight ~ gesttime + dose, data=litter, perm="Prob")
summary(fit)

#two way ANOVA
library(lmPerm)
set.seed(1234)
fit <- aovp(len~supp*dose, data=ToothGrowth, perm="Prob")
summary(fit)

#------------------------------------------------------------------------------
# BOOTSTRAPPING
#-------------------------------------------------------------------------------
#One statistic
rsq <- function(formula, data, indices) {
        d <- data[indices,]
        fit <- lm(formula, data=d)
        return(summary(fit)$r.square)
}
library(boot)
set.seed(1234)
results <- boot(data=mtcars, statistic=rsq,
                R=1000, formula=mpg~wt+disp)
print(results)
plot(results)
boot.ci(results, type=c("perc", "bca"))
#Several statistics of regression coefficients:
bs <- function(formula, data, indices) {
        d <- data[indices,]
        fit <- lm(formula, data=d)
        return(coef(fit))
}
library(boot)
set.seed(1234)
results <- boot(data=mtcars, statistic=bs,
                R=1000, formula=mpg~wt+disp)
print(results)
plot(results, index=2)
boot.ci(results, type="bca", index=2)