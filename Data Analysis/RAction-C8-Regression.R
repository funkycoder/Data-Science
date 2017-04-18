#Dataset women in the base installation provide the height and weight of 15 women
#ages 30 to 39
#---------------------------------------------------------------------------------
#Simple linear regression
fit <- lm(weight ~ height, data=women) #lm requires a dataframe
summary(fit)
fitted(fit) #List the predicted values in a fitted model
residuals(fit) #List the residual values in the fitted model
plot(women$height,women$weight,xlab="Height (in inches)",ylab="Weight (in pounds)")
abline(fit)
#Polynomial regression
fit2<- lm(weight~height + I(height^2), data = women) 
#I function treats the contents within the parentheses as an R regular expression
#^ operator has a special meaning in formula
summary(fit2)
plot(women$height,women$weight,xlab="Height (in inches)",ylab="Weight (in pounds)")
lines(women$height,fitted(fit2))
##more on polynomial regression
#spread=FALSE : suppress spread and asymmetry information

fit3<- lm(weight~ height + I(height^2) + I(height^3),data=women)
summary(fit3)
plot(women$height,women$weight,xlab="Height (in inches)",ylab="Weight (in pounds)")
lines(women$height,fitted(fit3))
library(car)
scatterplot(weight~height,data=women,spread=FALSE,smoother=loessLine,pch=19,main="Women age 
            30-39",xlab="Height (inches)",ylab="Weight (lbs)")
#Multiple linear regression
#---------------------------------------------------------------------------
states <- as.data.frame(state.x77[,c("Murder","Population","Illiteracy","Income","Frost")])
#Examine the relationship two at a time (bivariate correlations)
cor(states)
library(car)
scatterplotMatrix(states,spread=FALSE,smoother=loessLine,main="Scatter Plot Matrix")
fit<- lm(Murder~Population+Illiteracy+Income+Frost,data=states)
summary(fit)
#Multiple linear regression with interactions
#A significant interaction between 2 predictor variable tell you that the relationship
#btween one predictor and the response variable depends on the level of the other predictor
fit<- lm(mpg~hp+ wt + hp:wt,data=mtcars)
summary(fit)
library(effects)
plot(effect("hp:wt",fit,list(wt=c(2.2,3.2,4.2))),multiline=TRUE) #ERROR
#------------------------------------------------------------------------------
# Regression diagnostics
#------------------------------------------------------------------------------
fit <- lm(Murder~ Population + Illiteracy + Income + Frost,data=states)
confint(fit)
#A typical approach
fit <- lm(weight~height,data=women)
par(mfrow=c(2,2))
plot(fit)
fit2<- lm(weight~height+ I(height^2),data=women)
par(mfrow=c(2,2))
plot(fit2)
fit3<-lm(weight~height+I(height^2),data=women[-c(13,15),])
par(mfrow=c(2,2))
plot(fit3)
#Apply to the states multiple regression problem
fit <-lm(Murder ~ Population + Illiteracy + Income + Frost,data=states)
par(mfrow=c(2,2))
plot(fit)
#Advanced approach
#Normality
library(car)
fit <- lm(Murder ~ Population + Illiteracy + Income + Frost,data=states)
qqPlot(fit,labels=row.names(states),id.method="identify",simulate=TRUE,main="Q-Q plot")
states["Nevada",]
fitted(fit)["Nevada"]
residuals(fit)["Nevada"]
rstudent(fit)["Nevada"]
#Function for plotting studentinized residuals
residplot <- function(fit,nbreaks=10)
{
  z <- rstudent(fit)
  hist(z,breaks= nbreaks,freq=FALSE,
       xlab="Studentized Residual",
      main= "Distribution of Errors")
rug(jitter(z),col="brown")
curve(dnorm(x,mean=mean(z),sd=sd(z)),add=TRUE,col="blue",lwd=2)
lines(density(z)$x,density(z)$y,col="red",lwd=2,lty=2)
legend("topright",legend=c("Normal Curve","Kernel Density Curve"),lty=1:2,col=c("blue","red"),cex=.7)
}
residplot(fit)
#Independence of errors
durbinWatsonTest(fit)
#linearity
#Component plus residual plots(partial residual plots)
#nonlinearity in any of these plots suggests that you may not have adequately
#modeled the functional form of that predictor in the regression
crPlots(fit)
#Homoscedasticity
#ncvTest() produce a score test of the hypothesis of constant error variance 
#against the alternative that the error variance changes with the level of fitted values
#A significant results suggests heteroscedasticity (non constant error variance)
ncvTest(fit)
spreadLevelPlot(fit) #scatter plot of abs standardized residuals vs fitted values
#-------------------------------------------------------------------------------
# Global validation of linear model assumption
#-------------------------------------------------------------------------------
#gvlma() performs a global validation of linear model assumptions
#as well as separate evaluations of skewness,kurtosis, heteroscedasticity
library(gvlma)
gvmodel<-gvlma(fit)
summary(gvmodel)
#Multicollinearity
#vif:variance inflation factor. As a general rule sqrt(vif)>2: multicollinearity problems
vif(fit)
sqrt(vif(fit))>2 #problems?
library(gvlma)
#Transforming variables
#car::powerTransform() generate a maximum-likelihood of the power lambda most likely to
#normalize the variable Xlambda
states <- as.data.frame(state.x77[,c("Murder","Population","Illiteracy","Income","Frost")])
library(car)
summary(powerTransform(states$Murder))
#car::boxTidwell() : estimates predictor powers than can improve linearity
boxTidwell(Murder~Population + Illiteracy,data=states)
#--------------------------------------------------------------------------------------
#anova(): compare the fit of nested model
fit1<- lm(Murder~Population + Illiteracy + Income+ Frost,data=states)
fit2<- lm(Murder~Population + Illiteracy,data=states)
anova(fit2,fit1)
#Akaike Information Criterion (AIC): smaller AIC values - indicating adequate fit with fewer params
#DOESNT require nested model
fit1<- lm(Murder~Population + Illiteracy + Income+ Frost,data=states)
fit2<- lm(Murder~Population + Illiteracy,data=states)
AIC(fit1,fit2)
#----------------------------------------------------------------------------
#Stepwise regression
#Cons: Not very possible model is evaluated
#----------------------------------------------------------------------------
library(MASS)
fit<- lm(Murder~Population + Illiteracy + Income+ Frost,data=states)
stepAIC(fit,direction="backward")
#----------------------------------------------------------------------------
# All subsets regression
# Good model is one in which Cp statistic is close to number of model parameters including intercept
library(leaps)
leaps<-regsubsets(Murder~Population + Illiteracy + Income + Frost,data=states,nbest=4)
plot(leaps,scale="adjr2")
#Better models will fall close to a line with intercept 1 and slope 1
library(car)
subsets(leaps,statistic="cp",main="Cp plot for All subsets regression") ##Error
abline(1,1,lty=2,col="red")
#-------------------------------------------------------------------------
# Function for k-fold cross-validated R-square
--------------------------------------------------------------------------
  shrinkage <- function(fit, k=10){
    require(bootstrap) 
    theta.fit <- function(x,y){lsfit(x,y)} 
    theta.predict <- function(fit,x){cbind(1,x)%*%fit$coef} 
    x <- fit$model[,2:ncol(fit$model)] 
    y <- fit$model[,1] 
    results <- crossval(x, y, theta.fit, theta.predict, ngroup=k) 
    r2 <- cor(y, fit$fitted.values)^2 
    r2cv <- cor(y, results$cv.fit)^2 
    cat("Original R-square =", r2, "\n")
    cat(k, "Fold Cross-Validated R-square =", r2cv, "\n")
    cat("Change =", r2-r2cv, "\n")
  }
library(bootstrap)
fit<- lm(Murder~Population + Illiteracy + Income+ Frost,data=states)
shrinkage(fit)
fit2<- lm(Murder~Population + Illiteracy,data=states)
shrinkage(fit2)
#Relative importance
zstates <- as.data.frame(scale(states))
zfit <- lm(Murder~Population + Income + Illiteracy + Frost, data=zstates)
coef(zfit)
#-----------------------------------------------------------------------
#relweights()function for calculating relative importance of predictors
#Dr.  Johnson.  See  Johnson  (2000, Multivariate  Behavioral  Research,  35,(1-19)
#-----------------------------------------------------------------------
relweights <- function(fit,...){ 
  R <- cor(fit$model) 
  nvar <- ncol(R) 
  rxx <- R[2:nvar, 2:nvar] 
  rxy <- R[2:nvar, 1] 
  svd <- eigen(rxx) 
  evec <- svd$vectors 
  ev <- svd$values 
  delta <- diag(sqrt(ev)) 
  lambda <- evec %*% delta %*% t(evec) 
  lambdasq <- lambda ^ 2 
  beta <- solve(lambda) %*% rxy 
  rsquare <- colSums(beta ^ 2) 
  rawwgt <- lambdasq %*% beta ^ 2 
  import <- (rawwgt / rsquare) * 100 
  lbls <- names(fit$model[2:nvar]) 
  rownames(import) <- lbls
  colnames(import) <- "Weights"
  barplot(t(import),names.arg=lbls,
          ylab="% of R-Square",
          xlab="Predictor Variables",
          main="Relative Importance of Predictor Variables", 
          sub=paste("R-Square=", round(rsquare, digits=3)),
          ...) 
  return(import)
}
fit <- lm(Murder ~ Population + Illiteracy + Income + Frost, data=states)
relweights(fit, col="lightgrey")