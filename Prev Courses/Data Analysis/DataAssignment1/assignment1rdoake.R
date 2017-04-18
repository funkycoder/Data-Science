## Cleaned up version of Data Analysis Assignment 1 code. 
## Untested: use at your own risk. 
## 2/25/2013 Raja Doake

## Package requirements:
require(ggplot2)
require(gridExtra)
require(R.utils)

############################################################
## Read the data
############################################################

## Operating System Check
## Read loansClean.csv and ensure that NAs are inserted in Employment.Length in place of "n/a"
## Read it 'as is' to obtain numerics and characters rather than factors
## Read bigLoansData, the full Lending Club loan dataset, in the same way
if( Sys.info()['sysname']  == "Windows") {
  ## Windows
  setwd("C:\\Users\\Raja\\My Documents\\My Dropbox\\Personal\\Data Analysis\\WorkDir\\da1\\code")
  loansClean <- read.csv ( "C:\\Users\\Raja\\My Documents\\My Dropbox\\Personal\\Data Analysis\\WorkDir\\da1\\loansData.csv", 
                           as.is=TRUE, 
                           na.strings = c("NA","n/a") )
  
  bigLoans <- read.csv ( "C:\\Users\\Raja\\My Documents\\My Dropbox\\Personal\\Data Analysis\\WorkDir\\da1\\bigLoansData.csv", 
                         as.is=TRUE, 
                         na.strings = c("NA","n/a") )
  
} else {
  ## Mac OS X
  setwd("~/Dropbox/Personal/Data Analysis/WorkDir/da1/code")
  loansClean <- read.csv ( "~/Dropbox/Personal/Data Analysis/WorkDir/da1/loansData.csv", 
                           as.is=TRUE, 
                           na.strings = c("NA","n/a") )
  
  bigLoans <- read.csv ( "~/Dropbox/Personal/Data Analysis/WorkDir/da1/bigLoansData.csv", 
                         as.is=TRUE, 
                         na.strings = c("NA","n/a") )
}

############################################################
## Clean up loansData
############################################################

## Subset the data with complete cases. 
loansTest <- complete.cases ( loansClean )
loansClean <- loansClean[loansTest,]

## Take the log of monthly income to remove the right skew
loansClean$Monthly.Income <- log(loansClean$Monthly.Income + 1)

## Convert the Interest.Rate column from character to numeric
loansClean$Interest.Rate <- as.numeric ( gsub ( "%$","",loansClean$Interest.Rate ) ) 

## Convert the Loan.Length column from character to factor after removing the number of months
loansClean$Loan.Length <- as.factor( as.numeric ( gsub ( " months$","",loansClean$Loan.Length ) ) )

## Debt.To.Income.Ratio should also be numeric
## Some values in the character vector are NA, so we suppress warnings when coercing to numeric.
loansClean$Debt.To.Income.Ratio <- as.numeric ( gsub ( "%$","",loansClean$Debt.To.Income.Ratio ) )

## FICO scores are stored as a factor binned in increments of 5. Convert them to the value of the first integer in the bin.
FICO <- regexpr( "[0-9][0-9][0-9]", loansClean$FICO.Range )
FICO <- as.integer ( regmatches ( loansClean$FICO.Range, FICO ) )
loansClean$FICO.Range <- FICO

############################################################
## Clean up bigLoans
############################################################

## Convert the Interest.Rate column from character to numeric
bigLoans$Interest.Rate <- as.numeric ( gsub ( "%$","",bigLoans$Interest.Rate ) ) 

## Parse the dates
bigLoans$Issued.Date <- as.Date(bigLoans$Issued.Date, "%m/%d/%Y")

## Parse the loan lengths
bigLoans$Loan.Length <- as.factor( as.numeric ( gsub ( " months$","",bigLoans$Loan.Length ) ) )

## Convert loan ID to numeric. 
bigLoans$Loan.ID <- as.numeric(bigLoans$Loan.ID)

############################################################
## Do the iterative linear regression
############################################################

## Create a matrix to receive our R-squared values with the correct column names. 
lmXrAll <- matrix(nrow=2, ncol=14)
lmXrAll[1,] <- names(loansClean)

## Do the iterative regression with FICO and each other variable.
for (i in 1:length(names(loansClean))) {
  lmX <- lm(loansClean$Interest.Rate ~ loansClean$FICO.Range + loansClean[,i])
  lmXrAll[2,i] <- summary(lmX)$r.squared
}

lmXrAll <- lmXrAll[,-c(3,10)] ## remove FICO range from the residuals vector
which.max(lmXrAll[2,])  ## returns 3, which corresponds to loan length

## Check the model parameters. 
summary(lmXrAll)
confint(lmXrAll)

## Repeat the exercise with a FICO, loan length, and a third variable. 
for (i in 1:length(names(loansClean))) {
  lmX <- lm(loansClean$Interest.Rate ~ loansClean$FICO.Range + loansClean$Loan.Length + loansClean[,i])
  lmXrAll[2,i] <- summary(lmX)$r.squared
}

lmXrAll <- lmXrAll[,-c(3,4,10)] ## remove FICO range and loan length from the residuals vector
which.max(lmXrAll[2,]) ## returns 2, the column corresponding to amount funded.

## Since amount funded is an output, and it barely differs from amount requested, the final model will use amount requested. 

## Check the model parameters. 
summary(lmXrAll)
confint(lmXrAll)

############################################################
## Generate the final model
############################################################

lmX <- lm(loansClean$Interest.Rate ~ loansClean$FICO.Range + as.factor(loansClean$Loan.Length) + loansClean$Amount.Requested)
summary(lmX)
confint(lmX)

############################################################
## Consolidate data for plotting
############################################################

## Store model residuals in a data frame with the variables
lmResid <- data.frame(Rate = loansClean$Interest.Rate, 
                      FICO = loansClean$FICO.Range,
                      LoanLength = factor(loansClean$Loan.Length, labels=c("36 months", "60 months")),
                      Requested = loansClean$Amount.Requested,
                      Residuals = lmX$residuals)


## The full Lending Club dataset
bigRate <- data.frame(Rate = bigLoans$Interest.Rate,
                      Date = bigLoans$Issued.Date,
                      LoanLength = factor(bigLoans$Loan.Length, labels=c("36 months", "60 months")))

############################################################
## Do our plotting with ggplot2 and gridExtras
############################################################

## Hybrid violin/boxplot showing the gap between interest rates by loan length
lengthBox <- ggplot(lmResid, aes(x=LoanLength, y=Rate)) + 
  ggtitle("Figure 1-1: Box/Violin Plot:\nInterest Rate vs Loan Length") +
  geom_violin() +
  geom_boxplot(width=0.3, fill="grey90", outlier.colour=NA) + 
  xlab("Loan Length (months)") + 
  ylab("Interest Rate (%)") +
  stat_summary(fun.y=mean, geom="point", fill="white", shape=21, size=5)

## Scatterplot of loan rates over time, colour coded by Loan Length
fullScatter <- ggplot(bigRate, aes(x=Date, y=Rate, colour= LoanLength)) + 
  geom_point(alpha=0.25) +
  ggtitle("Figure 1-3: Lending Club Interest Rates 2007-2013") +
  xlab(NULL) +
  ylab("Interest Rate (%)") + 
  theme(legend.position=c(0.22,0.79), legend.justification=c(1,0)) + 
  scale_colour_brewer(palette="Set1") +
  guides(colour = guide_legend(override.aes = list(alpha=1)))

## Residual plot with no colour coding
blankResid <- ggplot(lmResid, aes(x=Rate, y=Residuals)) + 
  geom_point() + 
  stat_function(fun=function(x) x=0, geom="line", lwd=1.5, col="blue", alpha=0.66) +
  xlab("Interest Rate (%)") +
  ylab("Model Residuals") + 
  ggtitle("Figure 1-2: Model Residuals vs Interest Rate\nModel: FICO, Loan Length, Amount Requested")

## Produce and pane final plots
plotsFinal <- grid.arrange(lengthBox, blankResid, fullScatter, nrow=2, ncol=2)
