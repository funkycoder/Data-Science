#Author: Thia Kai Xin
#Feel free to share/use this code in any way, shape or form
#Drop me comments/thanks @ thiakx@gmail.com =)

#update your working directory path here
setwd("C:/Users/thiak/Documents/kx/coursera/data analysis course/assignment")
load("loansData.rda")

#lib
library(gbm)
library(stringr)
library(Hmisc)

#sumary stats
head(loansData)
summary(loansData)
sapply(loansData[1,],class)

#interest and debt ratio is loaded as factor, we want to convert it to numeric
loansData$Interest.Rate <- gsub("%", "", loansData$Interest.Rate)
loansData$Interest.Rate <- as.numeric(loansData$Interest.Rate)
loansData$Debt.To.Income.Ratio <- gsub("%", "", loansData$Debt.To.Income.Ratio)
loansData$Debt.To.Income.Ratio <- as.numeric(loansData$Debt.To.Income.Ratio)

#split fico.range and use the average for analysis instead
ficoRange<-data.frame(str_split_fixed(loansData$FICO.Range, "-", 2))
ficoRange$X1 <- as.numeric(as.character(ficoRange$X1))
ficoRange$X2 <- as.numeric(as.character(ficoRange$X2))
ficoMean<-rowMeans(ficoRange)
loansData<- data.frame(loansData,ficoMean)

#check for na, replace na with mean
sum(is.na(loansData))
loansData$Monthly.Income[is.na(loansData$Monthly.Income)] <- mean(loansData$Monthly.Income, na.rm = TRUE)
loansData$Open.CREDIT.Lines[is.na(loansData$Open.CREDIT.Lines)] <- mean(loansData$Open.CREDIT.Lines, na.rm = TRUE)
loansData$Revolving.CREDIT.Balance[is.na(loansData$Revolving.CREDIT.Balance)] <- mean(loansData$Revolving.CREDIT.Balance, na.rm = TRUE)
loansData$Inquiries.in.the.Last.6.Months[is.na(loansData$Inquiries.in.the.Last.6.Months)] <- mean(loansData$Inquiries.in.the.Last.6.Months, na.rm = TRUE)

#drop fico range and use ficoMean instead, amt Funded will not affect interest rate based on lending club website
loansData$FICO.Range <- NULL
loansData$Amount.Funded.By.Investors <- NULL

#create train and test data sets (use 3/4 of data for training)
positions <- sample(nrow(loansData),size=floor((nrow(loansData)/4)*3))
training<- loansData[positions,]
testing<- loansData[-positions,]

#remove Interest.Rate from test data
actualValues<-testing$Interest.Rate
testing$Interest.Rate<-NULL

#gbm model to get top variables
gbmMod<-gbm(Interest.Rate ~., training,n.trees=1000,
            shrinkage=0.01, distribution="gaussian",
            interaction.depth=7,         
            bag.fraction=0.9,
            cv.fold=5,
            n.minobsinnode = 50
)

#find the best iteration for gbmMod
best.iter <- gbm.perf(gbmMod,method="cv")

#table that lists the reduction of squared error attributable to each variable
iScore <- summary(gbmMod, best.iter)
names(iScore)[1] <- "attribute"
names(iScore)[2] <- "importance"

#calculate root mean square error % of gbMod
result<- predict(gbmMod,testing,best.iter, type="response")
rMSEpercent<- sqrt(mean((actualValues-result)^2))/mean(actualValues)*100

#final plot
png(filename="finalPlot.png", width=800,height=800)
par(mfrow=c(2,2), mar=c(3,3,2,1), oma=c(0,0,3,1))
interestCut = cut2(loansData$Interest.Rate, g = 7)
amtRequestCut = cut2(loansData$Amount.Requested, g = 5)

#plot 1: Color grouped by interest rate
plot(loansData$ficoMean,loansData$Interest.Rate,pch=19, col=interestCut,
     main = "FigA: Color grouped by interest rate",ylab="",xlab="")
     legend("topright", legend=levels(interestCut), text.col=seq_along(levels(interestCut)))
lmInterest <- lm(loansData$Interest.Rate ~ loansData$ficoMean)
abline(lmInterest,col="blue",lwd=3)
mtext("Interest Rate",side=2,las=1,line=-3,at=1.05*max(loansData$Interest.Rate))
mtext("Fico Mean",side=1,line=2,at=max(loansData$ficoMean))

#plot 2: Color grouped by interest loan length
plot(loansData$ficoMean,loansData$Interest.Rate,pch=19, col=loansData$Loan.Length,
     main = "FigB: Color grouped by interest loan length",ylab="",xlab="")
     legend("topright", legend=levels(loansData$Loan.Length), text.col=seq_along(levels(loansData$Loan.Length)))
lmLength <- lm(loansData$Interest.Rate ~ loansData$ficoMean*loansData$Loan.Length)
abline(c(lmLength$coeff[1]+lmLength$coeff[3],
         lmLength$coeff[2]+lmLength$coeff[4]),
       col="blue",lwd=3)
mtext("Interest Rate",side=2,las=1,line=-3,at=1.05*max(loansData$Interest.Rate))
mtext("Fico Mean",side=1,line=2,at=max(loansData$ficoMean))

#plot 3: Color grouped by amount requested
#no idea how to plot abline for this subplot yet
plot(loansData$ficoMean,loansData$Interest.Rate,pch=19, col=amtRequestCut,
     main = "FigC: Color grouped by amount requested",ylab="",xlab="")
     legend("topright", legend=levels(amtRequestCut), text.col=seq_along(levels(amtRequestCut)))
lmAmt <- lm(loansData$Interest.Rate ~ loansData$ficoMean*loansData$Loan.Length*loansData$Amount.Requested)
mtext("Interest Rate",side=2,las=1,line=-3,at=1.05*max(loansData$Interest.Rate))
mtext("Fico Mean",side=1,line=2,at=max(loansData$ficoMean))

#put the big title above the 3 small graphs
mtext("Interest Rate vs FICO Mean", side=3, line=1, outer=TRUE, cex=2, font=2)

dev.off()

summary(lmAmt)

#confounders
anova(lm(ficoMean~.,loansData))
anova(lm(Interest.Rate  ~.,loansData))
