#set the R working directory
setwd("D:/Google Drive/Public Health/R/Statistical Analysis with R")

load("rBeginnersGuide_Ch_05_ReadersCopy.RData")
ls()
meanSoldiersShu
meanSoldiersWei
#ratio of mean Wei soldiers to Shu soldiers
meanSoldierRatioWeiShu <- meanSoldiersWei / meanSoldiersShu
meanSoldierRatioWeiShu
#how many Wei soldiers would we expect to engage in battle if our Shu forces numbered 100,000?
100000 * meanSoldierRatioWeiShu
battleHistory
#create a subset that isolates our head to head combat data
subsetHeadToHead <- subset(battleHistory,battleHistory$Method=="headToHead")
subsetHeadToHead
#what was the mean number of Shu soldiers engaged in past head to head conflicts?
meanShuSoldiersHeadToHead <-  mean(subsetHeadToHead$ShuSoldiersEngaged)
#what was the mean number of Wei soldiers engaged in past head to head conflicts?
meanWeiSoldiersHeadToHead <- mean(subsetHeadToHead$WeiSoldiersEngaged)
#what was the mean duration (in days) of past head to head conflicts?
meanDurationHeadToHead <- mean(subsetHeadToHead$DurationInDays)
#display the calculated means
meanShuSoldiersHeadToHead
meanWeiSoldiersHeadToHead
meanDurationHeadToHead

#what was the standard deviation of Shu soldiers engaged in past head to head conflicts?
sdShuSoldiersHeadToHead <-  sd(subsetHeadToHead$ShuSoldiersEngaged)
#what was the standard deviation of Wei soldiers engaged in past head to head conflicts?
 sdWeiSoldiersHeadToHead <-  sd(subsetHeadToHead$WeiSoldiersEngaged)
#what was the standard deviation of duration (in days)  in past head to head conflicts?
sdDurationHeadToHead <- mean(subsetHeadToHead$DurationInDays)
sdShuSoldiersHeadToHead
sdWeiSoldiersHeadToHead
sdDurationHeadToHead
#what was the range of Shu soldiers engaged in past head to head conflicts?
rangeShuSoldiersHeadToHead <-  range(subsetHeadToHead$ShuSoldiersEngaged)
#what was the range of Wei soldiers engaged in past head to head conflicts?
rangeWeiSoldiersHeadToHead <-  range(subsetHeadToHead$WeiSoldiersEngaged)
#what was the range of duration (in days) of past head to head conflicts?
rangeDurationHeadToHead <-  range(subsetHeadToHead$DurationInDays)
rangeShuSoldiersHeadToHead
rangeWeiSoldiersHeadToHead
rangeDurationHeadToHead
#represent categorical data numerically using as.numeric(data)
#recode the SuccessfullyExecuted column into N = 1 and Y = 2
numericExecutionHeadToHead <- as.numeric(subsetHeadToHead$SuccessfullyExecuted)
#recode the SuccessfullyExecuted column into N = 0 and Y = 1
#by default, R recodes variables alphabetically from 1 to n,  so subtract one to offset the coding from 0 to n
numericExecutionHeadToHead <-  as.numeric(subsetHeadToHead$SuccessfullyExecuted) - 1
#recode the Result column into Defeat = 0 and Victory = 1
numericResultHeadToHead <- as.numeric(subsetHeadToHead$Result)- 1
#use cor(x,y) to calculate the correlation between two variables
#remember only to use numeric values when calculating correlations
#How is the performance rating of the Shu army related to the outcome of a head to head battle?
corRatingResultHeadToHead <- cor(subsetHeadToHead$Rating,numericResultHeadToHead)
#display the value of the correlation
corRatingResultHeadToHead
#How is the number of Shu soldiers engaged in a head to headbattle correlated with the number of Wei soldiers engaged?
corShuWeiSoldiersHeadToHead <-  cor(subsetHeadToHead$ShuSoldiersEngaged,subsetHeadToHead$WeiSoldiersEngaged)
#display the value of the correlation
corShuWeiSoldiersHeadToHead
#use cor(data) to calculate the correlation between all numeric variables in a dataset
#How are all of our numeric battle data correlated with one another?
corHeadToHead <- cor(subsetHeadToHead)
#display the correlations
corHeadToHead