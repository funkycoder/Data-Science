library(randomForest)
library(sampling)
library(caret)

# #Actual code, keep these coded commented if you running on single core machines
# #parallel processing library. Note that I ran this code on a quad core machine.
# #in general, you try to use no.of cores -1 as number of workers
# library(foreach)
# library(doSNOW)
# library(parallel)
# coreNumber<-max(detectCores()-1,1)
# cluster <-makeCluster(coreNumber, type = "SOCK",outfile="")
# registerDoSNOW(cluster)

#colorPalette to plot colorblind friendly graphs
#source: http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442",
                "#0072B2", "#D55E00", "#CC79A7","#999999")

setwd("C:/Users/thiak/Documents/kx/coursera/data analysis course/assignment2")
load("samsungData.rda")

# initial data exploration
# dataHead<-head(samsungData)
# dataClass<-sapply(samsungData[1,],class)
# dataSummary<-summary(samsungData)
# sum(is.na(samsungData))

#clean up the col names to remove brackets and commas etc that may cause error in modelling
names(samsungData)<-(gsub("[[:punct:]]","",names(samsungData)))

#data.frame defaults check.names = TRUE, auto renaming columns with same name (e.g. bandsEnegy)
samsungData<-data.frame(samsungData)

#convert activity and subject to factor
samsungData$activity<-as.factor(samsungData$activity)
activitySummary<-summary(samsungData$activity)
samsungData$subject<-as.factor(samsungData$subject)
subjectSummary<-summary(samsungData$subject)

#create training set: we try to have as much training data as possible by including more subjects
#we have to use as.numeric(as.character()) here 
training<-samsungData[as.numeric(as.character(samsungData$subject))<27,]

#relevel subject, since we now only have subject<23
training$subject<-factor(training$subject)

#make sure every training subject have equal number of samples to prevent skew
#17 training subjects
minSize<-min(summary(training$subject))
training=getdata(training, strata(training,"subject",
                                  size=c(minSize,minSize,minSize,minSize,minSize,
                                         minSize,minSize,minSize,minSize,minSize,
                                         minSize,minSize,minSize,minSize,minSize,
                                         minSize,minSize),method="srswor"))

# remove extra variables introduced by strata 
training[,(length(training)-2):length(training)]<-as.list(NULL)

#create test set: must include subjects 27, 28, 29, and 30
test<-samsungData[as.numeric(as.character(samsungData$subject))>26,]
actualValues<-test$activity
test$activity<-NULL

#remove subject
training$subject<-NULL
test$subject<-NULL

#split off validation data from training data
positions <- sample(nrow(training),size=floor(nrow(training)*3/4))
trainingData<-training
training<- trainingData[positions,]
validation<- trainingData[-positions,]
validationValues<-validation$activity
validation$activity<-NULL

#test if training data and test data have similar distribution using PCA to compress data and plot
pcaTrainData<-training
pcaTestData<-test
pcaTrainData$activity<-NULL
pcaTestData$activity<-NULL

pcaTrain<-prcomp(pcaTrainData)
pcaTest<-prcomp(pcaTestData)

plot(pcaTrain$x, col=cbbPalette[1],main = "Training data (Black) against Test data(Yellow)" )
par(new=TRUE)
plot(pcaTest$x, col=cbbPalette[2])

##validation step
##testing various values of mtry, seems like mtry 1/3 with larger ntree works best
##calling the columns directly is faster than using the equation format: activity~.

#  rf1  <-randomForest(training[,1:length(training)-1], training$activity,
#                      xtest=validation,ytest=validationValues,keep.forest=TRUE,
#                      ntree=200, do.trace=40,mtry=round(length(training)^(1/2)))
# rf2  <-randomForest(training[,1:length(training)-1], training$activity,
#                     xtest=validation,ytest=validationValues,keep.forest=TRUE,
#                     ntree=200, do.trace=40,mtry=round(length(training)^(1/3)))
# rf3  <-randomForest(training[,1:length(training)-1], training$activity,
#                     xtest=validation,ytest=validationValues,keep.forest=TRUE,
#                     ntree=200, do.trace=40,mtry=round(length(training)/2))
# rf4  <-randomForest(training[,1:length(training)-1], training$activity,
#                     xtest=validation,ytest=validationValues,keep.forest=TRUE,
#                     ntree=200, do.trace=40,mtry=round(length(training)^(1/4)))
# rf5  <-randomForest(training[,1:length(training)-1], training$activity,
#                     xtest=validation,ytest=validationValues,keep.forest=TRUE,
#                     ntree=1000, do.trace=200,mtry=round(length(training)^(1/4)))
# rf6  <-randomForest(training[,1:length(training)-1], training$activity,
#                     xtest=validation,ytest=validationValues,keep.forest=TRUE,
#                     ntree=1000, do.trace=200,mtry=round(length(training)^(1/3)))
# rf7  <-randomForest(training[,1:length(training)-1], training$activity,
#                     xtest=validation,ytest=validationValues,keep.forest=TRUE,
#                     ntree=1000, do.trace=200,mtry=round(length(training)^(1/2)))


##non-parallel code, not my actual submission but should work better on most computers without multicore
rf  <-randomForest(training[,1:length(training)-1], training$activity,
                    xtest=validation,ytest=validationValues,keep.forest=TRUE,
                    ntree=1001, do.trace=200,mtry=round(length(training)^(1/3)))

# # my actual submission, parallel processing to speed up randomforest
# # uncomment this section to run randomforest in parallel
# # we try to have odd number of tree counts (667*3) to break draw votes
# #.inorder = FALSE further increase the speed
# #takes about 3min to run on my machine
# rf  <-foreach(ntree=rep(round(2001/coreNumber),coreNumber), .combine=combine, .packages='randomForest',.inorder=FALSE) %dopar%{
#                randomForest(training[,1:length(training)-1], training$activity, do.trace=100,
#                xtest=validation,ytest=validationValues,keep.forest=TRUE,
#                ntree=ntree, mtry=round(length(training)^(1/3)))
# }
# #stop parallel processing
# stopCluster(cluster)

#benchmark result from just randomly guessing each activities
predictedRandom <- sample(as.factor(c("laying","sitting","standing","walk",
                                            "walkdown","walkup")), nrow(test), replace=T)
conTableR<-confusionMatrix(table(predictedRandom,actualValues))
conTableR

predictedValues <- predict(rf, test, type="response")
conTable<-confusionMatrix(table(predictedValues,actualValues))
conTable

heatmap(table(predictedValues,actualValues),col=(cbbPalette))
legend(x=-14,y=8, fill = c(cbbPalette[1:2],cbbPalette[9]),
       legend = c("<10% error", ">10% error", "Correct Prediction"))
mtext("Predicted Activity",side=4,las=1,line=-16,col=cbbPalette[2])
mtext("Actual Activity",side=1,line=8,col=cbbPalette[2])

#calculate relative importance of each variable
iScore <- importance(rf)
iScore <-data.frame(rownames(iScore),iScore)
names(iScore)<-c("attribute","importance")
iScore<-iScore[order(iScore$importance,decreasing=TRUE),]

