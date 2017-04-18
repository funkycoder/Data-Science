#Import ggplot2
library(ggplot2)

#Set the working directory where the dataset is stored
setwd("D:/OneDrive/Data Science/R/R Data Visualization")

#Load the dataset in R
Data <- read.table(file="EPA_Data.csv", 
                   sep=",", 
                   header=TRUE, 
                   colClasses=c("Date","factor",rep("numeric",5)), 
                   na.string="NA")


#Here we are goign to use the subsets we created before
California <- Data[Data$State=="California",]

Pollution_1stOct2013 <- Data[Data$Date=="2013-10-01",]


#Add Fixed Text Label
ggplot(data=California, aes(NO2)) + 
  geom_histogram() +
  geom_vline(xintercept = mean(California$NO2),
             color="red",
             linetype = "longdash") +
  geom_text(aes(label="Mean", x=11, y=25, angle=90), color="red")
  


#Add Dynamic Text Label
ordered <- Pollution_1stOct2013[order(Pollution_1stOct2013$SO2),]

ggplot(data=Pollution_1stOct2013, aes(x=State, y=SO2)) +
  geom_bar(stat="identity", fill="light gray") +
  scale_x_discrete(limits=ordered$State) +
  theme_classic() +
  geom_text(aes(label=paste(Pollution_1stOct2013$SO2), 
                x=State, 
                y=SO2 +0.05, 
                angle=0), color="red")


#Change the Axis Labels
ggplot(data=Pollution_1stOct2013, aes(x=State, y=SO2)) +
  geom_bar(stat="identity") +
  scale_x_discrete(limits=ordered$State, "State", 
                   labels = c("ME","TX","CA","IA","NY","OH"))



#Change the Axis Labels
ggplot(data=California, aes(x=Temperature, y=Pressure)) +
  geom_point() +
  xlab("") +
  scale_x_continuous(breaks=seq(0,30,5), 
                     label=paste(seq(0,30,5),"oC")) +
  ylab("") +
  scale_y_continuous(breaks=seq(950,1000,10), 
                     label=paste(seq(950,1000,10),"hPA"))



