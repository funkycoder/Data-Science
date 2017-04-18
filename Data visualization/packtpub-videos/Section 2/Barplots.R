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


#Let us create a subset of our data,
#including just observations on October the 1st
Pollution_1stOct2013 <- Data[Data$Date=="2013-10-01",]


#Explore the dataset to identify the variables
str(Pollution_1stOct2013)


#Plot a simple Bar-Chart
ggplot(data=Pollution_1stOct2013, aes(x=State, y=SO2)) +
  geom_bar(stat="identity") 


#Order the columns
ordered <- Pollution_1stOct2013[order(Pollution_1stOct2013$SO2),]

ggplot(data=Pollution_1stOct2013, aes(x=State, y=SO2)) +
  geom_bar(stat="identity") +
  scale_x_discrete(limits=ordered$State) 

#Another bar chart
ggplot(data=Pollution_1stOct2013, aes(x=State,y=NO2)) +
  geom_bar(stat="identity")
ordered <- Pollution_1stOct2013[order(Pollution_1stOct2013$NO2),]
ggplot(data=Pollution_1stOct2013, aes(x=State, y=NO2)) +
  geom_bar(stat="identity") +
  scale_x_discrete(limits=ordered$State) 