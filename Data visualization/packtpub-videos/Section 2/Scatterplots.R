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

#Extract only the measurements in California
California <- Data[Data$State=="California",]


#Create a simple scatterplot
ggplot(data=California, aes(x=NO2, y=CO)) +
  geom_point() 


#Increase the sizes of the dots
ggplot(data=California, aes(x=NO2, y=CO)) +
  geom_point(size=3) 


#Increase the sizes of the dots
ggplot(data=California, aes(x=NO2, y=SO2, color=Temperature, size=CO)) +
  geom_point()


#Extract the measurements in Maine
Maine <- Data[Data$State=="Maine",]
ggplot(data=Maine, aes(x=NO2, y=CO)) +
  geom_point(size=3)

ggplot(data=Maine, aes(x=NO2, y=SO2, color=Temperature, size=CO)) +
  geom_point()