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


#Create a simple Time-Series plot
ggplot(data=California, aes(x=Date, y=NO2)) +
  geom_line() 


#Add data as colors and change the line size
ggplot(data=California, aes(x=Date, y=NO2, color=CO)) +
  geom_line(size=1) 




