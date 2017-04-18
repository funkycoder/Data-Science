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


str(Data)

#Create simple Box-Plots
ggplot(data=Data, aes(x=State, y=NO2)) + 
  geom_boxplot()

#Ordered
ordered <- c("Maine","Iowa","Texas","California","Ohio","New York")

ggplot(data=Data, aes(x=State, y=NO2)) + 
  geom_boxplot() +
  scale_x_discrete(limits=ordered)

#Create another box-plots
ggplot(data=Data, aes(x=State, y=SO2)) +
  geom_boxplot()
ordered<- c("Texas","California","Iowa","Maine","New York","Ohio")
ggplot(data=Data, aes(x=State,y=SO2)) +
  geom_boxplot() +
  scale_x_discrete(limits=ordered)

