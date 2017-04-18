#Install ggplot2
install.packages("ggplot2")

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


#Explore the dataset to identify the variables
str(Data)


#Plot a simple Histogram of NO2
ggplot(data=Data, aes(x=NO2)) + 
  geom_histogram() 

ggplot(data=Data,aes(SO2)) + 
  geom_histogram()


#Use Faceting to plot multiple histograms
ggplot(data=Data, aes(x=NO2)) + 
  geom_histogram(binwidth = 1) +
  facet_wrap(~State)

ggplot(data=Data, aes(x=SO2)) +
  geom_histogram(binwidth = 1) +
  facet_wrap(~State)

