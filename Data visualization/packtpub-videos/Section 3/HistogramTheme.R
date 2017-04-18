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


#Default Theme
ggplot(data=Data, aes(NO2)) + 
  geom_histogram()


#Theme Minimal
#No Background, no Axes lines, light gridilines
ggplot(data=Data, aes(NO2)) + 
  geom_histogram() +
  theme_minimal()


#Theme Light
#No Background, light box around the plot, light gridilines
ggplot(data=Data, aes(NO2)) + 
  geom_histogram() +
  theme_light()


#Theme Classic
#No Background, tick axes lines, no gridilines
ggplot(data=Data, aes(NO2)) + 
  geom_histogram() +
  theme_classic()




