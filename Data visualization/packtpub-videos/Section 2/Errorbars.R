#Import ggplot2
library(ggplot2)

#Set the working directory where the dataset is stored
setwd("D:/OneDrive/Data Science/R/R Data Visualization")

#Load the dataset in R
Weekly_Values <- read.table("Weekly_Values.csv",sep=",",header=T)
ordered <- Weekly_Values[order(Weekly_Values$SO2.mean),]

str(Weekly_Values)
str(ordered)

#Plot Error Bars
ggplot(data=Weekly_Values, aes(x=State, y=SO2.mean)) +
  geom_bar(stat="identity", fill="grey") +
  scale_x_discrete(limits=ordered$State) +
  geom_errorbar(data=Weekly_Values, aes(ymin=SO2.mean-(2*SO2.se), 
                                        ymax=SO2.mean+(2*SO2.se)), width=0.1)


#Error bars in scatterplots
ggplot(data=Weekly_Values, aes(x=NO2.mean, y=SO2.mean)) +
  geom_point(stat="identity", fill="grey") +
  geom_errorbarh(data=Weekly_Values, aes(xmin=NO2.mean-NO2.se, 
                                         xmax=NO2.mean+NO2.se), height=0.01)+
  geom_errorbar(data=Weekly_Values, aes(ymin=SO2.mean-SO2.se, 
                                        ymax=SO2.mean+SO2.se), width=0.01)



