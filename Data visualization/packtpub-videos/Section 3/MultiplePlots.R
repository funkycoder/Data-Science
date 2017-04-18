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



#Multiple plots in one graph
ggplot(data=Data, aes(SO2)) + 
  geom_histogram(binwidth = 0.1) + 
  facet_wrap(~State)



#Different plots side-by-side
#install.packages("gridExtra")
library(gridExtra)


#Vertical Stacking
plot1 <- ggplot(data=California, aes(NO2)) + 
  geom_histogram(binwidth = 0.5) +
  ylab("") +
  scale_y_continuous(breaks=NULL)

plot2 <- ggplot(data=California, aes(x=1, y=NO2)) +
  geom_boxplot(width=0.5) +
  coord_flip() +
  xlab("") +
  scale_x_continuous(breaks=NULL)

grid.arrange(plot1, plot2, nrow=2)




#Horizontal Stacking
plot1 <- ggplot(data=California, aes(NO2)) + 
  geom_histogram(binwidth = 0.5)

plot2 <- ggplot(data=California, aes(x=Date, y=NO2)) +
  geom_line()

grid.arrange(plot1, plot2, ncol=2)


