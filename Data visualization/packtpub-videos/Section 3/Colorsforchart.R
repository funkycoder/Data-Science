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



#Default: from dark blue to light blue
ggplot(data=California, aes(x=NO2, y=CO, color=Temperature)) +
  geom_point() 


#Change the two colors
ggplot(data=California, aes(x=NO2, y=CO, color=Temperature)) +
  geom_point() +
  scale_color_gradient(low="orange", high="red")


#More info about all the colors available in R here:
#http://research.stowers-institute.org/efg/R/Color/Chart/


#Change between n colors
ggplot(data=California, aes(x=NO2, y=CO, color=Temperature)) +
  geom_point() +
  scale_color_gradientn(colours=c("blue","light blue","green","orange","red"))