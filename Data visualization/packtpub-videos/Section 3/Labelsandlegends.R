#Import ggplot2
library(ggplot2)

#Set the working directory where the dataset is stored
setwd("E:/OneDrive/R Video Course - Packt/Data")

#Load the dataset in R
Data <- read.table(file="EPA_Data.csv", 
                   sep=",", 
                   header=TRUE, 
                   colClasses=c("Date","factor",rep("numeric",5)), 
                   na.string="NA")


#Here we are goign to use the subsets we created before
California <- Data[Data$State=="California",]




#Add a Title
ggplot(data=California, aes(x=NO2, y=CO, color=Temperature)) +
  geom_point() +
  scale_color_gradientn(colours=c("blue","light blue","green","orange","red")) +
  theme_classic() +
  labs(title = "Scatterplot")



#Change the Title of the Legend
ggplot(data=California, aes(x=NO2, y=CO, color=Temperature)) +
  geom_point() +
  scale_color_gradientn(colours=c("blue","light blue","green","orange","red")) +
  theme_classic() +
  labs(title = "Scatterplot", 
       colour = "Temp. (°C)")



#Change the X axis label
ggplot(data=California, aes(x=NO2, y=CO, color=Temperature)) +
  geom_point() +
  scale_color_gradientn(colours=c("blue","light blue","green","orange","red")) +
  theme_classic() +
  labs(title = "Scatterplot", 
       colour = "Temp. (°C)") +
  xlab("Nitrogen Dioxide (ppm)") +
  ylab("Carbon Monoxide (ppm)")







