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


#Create a plot as an object
Image_Plot <- ggplot(data=California, aes(x=NO2, y=SO2, color=Temperature)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  theme_classic() +
  labs(title = "Scatterplot", 
       colour = "Temp. (°C)") +
  xlab("Nitrogen Dioxide (ppm)") +
  ylab("Carbon Monoxide (ppm)")


#Check the plot
print(Image_Plot)



#Save in A4 - Portrait
ggsave(filename="NamePlot.pdf", plot=Image_Plot, paper="a4")



#Save in A4 - Landscape
ggsave(filename="NamePlot.pdf", plot=Image_Plot, paper="a4r")



#Other possible formats:
#"letter"
#"legal"
#"executive"


#Customize the page size
#B5
ggsave(filename="NamePlot.pdf", plot=Image_Plot, width=250, height=176, units="mm")

#Page sizes can be found here:
#http://www.papersizes.org/a-paper-sizes.htm
#http://www.papersizes.org/b-paper-sizes.htm


#Change Resolution
#ggplot2 save at 300dpi by default
ggsave(filename="NamePlot.tiff", plot=Image_Plot)



#Change the resolution
ggsave(filename="NamePlot.tiff", plot=Image_Plot, dpi=600)

