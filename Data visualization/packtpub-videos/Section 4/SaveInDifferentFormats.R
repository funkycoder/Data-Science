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


#Save as jpeg image
ggsave(filename="NamePlot.jpeg", plot=Image_Plot)



#Save as tiff image
ggsave(filename="NamePlot.tiff", plot=Image_Plot)



#Change the size
ggsave(filename="NamePlot.tiff", plot=Image_Plot, width=14, height=8, units="cm")



#Understand the differences between Bitmap and Vector images
#http://www.bbc.co.uk/schools/gcsebitesize/dida/graphics/bitmapvectorrev3.shtml
#http://etc.usf.edu/techease/win/images/what-is-the-difference-between-bitmap-and-vector-images/


#Save as PDF
ggsave(filename="NamePlot.pdf", plot=Image_Plot)


#Save as SVG
ggsave(filename="NamePlot.svg", plot=Image_Plot)
