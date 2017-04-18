#Load rCharts
library(rCharts)

#Set the working directory where the dataset is stored
setwd("E:/OneDrive/R Video Course - Packt/Data")

#Load the dataset in R
Data <- read.table(file="EPA_Data.csv", 
                   sep=",", 
                   header=TRUE, 
                   colClasses=c("Date","factor",rep("numeric",5)), 
                   na.string="NA")

#Extract only the measurements in California
California <- Data[Data$State=="California",]


#Let's start with a basic multiple scatterplot
scat.int <- nPlot(x="NO2", y="SO2", group="State", 
                  data=Data, type="scatterChart") 
scat.int


#We can change the size of the points 
#by adding another simple javascript function
scat.int <- nPlot(x="NO2", y="SO2", group="State", 
                  data=Data, type="scatterChart") 
scat.int$xAxis(axisLabel="NO2 (ppm)")
scat.int$yAxis(axisLabel="SO2 (ppm)")
scat.int$chart(size = '#! function(d){return d.CO} !#')
scat.int$chart(tooltipContent = "#! function(key, x, y, e){ 
  return 'NO2 (ppm): ' + x + 
  '<br> SO2 (ppm): ' + y +
  '<br> CO (ppm): ' + e.point.CO
               } !#")
scat.int


#We can even add controls to choose different variables
scat.int <- nPlot(x="NO2", y="SO2", group="State", 
                  data=Data, type="scatterChart") 
scat.int$addControls("x", value = "NO2", values = names(Data)[3:7])
scat.int$addControls("y", value = "SO2", values = names(Data)[3:7])
scat.int

