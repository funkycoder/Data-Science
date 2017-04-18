#Load rCharts
library(rCharts)

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



#First we need to collect bins and frequencies in a data.frame
histogram <- hist(California$NO2, breaks=seq(0,max(California$NO2)+1,1), plot=F)
dat <- data.frame(Frequency=histogram$counts, NO2=histogram$mids)


#Learning the syntax of rCharts
hist.int <- nPlot(x = "NO2", y = "Frequency", data = dat, type = "discreteBarChart")
hist.int



#Adding elements
hist.int <- nPlot(x = "NO2", y = "Frequency", data = dat, type = "discreteBarChart")
hist.int$chart(color=rep("blue",nrow(dat)))
hist.int$xAxis(axisLabel="NO2")
hist.int


#We can add some javascript code for more flexibility
hist.int <- nPlot(x = "NO2", y = "Frequency", data = dat, type = "discreteBarChart")
hist.int$chart(color=rep("blue",nrow(dat)))
hist.int$xAxis(axisLabel="NO2")
hist.int$chart(tooltipContent = "#! function(key, x, y){ 
  return 'Frequency: ' + y + '<br> NO2 (ppm): ' + x 
         } !#")
hist.int



histogram2<- hist(California$SO2, breaks = seq(0,max(California$SO2)+1,1), plot = F)
dat2 <- data.frame(Frequency = histogram2$counts, SO2 = histogram2$mids)
hist2.int <- nPlot(x= "SO2", y = "Frequency", data= dat2, type= "discreteBarChart")
hist2.int$chart(color=rep("blue",nrow(dat2)))
hist2.int$xAxis((axisLabel="SO2"))
hist2.int$chart(tooltipContent = "#! function(key, x, y){ 
  return 'Frequency: ' + y + '<br> NO2 (ppm): ' + x 
                } !#")
hist2.int
#Thanks to Ramnath Vaidyanathan (https://github.com/ramnathv) for some of the code presented here

