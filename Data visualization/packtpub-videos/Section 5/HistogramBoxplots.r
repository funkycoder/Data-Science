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
histogram <- hist(California$NO2, 
                  breaks=seq(0,max(California$NO2)+1,1), 
                  plot=F)

dat <- data.frame(Frequency=histogram$counts, 
                  NO2=histogram$mids)



#Learning the syntax of rCharts
hist.int <- nPlot(x = "NO2", y = "Frequency", data = dat, 
                  type = "discreteBarChart")
hist.int



#Adding elements
hist.int <- nPlot(x = "NO2", y = "Frequency", 
                  data = dat, type = "discreteBarChart")
hist.int$chart(color=rep("blue",nrow(dat)))
hist.int$xAxis(axisLabel="NO2")
hist.int


#We can add some javascript code for more flexibility
hist.int <- nPlot(x = "NO2", y = "Frequency", 
                  data = dat, type = "discreteBarChart")
hist.int$chart(color=rep("blue",nrow(dat)))
hist.int$xAxis(axisLabel="NO2")
hist.int$chart(tooltipContent = "#! function(key, x, y){ 
  return 'Frequency: ' + y + '<br> NO2 (ppm): ' + x 
         } !#")
hist.int


#Thanks to Ramnath Vaidyanathan (https://github.com/ramnathv) for some of the code presented here




#compute boxplot statistics and cast it as a dataframe with no headers
NO2.stats <- setNames(
  as.data.frame(boxplot(NO2 ~ State, data = Data, plot = F)$stats),
  nm = NULL
)



#Box-Plots
#Create a new Highchart
h1 <- Highcharts$new()
h1$set(series = list(list(name = "NO2 (ppm)", data = NO2.stats)))


#Add axis details
h1$xAxis(
  categories = levels(Data$State),
  title = list(text = 'State')
)

h1$yAxis(
  title = list(text = 'NO2 (ppm)')  
)

#Define the type of plot
h1$chart(type = 'boxplot')
h1



SO2.stats <- setNames(
  as.data.frame(boxplot(SO2 ~ State, data = Data, plot = F)$stats),
  nm = NULL
)

h1 <- Highcharts$new()
h1$set(series = list(list(name = c("NO2 (ppm)","SO2 (ppm)"), 
                          data = c(NO2.stats,SO2.stats))))


#Add axis details
h1$xAxis(
  categories = levels(Data$State),
  title = list(text = 'State')
)

h1$yAxis(
  title = list(text = c("NO2 (ppm)","SO2 (ppm)")) 
)


#Define the type of plot
h1$chart(type = 'boxplot')
h1


#SOURCE: https://github.com/ramnathv/rCharts/issues/294