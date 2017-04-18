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


#Set the data in the correct format
dat <- transform(California, date = as.character(Date))


#Simple Time-Series Plot
ts.int <- mPlot(x = "date", y = "NO2", data = dat, 
                type = "Line")
ts.int


#Increasing its visual appeal
ts.int <- mPlot(x = "date", y = "NO2", data = dat, 
                type = "Line")
ts.int$set(pointSize = 0, lineWidth = 1, 
           xLabels="day",xLabelAngle=45)
ts.int



#Save the Plot in a HTML file

#First install package "base64enc"
install.packages("base64enc")
library(base64enc)


#Now we can save the plot
ts.int$save("Time_Series.html", standalone = TRUE)
