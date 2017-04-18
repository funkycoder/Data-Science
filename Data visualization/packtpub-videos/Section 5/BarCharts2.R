#Load rCharts
library(rCharts)
library(xlsx)

#Set the working directory where the dataset is stored
setwd("D:/OneDrive/Data Science/R/R Data Visualization")

seasons <- read.xlsx("Season_Reading.xlsx",sheetName="Summary")

str(seasons)



#Basic Plot
bar.int <- nPlot(y="SO2.mean", x="Season", group="State", data = seasons, 
                 type = "multiBarChart")
bar.int


#Add Elements
bar.int <- nPlot(y="SO2.mean", x="Season", group="State", data = seasons, 
                 type = "multiBarChart")
bar.int$yAxis(axisLabel="SO2 (ppm)")
bar.int$xAxis(axisLabel="Seasons")
bar.int$chart(margin=list(left=100))
bar.int


#Include a Template
bar.int <- nPlot(y="SO2.mean", x="Season", group="State", data = seasons, 
                 type = "multiBarChart")
bar.int$templates$script <- "http://timelyportfolio.github.io/rCharts_nvd3_templates/chartWithTitle_styled.html"
bar.int$set(title = "Seasonality of SO2")
bar.int$yAxis(axisLabel="SO2 (ppm)")
bar.int$xAxis(axisLabel="Seasons")
bar.int$chart(margin=list(left=100))
bar.int

#SOURCE: https://github.com/timelyportfolio/rcharts_nvd3_templates