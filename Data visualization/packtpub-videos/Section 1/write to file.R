#Loading the required libraries
library(xlsx)

#Set the working directory where the dataset is stored
setwd("E:/OneDrive/R Video Course - Packt/Data")

#Loading CSV Files
Data <- read.table(file="EPA_Data.csv", sep=",", header=TRUE, colClasses=c("Date","factor",rep("numeric",5)), na.string="NA")

#Subsetting the data
California <- Data[Data$State=="California",]

#For more information on subsetting data in R
#please refer to:
#http://www.ats.ucla.edu/stat/r/modules/subsetting.htm
#http://www.statmethods.net/management/subset.html

#Exporting the results
write.table(California, file="Data/California.csv", sep=",", row.names=F)
write.table(California, file="Data/California.txt", sep=" ", row.names=F)
write.xlsx(California, file="Data/California.xlsx", sheetName="Air Pollution Data", row.names=F)

#Separating EXCEL Sheets for each pollutant
write.xlsx(California[,c("Date","Temperature","CO")], file="California.xlsx", sheetName="CO", row.names=F)
write.xlsx(California[,c("Date","Temperature","NO2")], file="California.xlsx", sheetName="NO2", row.names=F, append=T)
write.xlsx(California[,c("Date","Temperature","SO2")], file="California.xlsx", sheetName="SO2", row.names=F, append=T)

