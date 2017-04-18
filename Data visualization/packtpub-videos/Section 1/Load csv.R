#Set the working directory where the data is stored
setwd("D:/OneDrive/Data Science/R/R Data Visualization")

#Loading the CSV file
data <- read.table("EPA_Data.csv",
                   header = TRUE,
                   sep= ",",
                   colClasses = c("Date","factor", rep("numeric",5)),
                   na.strings = "NA")
#See help function?
help("read.table")

#Examining the data
str(data)
