#Loading the required libraries
install.packages(c("xlsx","rJava"))

#Java needs to be installed on the system in rder to load the package xlsx
#http://java.com/en/download/manual.jsp
#Careful to install the correct version of Java for 64bit machines

library(xlsx)

#Set the working directory where the dataset is stored
setwd("E:/OneDrive/R Video Course - Packt/Data")

#Loading Excel Files
Data.xlsx <- read.xlsx(file="EPA_Data.xlsx",sheetName="Sheet1")

#Examining the data
str(Data.xlsx)


#Loading required libraries
install.packages(c("xlsx","rJava"))

#Java needs to be installed on the system in rder to load the package xlsx
#http://java.com/en/download/manual.jsp
#Careful to install the correct version of Java for 64bit machines
library(xlsx)

#Set working directory
setwd("D:/OneDrive/Data Science/R/R Data Visualization")

#Loading Excel file
data.xlsx <- read.xlsx(file="EPA_Data.xlsx", sheetnam= "Sheet1")

#Examining the data
str(data.xlsx)