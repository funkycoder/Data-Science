#-------------------------------------------------------------------------------
# WEEK 1 QUIZZES
#-------------------------------------------------------------------------------
# The American Community Survey distributes downloadable data about United States
#communities. Download the 2006 microdata survey about housing for the state of 
#Idaho using download.file() from here: 
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
#remove method="curl" on windows machine!
download.file (fileUrl,destfile="housingIdaho.csv")
date.downloaded <- date()
#is the file created?
list.files()
#read the data
hid <- read.csv("housingIdaho.csv")
head(hid)
str(hid)
summary(hid)
#The code book is at : https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FPUMSDataDict06.pdf 
#How many housing units in this survey were worth more than $1,000,000?
#VAL = 24
#Calculate the number of housing worth more than 1M. Remove all NAs!
sum(as.numeric(hid$VAL==24),na.rm=TRUE)
#Consider the variable FES in the code book. Which of the "tidy data" principles
#does this variable violate?
#FES : Family type and employment status

#Excel spreadsheet on Natural Gas Aquisition Program 
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx"
#remove method="curl" on windows machine!
download.file (fileUrl,destfile="naturalGasProgram.xlsx")
date.downloaded <- date()
#is the file created?
list.files()
#Read rows 18-23 and columns 7-15 into R and assign the result to a variable called dat
library(xlsx)
colIndex <- 7:15
rowIndex <- 18:23
dat <- read.xlsx("getdata-data-DATA.gov_NGAP.xlsx",sheetIndex=1,colIndex=colIndex,rowIndex=rowIndex)
sum(dat$Zip*dat$Ext,na.rm=T) 
#Read the XML data on Baltimore restaurants
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml"
library(XML)
doc <- xmlTreeParse("getdata-data-restaurants.xml",useInternal=TRUE)
rootNode <- xmlRoot(doc)
rootNode[[1]][[1]]
zipCode<- xpathSApply(doc,"//zipcode",xmlValue)
#count number of restaurant have zipcode as 21231
sum(as.numeric(zipCode=="21231"),na.rm=TRUE)
#The American Community Survey distributes downloadable data about United States communities.
#using the fread() command load the data into an R object
library(data.table)
DT <- fread("getdata-data-ss06pid.csv")
# Which of the following is the fastest way to calculate the average value of the variable
# pwgtp15 broken down by sex using the data.table package?

