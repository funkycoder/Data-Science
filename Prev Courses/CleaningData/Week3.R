#The American Community Survey distributes downloadable data about United States
#communities. Download the 2006 microdata survey about housing for the state of
#Idaho using download.file() from here: 
#        https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv 
#and load the data into R. The code book, describing the variable names is here: 
#        https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FPUMSDataDict06.pdf 
# Create a logical vector that identifies the households on greater than 10 acres 
#who sold more than $10,000 worth of agriculture products. Assign that logical
#vector to the variable agricultureLogical. Apply the which() function like this
#to identify the rows of the data frame where the logical vector is TRUE. 
#which(agricultureLogical) What are the first 3 values that result?

data<- read.csv("getdata-data-ss06hid.csv",header=TRUE)
agricultureLogical<-data$ACR==2 & data$AGS==6
head(which(agricultureLogical))
head(data)

#Using the jpeg package read in the following picture of your instructor into R 
#https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg 
#Use the parameter native=TRUE. What are the 30th and 80th quantiles of 
#the resulting data? 
img<- readJPEG("getdata-jeff.jpg",native=TRUE)
quantile(img,c(0.3,0.8))

#Load the Gross Domestic Product data for the 190 ranked countries in this data set: 
#https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv 
#Load the educational data from this data set: 
#  https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv 
#Match the data based on the country shortcode. How many of the IDs match? 
#Sort the data frame in descending order by GDP rank (so United States is last).
#What is the 13th country in the resulting data frame? 
gdp <- read.csv("getdata-data-GDP.csv",skip=5,header=FALSE,blank.lines.skip = TRUE,nrows=190)
gdp<-gdp[,c(1,2,4,5)]
colnames(gdp)<-c('id','rank','country','value')
edu<- read.csv("getdata-data-EDSTATS_Country.csv")
data<-merge(gdp,edu,by.x='id',by.y='CountryCode',all.x=TRUE)
data<-data[order(-data$rank),]
rank<-data$rank[data$Income.Group=="High income: OECD"]
mean(rank,na.rm=TRUE)
rank2<-data$rank[data$Income.Group=="High income: nonOECD"]
mean(rank2,na.rm=TRUE)
