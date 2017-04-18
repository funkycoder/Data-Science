data<- read.csv("getdata-data-ss06hid.csv");
splitNames= strsplit(names(data),"wgtp");
splitNames[[123]]


data<- read.csv("getdata-data-GDP.csv",nrows=190,header=FALSE,skip=5)
data<-data[,c(1,2,4,5)]
average<- as.numeric(gsub(",","",data$V5))
mean(average,na.rm=TRUE)
grep("^United",data$V4)


gdp<- read.csv("getdata-data-GDP.csv",nrows=190,header=FALSE,skip=5)
edu<- read.csv("getdata-data-EDSTATS_Country.csv")
colnames(gdp)[1]<- 'id'
data<-merge(gdp,edu,by.x="id",by.y="CountryCode",all.x=TRUE)
notes<- data$Special.Notes
sum(grepl("Fiscal year end: June",notes,fixed=TRUE))


library(quantmod)
amzn = getSymbols("AMZN",auto.assign=FALSE)
sampleTimes = index(amzn)
data<- as.data.frame(amzn)
#sum(grepl("2012",rownames(data),fixed=TRUE))
dates<- as.Date(sampleTimes,"%Y-%m-%d")
dates<-dates[grepl("2012",dates,fixed=TRUE)]
wkdays<- weekdays(dates)
sum(grepl("Monday",wkdays,fixed=TRUE))
