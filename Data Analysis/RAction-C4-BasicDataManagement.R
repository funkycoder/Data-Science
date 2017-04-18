################################################################################
# CHAPTER 4: Basic Data Management
################################################################################
#Creating the leadership data frame
manager<- c(1,2,3,4,5)
date<- c("10/24/08","10/28/08","10/1/08","10/12/08","5/1/09")
country<- c("US","US","UK","UK","UK")
gender<- c("M","F","F","M","F")
age<- c(32,45,25,39,99)
q1<- c(5,3,3,3,2)
q2<- c(4,5,5,3,2)
q3<- c(5,2,5,4,1)
q4<- c(5,5,5,NA,2)
q5<- c(5,5,2,NA,1)
leadership<- data.frame(manager,date,country,gender,age,q1,q2,q3,q4,q5,stringsAsFactors=FALSE)
#Create new variable
leadership$age[leadership$age==99]<-NA
leadership$agecat[leadership$age>75]<- "Elder"
leadership$agecat[leadership$age>=55 &
                  leadership$age<=75] <- "Middle Age"
leadership$agecat[leadership$age<55]<- "Young"

#Or more compact format
leadership<- within(leadership,{
                    agecat<-NA
                    agecat[age>75] <- "Elder"
                    agecat[age>=55 & age<=75]<- "Middle Age"
                    agecat[age<55]<- "Young"
                    
                    })
#renaming variable
fix(leadership)
#reshape:rename
library(reshape)
leadership<- rename(leadership,c(manager="managerID",date="testDate"))
#or
names(leadership)[2]<-"myDate"
names(leadership)[6:10]<- c("item1","item2","item3","item4","item5")
#Delete incomplete observations
newdata <- na.omit(leadership)
#Date values
#Default format for inputting Date is yyyy-mm-dd
myDate<- as.Date(c("2007-07-02","2008-09-02"))
#other format
strDates<- c("01/05/1994","08/16/2001")
date<- as.Date(strDates,"%m/%d/%Y")
#Other functions
#current date
Sys.Date()
#current date and time
date()
#format
format(Sys.Date(),format="%d %B %Y")
format(Sys.Date(),format="%A")

#Selecting variables
newdata<- leadership[,c(6:10)]

myvars<- c("q1","q2","q3","q4")
newdata<- leadership[myvars]

myvars<- paste("q",1:5,sep="")
newdata<- leadership[myvars]

##Dropping variables
#Exclude q3 and q4
myvars<- names(leadership) %in% c("q3","q4")
newdata<- leadership[!myvars]

newdata<- leadership[c(-8,-9)]

leadership$q3<- leadership$q4<- NULL

##Selecting observations
leadership$date<- as.Date(leadership$date,"%m/%d/%y")
startdate<- as.Date("2009-01-01")
enddate<- as.Date("2009-10-31")
newdata<- leadership[which(leadership$date>=startdate&leadership$date<=enddate),]

#The subset function
newdata<- subset(leadership,age>=35|age<24,select=c(q1,q2,q3,q4))
newdata<- subset(leadership,gender=="M"&age>25,select=gender:q4)

#Random sample
mysample<- leadership[sample(1:nrow(leadership),3,replace=FALSE),]
