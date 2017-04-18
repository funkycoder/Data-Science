#Import ggplot2
library(ggplot2)

#Set the working directory where the dataset is stored
setwd("E:/OneDrive/R Video Course - Packt/Data")

#Load the dataset in R
Data <- read.table(file="EPA_Data.csv", 
                   sep=",", 
                   header=TRUE, 
                   colClasses=c("Date","factor",rep("numeric",5)), 
                   na.string="NA")


#Here we are goign to use the subsets we created before
California <- Data[Data$State=="California",]

Pollution_1stOct2013 <- Data[Data$Date=="2013-10-01",]


#Add a Linear Trend Line
ggplot(data=California, aes(x=NO2, y=SO2)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE)



#Add a quadratic Trend Line
ggplot(data=California, aes(x=NO2, y=SO2)) +
  geom_point() +
  geom_smooth(method = "lm", formula=y ~ poly(x, 2), se = TRUE)



#Add Vertical Lines
ggplot(data=California, aes(NO2)) + 
  geom_histogram() +
  geom_vline(xintercept = mean(California$NO2),
             color="red",
             linetype = "longdash")
#More info on the line types available can be found here:
#http://sape.inf.usi.ch/quick-reference/ggplot2/linetype



#Add Horizontal Lines
ordered <- Pollution_1stOct2013[order(Pollution_1stOct2013$SO2),]

ggplot(data=Pollution_1stOct2013, aes(x=State, y=SO2)) +
  geom_bar(stat="identity") +
  scale_x_discrete(limits=ordered$State) +
  geom_hline(yintercept = 0.5,
             color="red",
             linetype = "longdash")




