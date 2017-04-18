#---------------------------------------------------------------------------------------------------------------------
# R Programming 6/2014
#---------------------------------------------------------------------------------------------------------------------
#Plot the 30-day mortality rates for heart attack
#Read the outcome data into R via the read.csv function and look at the first few rows.
plot.outcome<- function(){
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
ncol(outcome) #number variable
names(outcome)
# Because we originally read the data in as character (by specifying colClasses = "character" 
#we need to coerce the column to be numeric. You may get a warning about NAs being introduced.
outcome[, 11] <- as.numeric(outcome[, 11])
 ## You may get a warning about NAs being introduced; that is okay
hist(outcome[, 11])          
}
best <- function(state, outcome) {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  state<- toupper(state)
  outcome <- tolower(outcome)
  ## Check that state and outcome are valid
  #State are now character. Convert them to factor and get the level
  states<- levels(as.factor(data$State)) 
  outcomes<- c("heart attack", "heart failure","pneumonia")
  if (!state %in% states) stop ("invalid state")
  #if (!is.element(outcome,outcomes)) stop ("invalid outcome")
  # get the corresponding outcome column
  if (outcome=="heart attack") col<- 11
  else if (outcome=="heart failure") col<- 17
  else if (outcome=="pneumonia") col<- 23
  else stop ("invalid outcome")
  #Get the dataset in the choosen state with hospital name and outcome
  data.selected <- data[data$State==state,c(2,col)]
  #In this dataset NA was coded as "Not Available" so na.omit could not be used
  #Use as.numeric to convert all these codings to NAs
  data.selected[,2]<- as.numeric(data.selected[,2])
  #Now remove NAs
  data.selected<- na.omit(data.selected)
  #Change the second column name
  colnames(data.selected)[2] <- "outcome.rate" 
  best.rate <- min(data.selected$outcome.rate)
  ## Return hospital name in that state with lowest 30-day death rate in alphabetical order
  best.hospital <- sort(data.selected[data.selected$outcome.rate==best.rate,1])
  best.hospital[1]
}
#---------------------------------------------------------------------------------------------------------------------
# This is how I implemented this function in 2013 in Computing Data Analysis Course on coursera
#---------------------------------------------------------------------------------------------------------------------
best2 <- function(state, outcome) {
  ## Read outcome data
  outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  
  ## Get all states from the dataframe & Remove all duplicated values
  stateData<-outcomeData$State[!duplicated(outcomeData$State)]
  if (!any(stateData==state)) stop("invalid state")
  
  ##30-Day Death (Mortality) Rates from Heart Attack [11]
  ##30-Day Death (Mortality) Rates from Heart Failure [17]
  ##30-Day Death (Mortality) Rates from Pneumonia [23]
  
  
  if (outcome=="heart attack") {col=11}
  else if (outcome=="heart failure") {col=17}
  else if (outcome=="pneumonia") {col=23}
  else {stop("invalid outcome")}
  
  ## Get the outcome with selected state only
  outcomeState<-subset(outcomeData,State==state)
  
  ## Convert death rate to number
  
  ## What is the min death rate?
  minDeath<-min(as.numeric(outcomeState[,col]),na.rm=TRUE)
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  
  bestHospitalName<-outcomeState[outcomeState[,col]==minDeath,]
  
  ##Set order
  sortedHospitalName<-bestHospitalName[order("Hospital.Name"),]
  
  ## Return the first name
  sortedHospitalName[1,"Hospital.Name"]
}