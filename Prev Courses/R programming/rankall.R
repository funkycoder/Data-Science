rankall <- function(outcome,num="best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  outcome <- tolower(outcome)
  num<- tolower(num)
  
  ## Check if the outcome is valid
  
  #if (!is.element(outcome,outcomes)) stop ("invalid outcome")
  # get the corresponding outcome column
  if (outcome=="heart attack") col<- 11
  else if (outcome=="heart failure") col<- 17
  else if (outcome=="pneumonia") col<- 23
  else stop ("invalid outcome")
  
  #Get the dataset with hospital name, state and the desired outcome
  data <- data[,c(2,7,col)]
  #Set proper column names
  colnames(data)<- c("hospital","state","outcome")
  #Convert outcome to number
  data$outcome <- as.numeric(data$outcome)
  #Remove NAs
  data<- na.omit(data)

  #Split data by state
  s<- split(data,data$state)
  
  result<- sapply(s,function(x){
    # Sort each data frame in s (by outcome first then hospital name)
    x<- x[order(x$outcome,x$hospital),]
    #number of hospital in each state
    n= nrow(x)
    #Check the rank number user want to get
    if (num=="best") {
      rank = 1
    } else if (num=="worst") {
      rank = n
    } else {
      rank = as.numeric(num)
      if (is.na(rank)) stop ("invalid num")
    }
    #return the name of the hospital at the desired rank
    x[rank,"hospital"]
  })
  #Get the state name
  state<- names(result)
  #Remove named vector from result
  names(result)<- NULL
  #Get the hospital name
  hospital<- result
  #Return the required data frame
  as.data.frame(cbind(hospital,state))
}
#---------------------------------------------------------------------------------------------------------------------
# This is how I implemented this function in 2013 in Computing Data Analysis Course on coursera
#---------------------------------------------------------------------------------------------------------------------
rankall2 <- function(outcome, num = "best") {
## Read outcome data
outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

## --- Check that outcome are valid



##30-Day Death (Mortality) Rates from Heart Attack [11]
##30-Day Death (Mortality) Rates from Heart Failure [17]
##30-Day Death (Mortality) Rates from Pneumonia [23]
## Hospital.Name [2]

if (outcome=="heart attack") {col=11}
else if (outcome=="heart failure") {col=17}
else if (outcome=="pneumonia") {col=23}
else {stop("invalid outcome")}

## Get the num variable
if (is.numeric(num)){ rank<-as.numeric(num)}
else if (num=="best") {rank<-1}
else if (num=="worst") {rank<-0}
else {stop("invalid num")}

hospital<-vector(mode="character",length=0)
state<-vector(mode="character",length=0)

## Get all states from the dataframe & Remove all duplicated values
stateList<-outcomeData$State[!duplicated(outcomeData$State)]
for (stateVar in stateList) 
{
	## Get the outcome with selected state 
	outcomeState<-subset(outcomeData,State==stateVar)

	## Name the death rate column
	names(outcomeState)[col]<-"Death.Rate"

	## Remove records that have NA in the death rate column
	outcomeState<-outcomeState[,c("Death.Rate","Hospital.Name")]
	
	## Remove all NA values
	outcomeState<-na.omit(outcomeState)

	## Convert death rate form character to numeric
	outcomeState[,"Death.Rate"]<-as.numeric(outcomeState[,"Death.Rate"])
	
	## sort the outcome
	outcomeSorted<-outcomeState[order(outcomeState$Death.Rate,outcomeState$Hospital.Name),]

	if (rank==1)
		{
		hospital<-c(hospital,outcomeSorted[1,2])
		}
	else if (rank==0) 
		{
		hospital<-c(hospital,outcomeSorted[nrow(outcomeSorted),2])
	
		}
	else if (rank<=nrow(outcomeSorted))
		{
		hospital<-c(hospital,outcomeSorted[rank,2])
		}
	else	{
		hospital<-c(hospital,"NA")
		}
	state<-c(state,stateVar)
}

	data.frame(hospital,state)

}

