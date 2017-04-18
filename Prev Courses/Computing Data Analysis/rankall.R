rankall <- function(outcome, num = "best") {
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

