rankhospital <- function(state, outcome,num="best") {
        ## Read outcome data (get the link from the download button in figshare)
        data <- read.csv("http://files.figshare.com/1543502/outcome_of_care_measures.csv", colClasses = "character")
        state<- toupper(state)        
        outcome <- tolower(outcome)
        num<- tolower(num)
        
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
        #Use as.numeric to convert all these codings to NAs (outcome: column 2)
        data.selected[,2]<- as.numeric(data.selected[,2])
        #Now remove NAs
        data.selected<- na.omit(data.selected)
        #Change the second column name
        colnames(data.selected)[2] <- "outcome.rate" 
        #number of observations
        n = nrow(data.selected)
        
        #Check the rank number user want to get
        if (num=="best") {
                rank = 1
        } else if (num=="worst") {
                rank = n
        } else {
                rank = as.numeric(num)
                if (is.na(rank)) stop ("invalid num")
        }
                
        
        #sort dataset first by outcome.rate then by hospital name
        data.sorted = data.selected[order(data.selected$outcome.rate,
                                          data.selected$Hospital.Name),]

        ## Return hospital name in that state with lowest 30-day death rate in 
        # rank and alphabetical order
        data.sorted$Hospital.Name[rank]
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