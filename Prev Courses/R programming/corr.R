#---------------------------------------------------------------------------------------------------------------------
# R Programming 6/2014
#---------------------------------------------------------------------------------------------------------------------
#function that takes a directory of data files and a threshold for complete cases
#and calculates the correlation between sulfate and nitrate for monitor locations 
#where the number of completely observed cases (on all variables) is greater than
#the threshold. The function should return a vector of correlations for the 
#monitors that meet the threshold requirement. If no monitors meet the threshold
#requirement, then the function should return a numeric vector of length 0
corr <- function(directory, threshold = 0) {   
        #Vector hold the correlation. Vector of length 0 to start with
        corr.vector <- numeric(0)
        #Get all the csv files in the directory with its full name (with path)
        files<- list.files(path=directory,pattern="*.csv",full.names=TRUE)
        #Loop 
        for (file in files){
                #Read data from the specified csv file
                data<- read.csv(file,header=TRUE)
                #Boolean vector of NOT NAs in nitrate obs
                nitrate.clean <- !is.na(data$nitrate)
                #Boolean vector of NOT NAs in sulfate obs
                sulfate.clean <- !is.na(data$sulfate)
                #Complete observation boolean vector
                complete.obs <- nitrate.clean & sulfate.clean
                #Number of complete obs in this dataset
                n<- sum(complete.obs)
                #Number of complete obs smaller or equal to threshold?
                #skip to next file
                if (n <= threshold){
                        next
                }
                #Get non NAs obs for each 
                sulfate.complete <- data[complete.obs,"sulfate"]
                nitrate.complete <- data[complete.obs,"nitrate"]
                #Calculate correlation
                corr<- cor(sulfate.complete,nitrate.complete)
                #Store in the corr.vector
                corr.vector <- c(corr.vector,corr)
        }
        corr.vector
}
#---------------------------------------------------------------------------------------------------------------------
# This is how I implemented this function in 2013 in Computing Data Analysis Course on coursera
# completeData<-na.omit(read.csv(fileName)) <- COOL
#---------------------------------------------------------------------------------------------------------------------
corr <- function(directory, threshold=0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## get current directory
  currentDir <- getwd()
  ## contructs data directory
  directory<-paste(currentDir,"/",directory,"/",sep="")
  
  ## declare ids
  ids<-c(1:332)
  ## declare return corr vector
  vecCorr<-vector(mode="numeric",length=0)
  
  ## construct file names and loop through them
  for (i in ids) 
  {
    if (i<10) 
    {
      fileName<-paste(directory,"00",i,".csv",sep="")
    }
    else if (i<100) 
    {
      fileName<-paste(directory,"0",i,".csv",sep="")
    }
    else 
    {
      fileName<-paste(directory,i,".csv",sep="")
    }
    ## load the complete cases in each file
    
    completeData<-na.omit(read.csv(fileName))
    ## load number of complete cases to nCase
    nCase<-nrow(completeData)
    
    if (nCase>threshold)
    {	## get the cor for the completeData
      temp<-cor(completeData$sulfate,completeData$nitrate, use = "everything",method = c("pearson", "kendall", "spearman"))
      ## update corr vector
      vecCorr<-c(vecCorr,temp)
    }
    
  }        
  
  ## Return corr vector
  return(vecCorr)        
}