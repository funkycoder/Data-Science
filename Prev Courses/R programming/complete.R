#---------------------------------------------------------------------------------------------------------------------
# R Programming 6/2014
#---------------------------------------------------------------------------------------------------------------------
#A function that reads a directory full of files and reports the number of 
#completely observed cases in each data file. The function should return a 
#data frame where the first column is the name of the file and the second column
#is the number of complete cases.
complete <- function(directory, id = 1:332) {
        #Vector hold number of complete obs in each data set
        nobs<- numeric(0)
        #Vector hold the correspondent id
        ids <- numeric(0)
        #Get all the csv files in the directory with its full name (with path)
        #files<- list.files(path=directory,pattern="*.csv",full.names=TRUE)
        #Loop 
        for (x in id){
                #file name in this format "001.csv", "042.csv"
                name<- paste(formatC(x,width=3,format="d",flag="0"),"csv",sep=".")
                full.name<- paste(directory,name,sep="/")
                #Read data from the specified csv file
                data<- read.csv(full.name,header=TRUE)
                #Boolean vector of NOT NAs in nitrate obs
                nitrate.clean <- !is.na(data$nitrate)
                #Boolean vector of NOT NAs in sulfate obs
                sulfate.clean <- !is.na(data$sulfate)
                #Complete observation boolean vector
                complete.obs <- nitrate.clean & sulfate.clean
                #Number of complete obs in this dataset
                n<- sum(complete.obs)
                #id of this dataset
                id<- data[1,"ID"]
                #Store in appropriate vector
                ids<- c(ids,id)
                nobs<- c(nobs,n)
        }
        as.data.frame(cbind(ids,nobs))
}
#---------------------------------------------------------------------------------------------------------------------
# This is how I implemented this function in 2013 in Computing Data Analysis Course on coursera
# completeData<-na.omit(read.csv(fileName)) <- COOL
#---------------------------------------------------------------------------------------------------------------------
complete2 <- function(directory, ids) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## get current directory
  currentDir <- getwd()
  ## contructs data directory
  directory<-paste(currentDir,"/",directory,"/",sep="")
  
  ## declare a vec count
  vecCount<-NULL
  
  ## 'ids' is an integer vector indicating the monitor ID numbers
  ## to be used
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
    ## load number of complete cases to the vec count
    vecCount<-c(vecCount,nrow(completeData))
    
  }        
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  data.frame(id=ids,nobs=vecCount)
}