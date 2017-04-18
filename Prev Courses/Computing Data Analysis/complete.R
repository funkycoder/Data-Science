complete <- function(directory, ids) {
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