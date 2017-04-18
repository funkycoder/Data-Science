getmonitor <- function(id, directory, summarize = FALSE) {
    	  ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
	  
	  ## get local directory
	  currentDir <-getwd()
	  directory <-paste(currentDir,"/",directory,"/",sep='')

        ## 'id' is a vector of length 1 indicating the monitor ID
        ## number. The user can specify 'id' as either an integer, a
        ## character, or a numeric.

        ##  prefix the file name with '00' if id<100
	  if (as.numeric(id) < 10) 
		{
			fileName<-paste(directory,"00",id,".csv",sep='')
		}
	  else if (as.numeric(id) < 100) 
		{
			fileName<-paste(directory,"0",id,".csv",sep='')
		}
	  else
		{
			fileName<-paste(directory,id,".csv",sep='')
		}  

	  ## get the dataframe
	  dataFrame<-read.csv(fileName)
	  
        ## 'summarize' is a logical indicating whether a summary of
        ## the data should be printed to the console; the default is
        ## FALSE
        if (summarize==TRUE)
		{
			print(summary(dataFrame))
		}

	  ## return the dataframe
	  return(dataFrame)

}