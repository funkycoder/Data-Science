
## Return the mean of the pollutant across all monitors list
## in the 'id' vector (ignoring NA values)
pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        #The final vector which get all the non NAs value from all sites 
        pollutant.complete <- numeric(0)
        #Loop through ids
        for (x in id){
                #file name in this format "001.csv", "042.csv"
                name<- paste(formatC(x,width=3,format="d",flag="0"),"csv",sep=".")
                full.name<- paste(directory,name,sep="/")
                #Read data from the specified csv file
                data<- read.csv(full.name,header=TRUE)
                #Get the pollutant vector
                pollutant.vector <- data[,pollutant]
                #Get boolean vector where pollutant is NA
                bad <- is.na(pollutant.vector)
                #Just get the non NAs
                pollutant.clean<- pollutant.vector[!bad]
                #concatenate to a complete pollutant vector
                pollutant.complete <- c(pollutant.complete,pollutant.clean)
        } #end for
        #the mean of the pollutant across all monitors list
        mean(pollutant.complete)
}