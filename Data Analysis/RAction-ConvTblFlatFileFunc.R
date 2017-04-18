#-------------------------------------------------------#
#   CONVERT A TABLE INTO A FLAT FILE                    #
# myTable : data.frame with freq as the last column     #
# version: 1.0                                          #
# Author: Robert I.Kabacoff                             #
# Source: R in action Chapter 7, p. 158, Manning 2011   #
# Date: 4/30/2014                                       #
#-------------------------------------------------------#

table2flat <-function(myTable){
  dataFrame <- as.data.frame(myTable)
  rows <- dim(dataFrame)[1]
  cols <- dim(dataFrame)[2]
  x <- NULL
  # Repeat each row with the number in the freq column
  for (i in 1:rows) {
    # !!! freq must be numeric for this function to work
    # n= the number in the freq column corespond to this row
    n <-dataFrame$Freq[i]
    # Repeat this row n times
    for (j in 1:n){
      # Get the current row values except the freq column (last column)
      row <- dataFrame[i,c(1:(cols-1))]
      x <- rbind(x,row)
    }
  }
  # Attach row numbers as id
  row.names(x) <- c(1:dim(x)[1])
  return(x)
}
treatment<-rep(c("Placebo","Treated"),times=3)
improved<-rep(c("None","Some","Marked"),each=2)
Freq<-as.numeric(c(29,13,7,17,7,21))
mytable<-data.frame(treatment,improved,Freq)
mydata<-table2flat(mytable)
mydata
