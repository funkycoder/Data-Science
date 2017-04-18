count <- function(cause = NULL) {
## Check that "cause" is non-NULL; else throw error
if(is.null(cause)) stop("cause must be provided")

## Check that specific "cause" is allowed; else throw error
causeOfDeath<-c("asphyxiation","blunt force","other","shooting","stabbing","unknown")
if (length(grep(cause,causeOfDeath))==0) {stop("that cause is not allowed")}

## Read "homicides.txt" data file
homicides<-readLines("homicides.txt")
## Extract causes of death
## Get the first char of cause
firstChar<-substr(cause,1,1)
## At a capitalized char in front : [Ss]
firstChar<-paste("[",toupper(firstChar),firstChar,"]",sep="")
## Get the remaining chars : hooting
tempCause<-substr(cause,2,nchar(cause))
## "<dd>[Cc]ause: [Ss]hooting(.*?)</dd>"
causeExpression<-paste("<dd>[Cc]ause: ",firstChar,tempCause,"(.*?)</dd>",sep="")
## Return integer containing count of homicides for that cause
length(grep(causeExpression,homicides))
}