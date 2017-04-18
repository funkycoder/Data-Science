agecount <- function(age = NULL) {
## Check that "age" is non-NULL; else throw error
if(is.null(age)) stop("cause must be provided")

## Read "homicides.txt" data file
homicides<-readLines("homicides.txt")

## <dd>(.*?)30 years old</dd>
causeExpression<-paste("<dd>(.*?)",age," [Yy]ears [Oo]ld</dd>",sep="")
## Return integer containing count of homicides for that cause
length(grep(causeExpression,homicides))
}