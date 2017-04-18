#-------------------------------------------------------------------------------
# Register an application with the Github API 
# https://github.com/settings/applications.
# Access the API to get information on your instructors repositories 
# (hint: this is the url you want "https://api.github.com/users/jtleek/repos"). 
# Use this data to find the time that the datasharing repo was created. 
# What time was it created? This tutorial may be useful 
# (https://github.com/hadley/httr/blob/master/demo/oauth2-github.r). 
# ------------------------------------------------------------------------------

library(httr)
#Install devtools then use devtools to install httpuv package
# install.packages("devtools")
# devtools::install_github("httpuv", "rstudio")

# 1. Find OAuth settings for github:
#    http://developer.github.com/v3/oauth/
github<- oauth_endpoints("github")

# 2. Register an application at https://github.com/settings/applications
#    Insert your values below - if secret is omitted, it will look it up in
#    the GITHUB_CONSUMER_SECRET environmental variable.
#
#    Use http://localhost:1410 as the callback url
#-------------------------------------------------------------------------------
#    If you will authenticate your Github account interactively 
#    (using the OAuth dance, for example), you should point the Callback URL to
#    "http://localhost:1410". This is so that interactive.login works: 
#    the interactive OAuth login dancing involves redirection to a URL 
#    that is specified in advance. httr creates a web server on port 1410 
#    to catch the redirection request and the token that comes with it.
#    Source: https://github.com/cscheid/rgithub

Sys.setenv(GITHUB_CONSUMER_SECRET='c4d46b720a3c90133621edd25ccc19f139012ca0')
myapp <- oauth_app("github",key="c6952424c320c2a74d77")

# 3. Get OAuth credentials
github_token <- oauth2.0_token(github, myapp)


# 4. Use API
gtoken <- config(token = github_token)
req <- GET("https://api.github.com/users/jtleek/repos", gtoken)
json<- jsonlite::fromJSON(toJSON(content(req)))
names(json)
json$name[[5]]
json$created_at[[5]]

#How many characters are in the 10th, 20th, 30th and 100th lines of HTML from this page: 
#  http://biostat.jhsph.edu/~jleek/contact.html 
#(Hint: the nchar() function in R may be helpful)
# http://stackoverflow.com/questions/4081973/append-suffix-to-column-of-entries-in-csv-file-or-in-sqlite-database/4082480#4082480

# open="r" : to enable reading line by line
con<- url("http://biostat.jhsph.edu/~jleek/contact.html",open="r")
count<- 1
while (length(oneLine <- readLines(con, n = 1, warn = FALSE)) > 0) {
  requestedLines<- c(10,20,30,100)
  if (count %in% requestedLines) {
       char<- nchar(oneLine)
       print(char)
  }
  count<- count +1
} 

close(con)

#
# Read Fixed Width Format Files
# read.fwf(file, widths, sep=" ", as.is = FALSE, skip = 0, row.names, col.names)
# Use widths with the spaces counted. Don't use sep in this case.
# Read this data set into R and report the sum of the numbers in the fourth column. 
# https://d396qusza40orc.cloudfront.net/getdata%2Fwksst8110.for 
#Original source of the data: http://www.cpc.ncep.noaa.gov/data/indices/wksst8110.for 
#(Hint this is a fixed width file format)

data<- read.fwf("getdata-wksst8110.for",widths=c(14,14,4,10,22),skip=4)
sum(as.numeric(data$V3))
