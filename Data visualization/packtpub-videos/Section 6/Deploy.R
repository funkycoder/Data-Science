#install.packages('devtools')
#Wea ready installed devtools in Section 5 so we do not have to do it again.

devtools::install_github('rstudio/shinyapps')


#This can be copied and pasted directly from the website
shinyapps::setAccountInfo(name='YOUR_SERVER_NAME',
                          token='YOUR_TOKEN',
                          secret='YOUR_SECRET')



library(shinyapps)
shinyapps::deployApp('YOUR_PATH')

#It is important that only ui.r and server.r are present in this folder