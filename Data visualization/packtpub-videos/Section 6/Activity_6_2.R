#install.packages("shiny")
library(shiny)
library(ggplot2)

#Load the dataset in R
setwd("E:/OneDrive/R Video Course - Packt/Data")

Data <- read.table(file="EPA_Data.csv", 
                   sep=",", 
                   header=TRUE, 
                   colClasses=c("Date","factor",rep("numeric",5)), 
                   na.string="NA")



#The basic elements of a Shiny Website

#User Interface
ui <- shinyUI(fluidPage(
  
  #Title
  titlePanel("Title text"),
  
  #Sidebar
  sidebarLayout(
    sidebarPanel(
      #Add Elements here
    
    ),
    
  mainPanel(
      #Add Elements here
      
      
    )
  )
))
  
  
  
#Server side script
server <- shinyServer(function(input, output) {
   #Add Elements here
    
    
})
  


runApp(list(ui=ui, server=server))




#Let's create a simple website for our data
#User Interface
ui <- shinyUI(fluidPage(
  
  #Title
  titlePanel("Learning R for Data Visualization"),
  
  #Sidebar
  sidebarLayout(
    sidebarPanel(
      
      #Add Elements here
      helpText("How to create a simple website in Shiny"),
      
      selectInput(inputId="TypePlot", label="Type of Plot", 
                  choices=c(None=0,
                    Histogram="hist",
                    Scatterplot="points"))
      
      ),
    
    mainPanel(
      #Add Elements here
      plotOutput('plot')
      
    )
  )
))




#Server side script
server <- shinyServer(function(input, output) {
  #Add Elements here
  output$plot <- renderPlot({
    
    if(input$TypePlot!=0){
      
      if(input$TypePlot=="hist"){
        ggplot(data=Data, aes(x=NO2)) + 
          geom_histogram() 
        
      } else {
        ggplot(data=Data, aes(x=NO2, y=CO)) +
          geom_point()
      }
    }
    
  })
  
})


runApp(list(ui=ui, server=server))

