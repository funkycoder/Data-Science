library(shiny)
library(ggplot2)


#Simple File Import 

#User Interface
ui <- shinyUI(fluidPage(
  
  #Title
  titlePanel("Learning R for Data Visualization"),
  
  #Sidebar
  sidebarLayout(
    sidebarPanel(
      
      #Add Elements here
      helpText("How to create a simple website in Shiny"),
      
      fileInput(inputId="Data", label="Select a CSV file:", 
                multiple=F),
      
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
    
    #LOADING DATA
    FILE <- input$Data
    
    if (is.null(FILE))
      return(NULL)
    
    Data <- read.table(FILE$datapath, header = T, 
                           sep = ",")
    
    
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




#Add a way to select the file separator

#User Interface
ui <- shinyUI(fluidPage(
  
  #Title
  titlePanel("Learning R for Data Visualization"),
  
  #Sidebar
  sidebarLayout(
    sidebarPanel(
      
      #Add Elements here
      helpText("How to create a simple website in Shiny"),
      
      selectInput("separator", "Data Separator:", 
                  c(Comma=",",BlankSpace=" ",Semicolon=";")),
      
      fileInput(inputId="Data", label="Select a file:", multiple=F),
      
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
    
    #LOADING DATA
    FILE <- input$Data
    
    if (is.null(FILE))
      return(NULL)
    
    Data <- read.table(FILE$datapath, header = T, 
                           sep = input$separator)
    
    
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







