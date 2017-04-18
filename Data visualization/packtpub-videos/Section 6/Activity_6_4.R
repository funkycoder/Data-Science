library(shiny)
library(ggplot2)



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
                            Scatterplot="points")),
      #HISTOGRAM
      conditionalPanel(
        condition = "input.TypePlot == 'hist'",
        uiOutput("HISTnames.selector"),
        uiOutput("HISTmulti.selector"),
        actionButton("hist.button", "Plot!")
      ),
      
      #SCATTERPLOT
      conditionalPanel(
        condition = "input.TypePlot == 'points'",
        uiOutput("SCPx.selector"),
        uiOutput("SCPy.selector"),
        uiOutput("SCPcol.selector"),
        uiOutput("SCPsize.selector"),
        actionButton("sct.button", "Plot!")
      )
 
    ),
    
    mainPanel(
      #Add Elements here
      
      #HISTOGRAM
      conditionalPanel(
        condition = "input.TypePlot == 'hist'",
        plotOutput('histogram')
      ),
      
      #SCATTERPLOT
      conditionalPanel(
        condition = "input.TypePlot == 'points'",
        plotOutput('scatterplot')
      )
      
    )
  )
))




#Server side script
server <- shinyServer(function(input, output) {
  #Add Elements here
  
  #HISTOGRAM
  output$histogram <- renderPlot({
    
    #LOADING DATA
    FILE <- input$Data
    
    if (is.null(FILE))
      return(NULL)
    
    Data <- read.table(FILE$datapath, header = T, sep = input$separator)
    
    classes <- sapply(Data, class)
    
    #PANEL FOR UI
    output$HISTnames.selector <- renderUI({
      selectInput(inputId="hist.x", label="Select the variable to plot:", 
                  choices=names(Data)[classes=="numeric"])
    })
    
    output$HISTmulti.selector <- renderUI({
      selectInput(inputId="multi", label="Select the facets variable:", 
                  choices=c("None",names(Data)[classes=="factor"]))
    })
    
    #CREATE PLOT
    hist.plot <- eventReactive(input$hist.button, {
      
      if(input$multi!="None"){
        data.histogram <- data.frame(var=Data[,input$hist.x], 
                                     multi=Data[,input$multi])
        
        plot <- ggplot(data=data.histogram,aes(x=var)) + 
          geom_histogram() +
          xlab(paste(input$hist.x)) + 
          ylab("Frequency") +
          ggtitle(paste("Histogram of",input$hist.x)) +
          facet_wrap(~multi) +
          theme_minimal()
        
        print(plot)
        
      } else {
        data.histogram <- data.frame(var=Data[,input$hist.x])
        
        plot <- ggplot(data=data.histogram,aes(x=var)) + 
          geom_histogram() +
          xlab(paste(input$hist.x)) + 
          ylab("Frequency") +
          ggtitle(paste("Histogram of",input$hist.x)) +
          theme_minimal()
        
        
        print(plot)
      }
      
      
    })
    
    
    #PLOT!
    hist.plot()
    
  })
  
  
  #SCATTERPLOT
  output$scatterplot <- renderPlot({
    
    #LOADING DATA
    FILE <- input$Data
    
    if (is.null(FILE))
      return(NULL)
    
    dat.load <- read.table(FILE$datapath, header = T, sep = input$separator)
    classes <- sapply(dat.load, class)
    
    
    #PANELS FOR UI
    output$SCPx.selector <- renderUI({
      selectInput(inputId="x.sct", label="Select the variable for X:", 
                  choices=c(None=0,names(dat.load)[classes=="numeric"]))
    })
    
    output$SCPy.selector <- renderUI({
      selectInput(inputId="y.sct", label="Select the variable for Y:", 
                  choices=c(None=0,names(dat.load)[classes=="numeric"]))
    })
    
    output$SCPcol.selector <- renderUI({
      selectInput(inputId="col.sct", label="Select the variable for color:", 
                  choices=c(None=0,names(dat.load)[classes=="numeric"]))
    })
    
    output$SCPsize.selector <- renderUI({
      selectInput(inputId="size.sct", label="Select the variable for size:", 
                  choices=c(None=0,names(dat.load)[classes=="numeric"]))
    })
    
    
    #CREATE PLOT
    scatterplot.plot <- eventReactive(input$sct.button, {
      
      if(input$col.sct!=0&input$size.sct==0){
        data.scatterplot <- data.frame(x=dat.load[,input$x.sct], 
                                       y=dat.load[,input$y.sct], 
                                       color=dat.load[,input$col.sct])
        
        plot <- ggplot(data=data.scatterplot, aes(x=x, y=y)) +
          geom_point(mapping=aes(color=color)) +
          ggtitle(paste0("Scatterplot ",input$x.sct, " vs. ", input$y.sct)) +
          labs(color=paste(input$col.sct)) +
          xlab(paste(input$x.sct)) + 
          ylab(paste(input$y.sct)) +
          theme_minimal()
        
        print(plot)
        
      } else if(input$col.sct!=0&input$size.sct!=0){
        data.scatterplot <- data.frame(x=dat.load[,input$x.sct], 
                                       y=dat.load[,input$y.sct], 
                                       color=dat.load[,input$col.sct], 
                                       size=dat.load[,input$size.sct])
        
        plot <- ggplot(data=data.scatterplot, aes(x=x, y=y)) +
          geom_point(mapping=aes(color=color, size=size)) +
          ggtitle(paste0("Scatterplot ",input$x.sct, " vs. ", input$y.sct)) +
          labs(color=paste(input$col.sct), size=paste(input$size.sct)) +
          xlab(paste(input$x.sct)) + 
          ylab(paste(input$y.sct)) +
          theme_minimal()
        
        print(plot)
        
      } else {
        data.scatterplot <- data.frame(x=dat.load[,input$x.sct], y=dat.load[,input$y.sct])
        
        plot <- ggplot(data=data.scatterplot, aes(x=x, y=y)) +
          geom_point() +
          ggtitle(paste0("Scatterplot ",input$x.sct, " vs. ", input$y.sct)) +
          xlab(paste(input$x.sct)) + 
          ylab(paste(input$y.sct)) +
          theme_minimal()
        
        print(plot)
      }
    })
    
    #PLOT!
    scatterplot.plot()
  })
  
  
})


runApp(list(ui=ui, server=server))




