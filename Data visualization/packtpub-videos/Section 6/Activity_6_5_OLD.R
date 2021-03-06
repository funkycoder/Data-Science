library(shiny)
library(ggplot2)



#User Interface
ui <- shinyUI(fluidPage(
  
  titlePanel("Learning R for Data Visualization"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Here you can select a file (csv or txt) and plot your data!"),
      selectInput("separator", "Data Separator:", c(Comma=",",BlankSpace=" ",Semicolon=";")),
      
      fileInput(inputId="Data", label="Select a file:", multiple=F),
      
      selectInput("TypePlot", "Type of Plot", c("None",BarChart="bar",Histogram="hist",BoxPlot="box",Scatterplot="points",TimeSeries="ts")),
      
      #HISTOGRAM
      conditionalPanel(
        condition = "input.TypePlot == 'hist'",
        uiOutput("HISTnames.selector"),
        uiOutput("HISTmulti.selector"),
        actionButton("hist.button", "Plot!")
      ),
      
      
      #BAR CHART
      conditionalPanel(
        condition = "input.TypePlot == 'bar'",
        uiOutput("BARx.selector"),
        uiOutput("BARy.selector"),
        uiOutput("BARcol.selector"),
        actionButton("barchart.button", "Plot!")
      ),
      
      
      #BOX PLOT
      conditionalPanel(
        condition = "input.TypePlot == 'box'",
        uiOutput("BOXx.selector"),
        uiOutput("BOXy.selector"),
        actionButton("box.button", "Plot!")
      ),
      
      
      #SCATTERPLOT
      conditionalPanel(
        condition = "input.TypePlot == 'points'",
        uiOutput("SCPx.selector"),
        uiOutput("SCPy.selector"),
        uiOutput("SCPcol.selector"),
        uiOutput("SCPsize.selector"),
        actionButton("sct.button", "Plot!")
      ),
      
      
      #TIME SERIES
      conditionalPanel(
        condition = "input.TypePlot == 'ts'",
        uiOutput("TSx.selector"),
        uiOutput("TSy.selector"),
        actionButton("ts.button", "Plot!")
      )
      
      
    ),
    
    
    #MAIN
    mainPanel(
      
      #HISTOGRAM
      conditionalPanel(
        condition = "input.TypePlot == 'hist'",
        plotOutput('histogram')
      ),
      
      #BAR CHART
      conditionalPanel(
        condition = "input.TypePlot == 'bar'",
        plotOutput('barchart')
      ),
      
      #BOX PLOT
      conditionalPanel(
        condition = "input.TypePlot == 'box'",
        plotOutput('boxplot')
      ),
      
      
      #SCATTERPLOT
      conditionalPanel(
        condition = "input.TypePlot == 'points'",
        plotOutput('scatterplot')
      ),
      
      
      #TIME-SERIES
      conditionalPanel(
        condition = "input.TypePlot == 'ts'",
        plotOutput('time_series')
      )
      
    )
    
  )))




#Server side script
server <- shinyServer(function(input, output) {
  
  #HISTOGRAM
  output$histogram <- renderPlot({
    
    #LOADING DATA
    FILE <- input$Data
    
    if (is.null(FILE))
      return(NULL)
    
    dat.load <- read.table(FILE$datapath, header = T, sep = input$separator)
    classes <- sapply(dat.load, class)
    
    #PANEL FOR UI
    output$HISTnames.selector <- renderUI({
      selectInput(inputId="hist.x", label="Select the variable to plot:", 
                  choices=names(dat.load)[classes=="numeric"])
    })
    
    output$HISTmulti.selector <- renderUI({
      selectInput(inputId="multi", label="Select the facets variable:", 
                  choices=c("None",names(dat.load)[classes=="factor"]))
    })
    
    #CREATE PLOT
    hist.plot <- eventReactive(input$hist.button, {
      
      if(input$multi!="None"){
        data.histogram <- data.frame(var=dat.load[,input$hist.x], 
                                     multi=dat.load[,input$multi])
        
        plot <- ggplot(data=data.histogram,aes(x=var)) + 
          geom_histogram() +
          xlab(paste(input$hist.x)) + 
          ylab("Frequency") +
          ggtitle(paste("Histogram of ",input$hist.x)) +
          facet_wrap(~multi) +
          theme_minimal()

        print(plot)
      } else {
        data.histogram <- data.frame(var=dat.load[,input$hist.x])
        
        plot <- ggplot(data=data.histogram,aes(x=var)) + 
          geom_histogram() +
          xlab(paste(input$hist.x)) + 
          ylab("Frequency") +
          ggtitle(paste("Histogram of ",input$hist.x)) +
          theme_minimal()

        print(plot)
      }
      
      
    })
    
    #PLOT!
    hist.plot()
    
  })
  
  
  #BAR CHART
  output$barchart <- renderPlot({
    
    #LOADING DATA
    FILE <- input$Data
    
    if (is.null(FILE))
      return(NULL)
    
    dat.load <- read.table(FILE$datapath, header = T, sep = input$separator)
    classes <- sapply(dat.load, class)
    
    
    #PANELS FOR UI
    output$BARx.selector <- renderUI({
      selectInput(inputId="x.bar", label="Select the variable for X:", 
                  choices=c(None=0,names(dat.load)[classes=="factor"]))
    })
    
    output$BARy.selector <- renderUI({
      selectInput(inputId="y.bar", label="Select the variable for Y:", 
                  choices=c(None=0,names(dat.load)[classes=="numeric"]))
    })
    
    output$BARcol.selector <- renderUI({
      selectInput(inputId="col.bar", label="Select the variable for color:", 
                  choices=c(None=0,names(dat.load)[classes=="numeric"]))
    })
    
    
    #CREATE PLOT
    barchart.plot <- eventReactive(input$barchart.button, {
      
      #Here we add a condition to allow users not to fill the bar-chart
      if(input$col.bar!=0){
        data.barchart <- data.frame(x=dat.load[,input$x.bar], 
                                    y=dat.load[,input$y.bar], 
                                    color=dat.load[,input$col.bar])
        
        ggplot(data=data.barchart,aes(x=x, y=y, fill=color)) + 
          geom_bar(stat="identity") +
          xlab(paste(input$x.bar)) + 
          ylab(paste(input$y.bar)) +
          theme_minimal()
      } else {
        data.barchart <- data.frame(x=dat.load[,input$x.bar], 
                                    y=dat.load[,input$y.bar])
        
        ggplot(data=data.barchart,aes(x=x, y=y)) + 
          geom_bar(stat="identity") +
          xlab(paste(input$x.bar)) + 
          ylab(paste(input$y.bar)) +
          theme_minimal()
      }
    })
    
    #PLOT!
    barchart.plot()
  })
  
  
  #BOX PLOT
  output$boxplot <- renderPlot({
    
    #LOADING DATA
    FILE <- input$Data
    
    if (is.null(FILE))
      return(NULL)
    
    dat.load <- read.table(FILE$datapath, header = T, sep = input$separator)
    classes <- sapply(dat.load, class)
    
    
    #PANELS FOR UI
    output$BOXx.selector <- renderUI({
      selectInput(inputId="x.box", label="Select the variable for X:", 
                  choices=names(dat.load)[classes=="factor"])
    })
    
    output$BOXy.selector <- renderUI({
      selectInput(inputId="y.box", label="Select the variable for Y:", 
                  choices=names(dat.load)[classes=="numeric"])
    })
    
    
    
    #CREATE PLOT
    boxplot.plot <- eventReactive(input$box.button, {
      
      data.boxplot <- data.frame(x=dat.load[,input$x.box], 
                                 y=dat.load[,input$y.box])
      
      plot <- ggplot(data=data.boxplot,aes(x=x, y=y)) + 
        geom_boxplot() +
        xlab(paste(input$x.box)) + 
        ylab(paste(input$y.box)) +
        theme_minimal()
      
      print(plot)
    })
    
    #PLOT!
    boxplot.plot()
    
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
        data.scatterplot <- data.frame(x=dat.load[,input$x.sct], 
                                       y=dat.load[,input$y.sct])
        
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
  
  
  
  #TIME-SERIES
  output$time_series <- renderPlot({
    
    #LOADING DATA
    FILE <- input$Data
    
    if (is.null(FILE))
      return(NULL)
    
    dat.load <- read.table(FILE$datapath, header = T, sep = input$separator)
    classes <- sapply(dat.load, class)
    
    
    #PANELS FOR UI 
    output$TSx.selector <- renderUI({
      selectInput(inputId="x.ts", label="Select the variable for X:", 
                  choices=names(dat.load)[classes=="factor"])
    })
    
    output$TSy.selector <- renderUI({
      selectInput(inputId="y.ts", label="Select the variable for Y:", 
                  choices=names(dat.load)[classes=="numeric"])
    })
    
    
    
    #CREATE PLOT
    time_series.plot <- eventReactive(input$ts.button, {
      data.time_series <- data.frame(x=as.Date(dat.load[,input$x.ts]), 
                                     y=dat.load[,input$y.ts])
      
      plot <- ggplot(data=data.time_series,aes(x=x, y=y)) + 
        geom_line() +
        xlab("Date") + 
        ylab(paste(input$y.ts)) +
        theme_minimal()
      
      print(plot)
    })
    
    #PLOT!
    time_series.plot()
    
  })
  
})


runApp(list(ui=ui, server=server))




