#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  sep <- reactive({
    
    return(input$separator)
    
  })
  
  load_data <- reactive({
    
    req( input$dataLoad )
    # inference <- get.delim(input$dataLoad$datapath, 1000, comment = '#',
    #                        delims = c("\t","\t| +", " ", ";", ","))
    # 
    #podmienka, ak bude delimiter v subore ',' precitame to ako csv
    if( sep() == 'csv') {
      
      read.csv(input$dataLoad$datapath, header = ifelse(!is.null(input$header), TRUE, FALSE))
      
      #podmienka, ak bude delimiter v subore '\' precitame to ako delim  
    } else if (sep() == 'tab') {
      
      read.delim(input$dataLoad$datapath, header = ifelse(!is.null(input$header), TRUE, FALSE))
      
      #podmienka, ak bude delimiter v subore ' ' precitame to ako table 
    } else  {
      
      read.table(input$dataLoad$datapath, header = ifelse(!is.null(input$header), TRUE, FALSE))
      
    }
    

  })
  
  col <- reactive({
    
    input$variables 
    
  })
  
  output$plot <- renderPlot({
    input$upload
    #plot podla zvoleneho typu plotu
    isolate({if ( input$plotType == 'Histogram' ) {
      
      ggplot( data = load_data(), aes( x = load_data()[,col()] )) +
        geom_histogram(fill = colors()[sample(1:255,1)]) + 
        xlab(col())+
        theme_app()
      
    } else if ( input$plotType == 'Scatter Plot' ) {
      
      ggplot( data = load_data(), aes( x = load_data()[,col()[1]] , 
              y = load_data()[,col()[2]] ) ) +
        geom_point() +
        theme_app()
      
    } else if ( input$plotType == 'Bar Plot' ) {
      
      ggplot( data = load_data(), aes( x =  load_data()[,col()] )) +
        geom_bar() +
        theme_app()
      
    } else if ( input$plotType == 'Kolacovy diagram' ) {
      
      ggplot( data = load_data(), aes( x =  load_data()[,col()]) ) +
        geom_bar() + coord_polar() +
        theme_app()
      
    } else if ( input$plotType == 'Boxplot' ) {
      
      ggplot( data = load_data(), aes( x = load_data()[,col()[1]] , 
                                       y = load_data()[,col()[2]] ) ) +
        geom_boxplot() +
        theme_app()
      
    }})
  })
  
  output$data <- renderDataTable({
    
    input$upload
    isolate({load_data()})
    
  })
  #zobrazi mi drop down menu, kde budu na vyber premenne, ktore sa vykreslia
  output$test <- renderUI({
    
    
    req(input$dataLoad, input$separator, input$upload)
    
    if(input$plotType == 'ScatterPlot') {
    
    isolate({
      tagList(selectInput( inputId = "variables",
                                 label = "Choose x-axis variable",
                                 choices = c(colnames(load_data()))),
              selectInput( inputId = "variables_one",
                           label = "Choose y-axis variable",
                           choices = c(colnames(load_data())))
                          )})
      
    } else {
      
      selectInput( inputId = "variables",
                   label = "Choose x-axis variable",
                   choices = c(colnames(load_data())))
      
    }
    
  })
  
    
  
  output$descr <- renderPrint({
    
    if (!is.null(input$descr_load)) {
      
      readLines(input$descr_load$datapath)
    }

    
  })
  
  variables <- reactive({
    
    cols <- colnames(load_data())
    cls <- vector(mode = 'character', length = length(cols))
    
    for (i in 1:length(cols)) {
      
       cls[i] <- class(load_data()[,cols[i]])
      
    }
    
    df <- as.data.frame(cbind(cols,cls))
    colnames(df) <- c('Variable','Class')
    
    df
    
  })
  
  output$vars <- renderDataTable({
    
    variables()
    
  })
  
  summary_vals <- reactive({
    
    columns <- subset(variables(), variables()[,2] == 'integer')
    names <- columns$Variable
    
    n <- length(names)
    min <- c()
    max <- c()
    stQ <- c()
    rdQ <- c()
    med <- c()
    mean <- c()
    sd <- c()
    skw <- c()
    kurt <- c()
    
    
    for (i in 1:n) {
      
      min[i] <- min(load_data()[,names[i]])
      max[i] <- max(load_data()[,names[i]])
      med[i] <- median(load_data()[,names[i]])
      stQ[i] <- quantile(load_data()[,names[i]], probs = 0.25, type = 1)
      rdQ[i] <- quantile(load_data()[,names[i]], probs = 0.75, type = 1)
      mean[i] <- mean(load_data()[,names[i]])
      sd[i] <- sd(load_data()[,names[i]])
      skw[i] <- skewness(load_data()[,names[i]])
      kurt[i] <- kurtosis(load_data()[,names[i]])
      
    }
    
    df <- data.table(names,min,max,med,mean,stQ,rdQ,sd,skw,kurt)
    colnames(df) <- c('Variable','Min','Max','Median','Mean','1st Quartile','3rd Quartile',
                      'Standard deviation','Skewness','Kurtosis')
    df
    
  })
  
  output$summary <- renderDataTable({
    
    summary_vals()
    
  })
  
})  
