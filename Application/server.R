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
  
  load_data <- reactive({
    
    req( input$dataLoad )
    inference <- get.delim(input$dataLoad$datapath, 10, comment = '#',
                           delims = c("\t","\t| +", " ", ";", ","))
    
    #podmienka, ak bude delimiter v subore ',' precitame to ako csv
    if( inference == ',') {
      
      read.csv(input$dataLoad$datapath, header = TRUE)
      
      #podmienka, ak bude delimiter v subore '\' precitame to ako delim  
    } else if (inference == '\t') {
      
      read.delim(input$dataLoad$datapath, header = TRUE)
      
      #podmienka, ak bude delimiter v subore ' ' precitame to ako table 
    } else if (inference == ' ') {
      
      read.table(input$dataLoad$datapath, header = TRUE)
      
    }
    
  })
  
  output$plot <- renderPlot(
    #plot podla zvoleneho typu plotu
    if ( input$plotType == 'Histogram' ) {
      
      ggplot( data = load_data()[,1:2], aes( x = x )) +
        geom_histogram()
      
    } else if ( input$plotType == 'Scatter Plot' ) {
      
      ggplot( data = load_data()[,1:2], aes( x = x, y = y ) ) +
        geom_point()
      
    } else if ( input$plotType == 'Bar Plot' ) {
      
      ggplot( data = load_data()[,1:2], aes( x = x) ) +
        geom_bar()
      
    } else if ( input$plotType == 'Kolacovy diagram' ) {
      
      ggplot( data = load_data()[,1:2], aes( x = x) ) +
        geom_bar() + coord_polar()
      
    } else if ( input$plotType == 'Boxplot' ) {
      
      ggplot( data = load_data()[,1:2], aes( x = , y = y) ) +
        geom_boxplot()
      
    }
  )
  
})  
