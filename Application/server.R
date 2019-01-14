#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

##########################
#      	  E D A          #
##########################

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
    req(input$upload,input$show_descr)
    #plot podla zvoleneho typu plotu
    if ( input$plotType == 'Histogram' ) {
      
      if( mode(load_data()[,col()]) == 'numeric'  & class(load_data()[,col()]) != 'factor') {
        
        ggplot( data = load_data(), aes( x = load_data()[,col()] )) +
          geom_histogram(fill = colors()[sample(1:255,1)]) + 
          xlab(col()) +
          theme_app()
      } else {
        return(NULL)
      }
    } else if ( input$plotType == 'ScatterPlot' ) {
      
      var1 <- input$variables
      var2 <- input$variables_one
      
      if(mode(load_data()[,col()]) == 'numeric') {
        
        ggplot( data = load_data(), aes( x = load_data()[,var1] , 
                                         y = load_data()[,var2] ) ) +
          geom_point() +
          theme_app()
      } else {
        return(NULL)
      }
    } else if ( input$plotType == 'BarPlot' ) {
      
      if(mode(load_data()[,col()]) == 'numeric'  & class(load_data()[,col()]) != 'factor') {
        
        ggplot( data = load_data(), aes( x =  load_data()[,col()] )) +
          geom_bar() +
          theme_app()
      } else {
        
        return(NULL)
      }
    } else if ( input$plotType == 'Pie' ) {
      
      if(mode(load_data()[,col()]) == 'numeric'  & class(load_data()[,col()]) != 'factor') {
        
        ggplot( data = load_data(), aes( x =  load_data()[,col()]) ) +
          geom_bar() + coord_polar() +
          theme_app()
      } else {
        
        
        return(NULL)
      }
    } else if ( input$plotType == 'Boxplot' ) {
      
      if(mode(load_data()[,col()]) == 'numeric' & class(load_data()[,col()]) != 'factor') {
        
        ggplot( data = load_data(), aes( x = load_data()[,col()] , 
                                         y = load_data()[,col()] ) ) +
          geom_boxplot() +
          theme_app()
      } else {
        
        return(NULL)
        
      }
    }
  })
  
  output$data <- renderTable({
    
    input$upload
    isolate({load_data()})
    
  })
  #zobrazi mi drop down menu, kde budu na vyber premenne, ktore sa vykreslia
  output$test <- renderUI({
    
    
    req(input$dataLoad, input$separator, input$upload)
    
    if(input$plotType == 'ScatterPlot') {
      
      isolate({
        tagList(div(style = "display:inline-block",selectInput( inputId = "variables",
                                                                label = "Choose x-axis variable",
                                                                choices = c(colnames(load_data())),
                                                                width = '50%')),
                div(style = "display:inline-block",selectInput( inputId = "variables_one",
                                                                label = "Choose y-axis variable",
                                                                choices = c(colnames(load_data())),
                                                                width = '50%')
                ))})
      
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
  
  output$vars <- renderTable({
    
    variables()
    
  })
  
  
  
  summary_vals <- reactive({
    
    req(input$dataLoad,input$variables,input$header,input$show_descr)
    col.name  <- col()
    char <- rep('-',length(col.name))
    
    if (mode(load_data()[,col.name]) == 'numeric' & class(load_data()[,col.name]) != 'factor') {
      
      min  <- min(load_data()[,col.name])
      max  <- max(load_data()[,col.name])
      med  <- median(load_data()[,col.name])
      stQ  <- quantile(load_data()[,col.name], probs = 0.25, type = 1)
      rdQ  <- quantile(load_data()[,col.name], probs = 0.75, type = 1)
      mean <- mean(load_data()[,col.name])
      sd   <- sd(load_data()[,col.name])
      skw  <- skewness(load_data()[,col.name])
      kurt <- kurtosis(load_data()[,col.name])
      
      df <- data.table(col.name,min,max,med,mean,stQ,rdQ,sd,skw,kurt)
      colnames(df) <- c('Variable','Min','Max','Median','Mean','1st Quartile','3rd Quartile',
                        'Standard deviation','Skewness','Kurtosis')
      
    } else if (typeof(load_data()[,col.name]) == 'character') {
      
      df <- data.table(t(char))
      colnames(df) <- c('Variable','Min','Max','Median','Mean','1st Quartile','3rd Quartile',
                        'Standard deviation','Skewness','Kurtosis')
      
    } else if (class(load_data()[,col.name]) == 'factor') {
      
      string <- levels(load_data()[,col.name])
      df <- data.table(string)
      colnames(df) <- col.name
      rownames(df) <- paste("V",1:length(string),sep = "")
      df <- head(df,10)
    }
    
    df
    
  })
  
  output$characteristics <- renderTable({
    
    summary_vals()
    
  })
  
  ##########################
  #     M O D E L          #
  ##########################
  
  
  observe({
    
    updateSelectInput(session,"pred_var",choices = colnames(load_data()))	
    
  })
  
  observeEvent(input$depend_var,{
    
    showModal(modalDialog( title = "Choose features",
                           checkboxGroupInput( inputId = "test_check",
                                               label = "Features",
                                               choices = c( colnames( load_data() ) ) ) ) )
    
  })
  
  output$apply_model <- renderUI({
  
		if (length(input$test_check) != 0) {
		
			actionButton(inputId = "apply_ml",
						 label = "Apply technique")
		
		} else {
		
			return(NULL)
		
		}
  
  })
  
  output$regression <- renderPrint({
    
    req(input$test_check,input$apply_ml)
    isolate({
    pred.var <- input$pred_var
    features <- input$test_check
    formula <- as.formula(paste(pred.var,paste(features,collapse = "+"), sep = "~"))
    model.lm <- lm(formula, data = load_data())
    return(summary(model.lm))
    })
    
    
  })
  
  ##########################
  #      HYPOTHESIS        #
  ##########################  
  
   
})