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
      
      breaks <- hist(load_data()[,col()])$breaks
      
      if( mode(load_data()[,col()]) == 'numeric'  & class(load_data()[,col()]) != 'factor') {
        
		c <- sample(1:657,1) 
		
        ggplot( data = load_data(), aes( x = load_data()[,col()] )) +
          geom_histogram(fill = colors()[ifelse(c %in% seq(152,359,1),sample(1:657,1),c)],aes(y = ..count../sum(..count..)),
		  breaks = breaks, color = "black") + 
          xlab(col()) +
		  ylab('Proportions of data') +
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
  
  observe({
    
    updateSelectInput(session,"test_var",choices = colnames(load_data()))
	updateSelectInput(session,"test_var1",choices = colnames(load_data()))	
    
  })
  
  observeEvent(input$hyp_var,{
    
	if (input$one_s_test == "One sample Kolomgorov - Smirnov test") {
		
		showModal(modalDialog( title = "Type of hypothesis",
								radioButtons( inputId = "hyp_check",
                                               label = "Hypothesis",
                                               choices = c("H0 : FX = F0 vs H1: FX != F0")),
								selectInput(inputId = "dist_type",
											label = "Which type of distribution you want you data compare to?",
											choices = c("Normal",
													  "Uniform",
													  "Exponential")),
								conditionalPanel(
									condition = "input.dist_type == 'Normal'",
									numericInput(inputId = "norm_mean",
												 label = "Set mean of normal distribution",
												 value = 0),
									numericInput(inputId = "norm_var",
												 label = "Set variance of normal distribution",
												 value = 1)),
							    conditionalPanel(
									condition = "input.dist_type == 'Exponential'",
									numericInput(inputId = "exp_mean",
												 label = "Set mean of exponential distribution",
												 value = 1)),
								conditionalPanel(
									condition = "input.dist_type == 'Uniform'",
									sliderInput(inputId = "unif_pars",
												 label = "Set interval of uniform distribution",
												 value = c(0,1),
												 min = -100, 
												 max = 100))))
	} else {
	
		showModal(modalDialog( title = "Type of hypothesis",
								radioButtons( inputId = "hyp_check",
                                               label = "Hypothesis",
                                               choices = c("H0 : mu = mu0 vs H1: mu != mu0",
                                                           "H0 : mu <= mu0 vs H1: mu > mu0",
                                                           "H0 : mu => mu0 vs H1: mu < mu0")),
								numericInput(inputId = "hyp_par",
                                        label = "Set mu0",
                                        value = 0)))
	}
    
  })
  
  mu0 <- reactive({
    
    return(input$hyp_par)
    
  })
  
   X <- reactive({
    
    return(assign(input$test_var, load_data()[,colnames(load_data()) == input$test_var]))
    
  })
  
  Y <- reactive({
  
	return(assign(input$test_var1, load_data()[,colnames(load_data()) == input$test_var1]))
	
	})

  distr <- reactive({
  
	if(input$dist_type == "Normal") {
	
		return("pnorm")
	
	} else if (input$dist_type == "Exponential") {
	
		return("pexp")
	
	} else {
	
		return("punif")
	}
	
  })
  
  params <- reactive({
  
   if(input$dist_type == "Normal") {
	
		return(c(input$norm_mean,input$norm_var))
	
	} else if (input$dist_type == "Exponential") {
	
		return(input$exp_mean)
	
	} 
	
  
  })
  output$testOutput <- renderPrint({
  
    h1 <- "H0 : mu = mu0 vs H1: mu != mu0"
	h2 <- "H0 : mu <= mu0 vs H1: mu > mu0"
	h3 <- "H0 : mu >= mu0 vs H1: mu < mu0"
  
    
    
	#One sample 
	
	if (input$tests == "One sample for quantitative data") {

		# One sample T test
		if(input$one_s_test == "One sample T-test") {
		
		    req(input$hyp_var,input$hyp_check,input$hyp_par)
		
			if (input$hyp_check == "H0 : mu = mu0 vs H1: mu != mu0") {
      
				return(t.test(assign(input$test_var,X()),mu = mu0()))
   
			} else if (input$hyp_check == "H0 : mu <= mu0 vs H1: mu > mu0") {
      
				return(t.test(assign(input$test_var,X()),mu = mu0(), alternative = "greater"))
      
			} else if (input$hyp_check == "H0 : mu >= mu0 vs H1: mu < mu0") {
      
				return(t.test(assign(input$test_var,X()),mu = mu0(), alternative = "less"))
      
			}
    
	    # One sample wilcoxon
		} else if (input$one_s_test == "One sample Wilcoxon rank sum test") {
		
		
			req(input$hyp_var,input$hyp_check,input$hyp_par)
			
			if (input$hyp_check == "H0 : mu = mu0 vs H1: mu != mu0") {
      
				return(wilcox.test(assign(input$test_var,X()),mu = mu0()))
   
			} else if (input$hyp_check == "H0 : mu <= mu0 vs H1: mu > mu0") {
      
				return(wilcox.test(assign(input$test_var,X()),mu = mu0(), alternative = "greater"))
      
			} else if (input$hyp_check == "H0 : mu >= mu0 vs H1: mu < mu0") {
      
				return(wilcox.test(assign(input$test_var,X()),mu = mu0(), alternative = "less"))
      
			}
		#One sample signed test
		} else if (input$one_s_test == "One sample signed test") {
			
			req(input$hyp_var,input$hyp_check,input$hyp_par)
			
			if (input$hyp_check == "H0 : mu = mu0 vs H1: mu != mu0") {
      
				return(wilcox.test(assign(input$test_var,X()),mu = mu0()))
   
			} else if (input$hyp_check == "H0 : mu <= mu0 vs H1: mu > mu0") {
      
				return(wilcox.test(assign(input$test_var,X()),mu = mu0(), alternative = "greater"))
      
			} else if (input$hyp_check == "H0 : mu >= mu0 vs H1: mu < mu0") {
      
				return(wilcox.test(assign(input$test_var,X()),mu = mu0(), alternative = "less"))
      
			}
		#One sample KS test
		} else if (input$one_s_test == "One sample Kolomgorov - Smirnov test") {
		
            req(input$dist_type,input$hyp_check)
			
			return(ks.test(X(),distr(),params()[1],params()[2]))
   
		} else if (input$one_s_test == "One sample signed test") {
		
			req(input$hyp_var,input$hyp_check,input$hyp_par)
			
			if (input$hyp_check == "H0 : mu = mu0 vs H1: mu != mu0") {
      
				return(wilcox.test(assign(input$test_var,X()),mu = mu0()))
   
			} else if (input$hyp_check == "H0 : mu <= mu0 vs H1: mu > mu0") {
      
				return(wilcox.test(assign(input$test_var,X()),mu = mu0(), alternative = "greater"))
      
			} else if (input$hyp_check == "H0 : mu >= mu0 vs H1: mu < mu0") {
      
				return(wilcox.test(assign(input$test_var,X()),mu = mu0(), alternative = "less"))
      
			}
			
		#One sample chi square test
		} else if (input$one_s_test == "One sample chi square test on sample variance") {
		
			req(input$hyp_var,input$hyp_check,input$hyp_par)
			
			if (input$hyp_check == "H0 : mu = mu0 vs H1: mu != mu0") {
      
				return(wilcox.test(assign(input$test_var,X()),mu = mu0()))
   
			} else if (input$hyp_check == "H0 : mu <= mu0 vs H1: mu > mu0") {
      
				return(wilcox.test(assign(input$test_var,X()),mu = mu0(), alternative = "greater"))
      
			} else if (input$hyp_check == "H0 : mu >= mu0 vs H1: mu < mu0") {
      
				return(wilcox.test(assign(input$test_var,X()),mu = mu0(), alternative = "less"))
      
			}
		}
		
	#Two sample tests
		
	} else if (input$tests == "Two sample for quantitative data") {
	
		if (input$tw_s_test == "Two sample T-test") {
			
			req(input$hyp_var,input$hyp_check,input$hyp_par)
			
			if (input$hyp_check == "H0 : mu = mu0 vs H1: mu != mu0") {
      
				return(t.test(assign(input$test_var,X()),assign(input$test_var,Y()),mu = mu0()))
   
			} else if (input$hyp_check == "H0 : mu <= mu0 vs H1: mu > mu0") {
      
				return(t.test(assign(input$test_var,X()),assign(input$test_var,Y()),mu = mu0(), alternative = "greater"))
      
			} else if (input$hyp_check == "H0 : mu >= mu0 vs H1: mu < mu0") {
      
				return(t.test(assign(input$test_var,X()),assign(input$test_var,Y()),mu = mu0(), alternative = "less"))
      
			}
		} else if (input$tw_s_test == "Two sample Wilcoxon rank sum test") {
			
			req(input$hyp_var,input$hyp_check,input$hyp_par)
			
			if (input$hyp_check == "H0 : mu = mu0 vs H1: mu != mu0") {
      
				return(wilcox.test(assign(input$test_var,X()),assign(input$test_var,Y()),mu = mu0()))
   
			} else if (input$hyp_check == "H0 : mu <= mu0 vs H1: mu > mu0") {
      
				return(wilcox.test(assign(input$test_var,X()),assign(input$test_var,Y()),mu = mu0(), alternative = "greater"))
      
			} else if (input$hyp_check == "H0 : mu >= mu0 vs H1: mu < mu0") {
      
				return(wilcox.test(assign(input$test_var,X()),assign(input$test_var,Y()),mu = mu0(), alternative = "less"))
      
			}
		
		} else if (input$tw_s_test == "Two sample Kolomgorov - Smirnov test") {
			
			req(input$hyp_var,input$hyp_check,input$hyp_par)
			
			if (input$hyp_check == "H0 : mu = mu0 vs H1: mu != mu0") {
      
				return(wilcox.test(assign(input$test_var,X()),assign(input$test_var,Y())))
   
			} else if (input$hyp_check == "H0 : mu <= mu0 vs H1: mu > mu0") {
      
				return(wilcox.test(assign(input$test_var,X()),assign(input$test_var,Y()), alternative = "greater"))
      
			} else if (input$hyp_check == "H0 : mu >= mu0 vs H1: mu < mu0") {
      
				return(wilcox.test(assign(input$test_var,X()),assign(input$test_var,Y()),mu = mu0(), alternative = "less"))
      
			}
		
		} else if (input$tw_s_test == "Two sample chi square test on equal sample variances") {
			
			req(input$hyp_var,input$hyp_check,input$hyp_par)
			
			if (input$hyp_check == "H0 : mu = mu0 vs H1: mu != mu0") {
      
				return(var.test(assign(input$test_var,X()),assign(input$test_var,Y()),ratio = mu0()))
   
			} else if (input$hyp_check == "H0 : mu <= mu0 vs H1: mu > mu0") {
      
				return(var.test(assign(input$test_var,X()),assign(input$test_var,Y()),ratio = mu0(), alternative = "greater"))
      
			} else if (input$hyp_check == "H0 : mu >= mu0 vs H1: mu < mu0") {
      
				return(var.test(assign(input$test_var,X()),assign(input$test_var,Y()),ratio = mu0(), alternative = "less"))
      
			}
		}
	}
  })
  
})  
