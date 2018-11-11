#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(shinythemes) #umoznuje vyuzivat nice shiny temy
library(DT)
library(shinyBS)
library(reader)



# Define UI for application that draws a histogram
shinyUI(navbarPage('Shiny application',
                   #tab Panel plot
                   #tu by sa mali nacitat data, a vytvorit nejaky plot, najlepšie buť ak by som chcel 
                   # závislosť, alebo histogramy, teda rozdelenie atd
                   tabPanel('EDA',
                            
                            sidebarLayout(
                              
                              sidebarPanel(
                                
                                fileInput( inputId = 'dataLoad',
                                           label = 'Vyber súbor obsahujúci dáta'),
                                
                                radioButtons( inputId = 'plotType',
                                              label   = 'Vyber typ grafu',
                                              choices = c('Histogram','Scatter Plot',
                                                          'Kolacovy diagram','Bar Plot',
                                                          'Boxplot'))),
                              
                              mainPanel(
                                
                                plotOutput('plot')
                                
                              )
                              
                            )
                   )
)
)