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
library(ShinyItemAnalysis)
library(data.table)
library(moments)



# Define UI for application that draws a histogram
shinyUI(navbarPage('Shiny application',
                   #tab Panel plot
                   #tu by sa mali nacitat data, a vytvorit nejaky plot, najlepšie buť ak by som chcel 
                   # závislosť, alebo histogramy, teda rozdelenie atd
                   tabPanel('EDA',
                        sidebarLayout(
                            sidebarPanel(fileInput( inputId = 'dataLoad',
                                                    label = 'Vyber súbor obsahujúci dáta'),
                                         radioButtons(inputId = "separator",
                                                      label = "Vyber typ súboru",
                                                      choices = c(Csv = "csv",
                                                                  Tab = "tab",
                                                                  Raw = "raw")),
                                         fileInput(inputId = "descr_load",
                                                   label = "Načítaj popis dát"),
                                         radioButtons( inputId = 'plotType',
                                              label   = 'Vyber typ grafu',
                                              choices = c('Histogram','ScatterPlot',
                                                          'Pie','BarPlot',
                                                          'Boxplot')),
                                         uiOutput("test"),
                                         uiOutput("test_one"),
                                        checkboxGroupInput( inputId = 'header',
                                               label = 'Is file containig header?',
                                               choices = 'Yes'),
                                         fluidRow(actionButton( inputId = 'upload',
                                                                label = 'Upload data'),
                                                  actionButton( inputId = "show_descr",
                                                                label = "Show description"))),
                              
                              mainPanel(
                                tabsetPanel(
                                  tabPanel("Description of Data",
                                           plotOutput('plot'),
                                           br(),
                                           h3("Number characteristics"),
                                           tableOutput("characteristics"),
                                           br(),
                                           h3("Other statistics"),
                                           tableOutput("datachar")),
                                  tabPanel("Data",
                                           tableOutput("data")),
                                  tabPanel("Description",
                                           fluidRow(
                                             textOutput("descr")),
                                             br(),
                                           fluidRow(tableOutput("vars"))))
    )
   )
  ),
 tabPanel("Model")
)
)

