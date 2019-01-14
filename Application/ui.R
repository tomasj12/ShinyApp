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
library(latex2exp)



shinyUI(navbarPage('Shiny application',
                   #tab Panel plot
                   #tu by sa mali nacitat data, a vytvorit nejaky plot, najlepšie buť ak by som chcel 
                   # závislosť, alebo histogramy, teda rozdelenie atd
                   tabPanel('EDA',
                            sidebarLayout(
                              sidebarPanel(fileInput( inputId = 'dataLoad',
                                                      label = 'Choose a file containing data'),
                                           radioButtons(inputId = "separator",
                                                        label = "Choose file type",
                                                        choices = c(Csv = "csv",
                                                                    Tab = "tab",
                                                                    Raw = "raw")),
                                           fileInput(inputId = "descr_load",
                                                     label = "Load description of dataset"),
                                           radioButtons( inputId = 'plotType',
                                                         label   = 'Choose type of graph',
                                                         choices = c('Histogram','ScatterPlot',
                                                                     'Pie','BarPlot',
                                                                     'Boxplot')),
                                           fluidRow(uiOutput("test")),
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
                   tabPanel("Model",
                            sidebarLayout(
                              sidebarPanel(
                                radioButtons(inputId = "type_of_model",
                                             label = "Choose type of model",
                                             choices = c("Linear Regression",
                                                         "Logistic Regression",
                                                         "SVM",
                                                         "Decission Trees",
                                                         "Random Forrest",
                                                         "Naive Bayes",
                                                         "KNN",
                                                         "Neuron Nets")),
                                selectInput(inputId = "pred_var",
                                            label = "Choose variable to predict",
                                            choices = c()),
                                actionButton(inputId = "depend_var",
                                             "Choose features"),
                                br(),
                                br(),
                                uiOutput("apply_model")),
                              mainPanel(verbatimTextOutput("regression"))
                              
                            )),
                   tabPanel("Hypothesis testing",
                            sidebarLayout(
                              sidebarPanel(
                                radioButtons(inputId = "tests",
                                             label = "Choose type of testing",
                                             choices = c("One sample",
                                                         "Two sample",
                                                         "Paired")),
                                conditionalPanel(
                                  condition = "input.tests == 'One sample'",
                                  selectInput(inputId = "one_s_test",
                                              label = "Choose one sample test",
                                              choices = c("One sample T-test",
                                                          "One sample Wilcoxon rank sum test",
                                                          "One sample signed test",
                                                          "One sample Kolomgorov - Smirnov test",
                                                          "One sample chi square test on sample variance"))),
                                conditionalPanel(
                                  condition = "input.tests == 'Paired'",
                                  selectInput(inputId = "pair_s_test",
                                              label = "Choose paired test",
                                              choices = c("Paired T-test",
                                                          "Paired Wilcoxon rank sum test",
                                                          "Paired signed test"))),
                                conditionalPanel(
                                  condition = "input.tests == 'Two sample'",
                                  selectInput(inputId = "tw_s_test",
                                              label = "Choose two sample test",
                                              choices = c("Two sample T-test",
                                                          "Two sample Wilcoxon rank sum test",
                                                          "Two sample Kolomgorov - Smirnov test",
                                                          "Two sample chi square test on equal sample variances"))),
                                radioButtons(inputId = "hyp_type",
                                             label = "Choose hypothesis",
                                             choices = c("H0: mu = c vs H1: mu != c",
                                                         "H0: mu <= vs H1: mu > c",
                                                         "H0: mu >= c vs H1: mu < c"))
                                
                                
                              ),
                              mainPanel(
                                verbatimTextOutput("testOutput")
                              )))
))


