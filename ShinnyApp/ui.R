#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(markdown) 

shinyUI(navbarPage("Word predictor", 
                   tabPanel("Table", 
                            
                            # Sidebar 
                            sidebarLayout( 
                                sidebarPanel( 
                                    helpText("Write down the words that needs to be predicted"), 
                                    helpText("be patient the first time it takes some time to load"), 
                                    textInput("yoursentence", "your sentence here:", value = "replace this with your senence"),
                                    sliderInput("quantity", "maximum predictions:",
                                                min = 0, max = 20, value = 10
                                    ),
                                    submitButton(text = "submit", icon = NULL, width = NULL)),
                                
                                
                                mainPanel(
                                  
                                  h4("Prediction"),
                                  verbatimTextOutput("prediction"),
                                  
                                  br(),
                                  br(),
                                  dataTableOutput("dataprediction")
                                )
                            ))))

