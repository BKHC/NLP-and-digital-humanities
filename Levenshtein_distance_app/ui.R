#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  # Application title. 
  titlePanel("*** Levenshtein Distance ***"), 
  wellPanel(
  fluidRow(column(2,textInput("word1", "Please enter first word:", "apricot")),  
           column(2,textInput("word2", "Please enter second word:", "alphabet"))
  ),submitButton("Start to calculate Levenshtein Distance")
  ),
  mainPanel(width = 12,
    h5("You have entered the following words:", style = "color:blue"),
    textOutput("word1"), 
    textOutput("word2"), 
    h3("The Results:", style = "color:green"), 
    h5("Levenshtein distance:"),
    textOutput("distance"),
    h5("Matrix:"),
    verbatimTextOutput("m"),
    h5("Transformation:"), 
    wellPanel(verbatimTextOutput("transformation"))
    #tableOutput("view") 
  ) 
))
