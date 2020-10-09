#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  radioButtons(inputId = "language", label = "",
               choices = c("English" = "en", "Français" = "fr", "中文" = "ch"),
               selected = "en"),
  # Application title
  # Application title. 
  
  titlePanel("*** Levenshtein Distance ***"), 
  wellPanel(
  fluidRow(column(2, uiOutput("uiWord1")),  
           column(2, uiOutput("uiWord2"))
  )
  
  ),
  mainPanel(width = 12,
    h5(textOutput("inputlabel"), style = "color:blue"),
    textOutput("word1"), 
    textOutput("word2"), 
    h3(textOutput("resultlabel"), style = "color:green"), 
    h5("Levenshtein distance:"),
    textOutput("distance"),
    h5(textOutput("matrixlabel")),
    verbatimTextOutput("m"),
    h5(textOutput("transformationlabel")), 
    wellPanel(verbatimTextOutput("transformation"))
    #tableOutput("view") 
  ) 
))
