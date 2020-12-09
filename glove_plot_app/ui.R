library(shiny) 
library(shiny.i18n)
library(plotly)

# Define UI for dataset viewer application 
shinyUI(fluidPage( 

    # Application title. 
    titlePanel("*** Word Embedding - Glove ***"), 
    
    mainPanel( 
        
        textInput("word1", "Enter your word:"),  

        h5("You have entered the following word:", style = "color:blue"),
        br(), 

        h4("Run computation"),
        actionButton("goButton", "Compute!"),

        br(), 
        h5("the neareset 50 words:", style = "color:blue"),
        h3(""), plotlyOutput("plot_result",   width = "100%", height = "600px")

    ) 
) 
) 