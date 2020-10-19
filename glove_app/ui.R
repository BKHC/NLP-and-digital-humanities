library(shiny) 

# Define UI for dataset viewer application 
shinyUI(fluidPage( 

    # Application title. 
    titlePanel("*** Morphosyntax Analysis ***"), 
    
    mainPanel( 
        
        textInput("obs", "Please enter your statement:"),  
        helpText("The Shiny App will perform Morphosyntax Analysis on the entered statement and show you the results.", style = "color:blue"),
        helpText("After you enter your statement, please press 'Start the Morphosyntax Analysis Process' below:", style = "color:blue"),
        submitButton("Start the Morphosyntax Analysis Process"),
            
        h5("You have entered the following statement:", style = "color:blue"),
        br(), 
        textOutput("Original"), 
        br(), 
        h3("The output of Morphosyntax Analysis:", style = "color:green"), 
        tableOutput("view") 
    ) 
) 
) 