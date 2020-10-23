library(shiny) 

# Define UI for dataset viewer application 
shinyUI(fluidPage( 

    # Application title. 
    titlePanel("*** Word Embedding - Glove ***"), 
    
    mainPanel( 
        
        textInput("word1", "Enter your first word:"),  
        helpText("minus", style = "color:blue"),

        textInput("word2", "Enter your second word:"),  
        helpText("plus", style = "color:blue"),
        
        textInput("word3", "Enter your third word:"),  

        h5("You have entered the following formula:", style = "color:blue"),
        br(), 
        textOutput("Original"), 
        br(), 

        h4("Run computation"),
        actionButton("goButton", "Compute!"),

        br(), 
        h3("The top10 similar vectors after calculating the cosine similarity :", style = "color:green"), 
        tableOutput("cos_sim_sort") 
    ) 
) 
) 