options(shiny.maxRequestSize=30*1024^2) 

# to do - try to find udpipe model for simplified chinese

# Import libraries that are needed for processing in this module.
library(shiny)
library(data.table) 
library(udpipe)

# The first time, you have to download a language model to perform morpho-syntactic analysis
#udpipe_download_model(language = "english")
#udpipe_download_model(language = "chinese")
#udpipe_download_model(language = "french")

modelList <- list("en" = "english-ewt-ud-2.4-190531.udpipe", "fr" = "french-gsd-ud-2.4-190531.udpipe", "cn" = "chinese-gsd-ud-2.4-190531.udpipe")

# Define server logic required to summarize and view the result of analysis 
shinyServer(function(input, output, session) {     

  observeEvent(input$selected_language, {
    update_lang(session, input$selected_language)
  })

  # display the original text inputted by the user 
  output$Original <- renderText({ 
    OriginalTextInput <- input$obs
    return(OriginalTextInput) 
  }) 

  # Display the top predictive observations 
  output$view <- renderTable({ 

  # change model accroding to selected_language
  my_language_model <- udpipe_load_model(modelList[[input$selected_language]])
  
  OriginalTextInput <- input$obs 
  my_output <- udpipe_annotate(my_language_model, x= OriginalTextInput, keep_acronyms=TRUE)

  # Let's transform the output, so it is easier to inspect...
  my_output <- as.data.frame(my_output)

  # Dropping some columns to see things more easily
  my_simplified_output <- subset(my_output, select = -c(doc_id, paragraph_id, sentence, deps, misc))

  # Displaying the result
  return(my_simplified_output)    
  }) 
}) 