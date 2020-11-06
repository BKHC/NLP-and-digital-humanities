options(shiny.maxRequestSize=30*1024^2) 

# Import libraries that are needed for processing in this module.
library(shiny)
library(data.table) 
library(udpipe)
library(cld3)
library(textplot) # to plot the dependencies

# The first time, you have to download a language model to perform morpho-syntactic analysis
if (!file.exists('chinese-gsdsimp-ud-2.5-191206.udpipe')) {
  udpipe_download_model(language = "english")
  udpipe_download_model(language = "chinese")
  udpipe_download_model(language = "chinese-gsdsimp")
  udpipe_download_model(language = "french")
}


modelList <- list("en" = "english-ewt-ud-2.5-191206.udpipe", "fr" = "french-gsd-ud-2.5-191206.udpipe", "zh-trad" = "chinese-gsd-ud-2.5-191206.udpipe", "zh-sim" = "chinese-gsdsimp-ud-2.5-191206.udpipe")

# Define server logic required to summarize and view the result of analysis 
shinyServer(function(input, output, session) {     

  observeEvent(input$selected_interface_language, {
    update_lang(session, input$selected_interface_language)
  })

  # display the original text inputted by the user 
  output$Original <- renderText({ 
    OriginalTextInput <- input$obs
    return(OriginalTextInput) 
  }) 

  model <- eventReactive(input$goButton,{
    print("Computing!")
 
    withProgress(message = 'Computing...', value = 0, {
      my_language_model <- udpipe_load_model(modelList[[input$selected_model_language]])
      
    })
    return (my_language_model)
  })

  output$analysis_result <- renderTable({ 
    if (is.null(model()))
      return (NULL)

    OriginalTextInput <- input$obs 
    my_output <- udpipe_annotate(model(), x= input$obs, keep_acronyms=TRUE)

    # Let's transform the output, so it is easier to inspect...
    my_output <- as.data.frame(my_output)

    # Dropping some columns to see things more easily
    table_output <- subset(my_output, select = -c(doc_id, paragraph_id, sentence, deps, misc))

  return(table_output)    
  }) 

  output$plot_result <- renderPlot({ 
    if (is.null(model()))
      return (NULL)

    OriginalTextInput <- input$obs 
    my_output <- udpipe_annotate(model(), x= input$obs, keep_acronyms=TRUE)

    # Let's transform the output, so it is easier to inspect...
    my_output <- as.data.frame(my_output)

  return(textplot_dependencyparser(my_output))    
  }) 

}) 