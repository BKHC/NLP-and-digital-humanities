options(shiny.maxRequestSize=30*1024^2) 

# Import libraries that are needed for processing in this module.
library(shiny) 
library(dplyr)
library(text2vec)

# For the first time, download the pre-trained glove vector by standord nlp group

if (!file.exists('glove.6B.zip')) {
  download.file('http://nlp.stanford.edu/data/glove.6B.zip',destfile = 'glove.6B.zip')
  unzip('glove.6B.zip')
}

# After that, we can simply load the glove vectors stored locally
vectors = data.table::fread('glove.6B.300d.txt', data.table = F,  encoding = 'UTF-8')
colnames(vectors) = c('word',paste('dim',1:300,sep = '_'))

as_tibble(vectors)

# Define server logic required to summarize and view the result of analysis 
shinyServer(function(input, output) {     
  # display the original text inputted by the user 
  output$Original <- renderText({ 
    OriginalTextInput <- input$obs 
    return(OriginalTextInput) 
  }) 

  # Display the top predictive observations 
  output$view <- renderTable({ 
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