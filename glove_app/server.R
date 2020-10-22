options(shiny.maxRequestSize=30*1024^2) 

# Import libraries that are needed for processing in this module.
library(shiny) 
library(dplyr)
library(text2vec)
library(data.table)

# For the first time, download the pre-trained glove vector by standord nlp group
if (!file.exists('glove.6B.50d.txt')) {
  download.file('https://www.kaggle.com/watts2/glove6b50dtxt/download',destfile = 'glove.6B.zip')
  unzip('archive.6B.zip')
}


# After that, we can simply load the glove vectors stored locally
vectors = data.table::fread('glove.6B.50d.txt', data.table = F,  encoding = 'UTF-8')

colnames(vectors) = c('word',paste('dim',1:50,sep = '_'))

as_tibble(vectors)

rownames(vectors) <- vectors$word

# Define server logic required to summarize and view the result of word embeddings  
shinyServer(function(input, output, session) {     
  # display the original text inputted by the user 
  output$Original <- renderText({ 
    combineFormula <- paste(input$word1, "-", input$word2, "+", input$word3, sep=" ") 
    return(combineFormula) 
  }) 

  # Display the top words with highest consine similarity
  output$cos_sim_sort <- renderTable({ 
  target <- as.numeric(vectors[vectors[, "word"] == input$word1,])[-1]- as.numeric(vectors[vectors[, "word"] == input$word2,])[-1] + as.numeric(vectors[vectors[, "word"] == input$word3,])[-1]

  cos_sim = sim2(x = as.matrix(subset(vectors, select = -c(word))), y = t(as.matrix(target)), method = "cosine", norm = "l2")

  output_data = as.data.frame(head(sort(cos_sim[,1], decreasing = TRUE), 10))

  setDT(output_data, keep.rownames = TRUE)[]

  colnames(output_data) <- c("words", "cosine similarity")

  # Displaying the result
  return(output_data)    
  }) 
}) 