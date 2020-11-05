options(shiny.maxRequestSize=30*1024^2) 

# Import libraries that are needed for processing in this module.
library(shiny) 
library(dplyr)
library(text2vec)
library(data.table)
library(R.utils)

# For the first time, download the pre-trained glove vector by standord nlp group
if (!file.exists('glove.6B.50d.txt')) {
  download.file('https://github.com/kmr0877/IMDB-Sentiment-Classification-CBOW-Model/raw/master/glove.6B.50d.txt.gz',destfile = 'glove.6B.50d.txt.gz', mode='wb', cacheOK=FALSE)
  gunzip('glove.6B.50d.txt.gz')
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

  results <- eventReactive(input$goButton,{
    print("Computing!")
 
    withProgress(message = 'Computing...', value = 0, {
      target <- as.numeric(vectors[vectors[, "word"] == tolower(input$word1),])[-1]- as.numeric(vectors[vectors[, "word"] == tolower(input$word2),])[-1] + as.numeric(vectors[vectors[, "word"] == tolower(input$word3),])[-1]

      cos_sim = sim2(x = as.matrix(subset(vectors, select = -c(word))), y = t(as.matrix(target)), method = "cosine", norm = "l2")

      output_data = as.data.frame(head(sort(cos_sim[,1], decreasing = TRUE), 10))

      setDT(output_data, keep.rownames = TRUE)[]

      colnames(output_data) <- c("words", "cosine similarity")
    })
    
    rm("target", "cos_sim")
    gc()

    return (output_data)
  })

  # Display the top words with highest consine similarity
  output$cos_sim_sort <- renderTable({ 
    if (is.null(results()))
      return (NULL)
  # Displaying the result
  return(results())    
  }) 
}) 