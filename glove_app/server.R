options(shiny.maxRequestSize=30*1024^2) 

# Import libraries that are needed for processing in this module.
library(shiny) 
library(dplyr)
library(text2vec)
library(data.table)

# For the first time, download the pre-trained glove vector by standord nlp group
if (!file.exists('glove.6B.50d.txt')) {
  download.file('https://storage.googleapis.com/kaggle-data-sets/8542/11957/bundle/archive.zip?X-Goog-Algorithm=GOOG4-RSA-SHA256&X-Goog-Credential=gcp-kaggle-com%40kaggle-161607.iam.gserviceaccount.com%2F20201023%2Fauto%2Fstorage%2Fgoog4_request&X-Goog-Date=20201023T061031Z&X-Goog-Expires=259199&X-Goog-SignedHeaders=host&X-Goog-Signature=1001193cf5f9c51b2bb3c129a6938843cc4da0776e443742eae99eed0b92a803b3292c3f29108655ac3115d30c10b70df91c48747f34e3e7cd48cbd1112c15e053dc62b1aa2bfcdb05bf85e5586b9613159a60a920a938f3d0d74a0de7fc6dcc4eaf2f1779cbd51067e4556b58cbf97b66a31daf49ba8c825e87d60824220fe303f5d26f288d2a5de8eff374cff2d3974c602c1ec98c33f77a734b55a70fe7c6dbce010127da60484b397539983b1f48b55b4da62a01b9ad65a8ffc642ca95cf5fc912537e2f4ecc6487f48cbfe730807464b1b4580e4b9110c7f2df72dc05024ab4a347c96c14cf74b5f4a2e76d56f931c867ffa7b3678367f83e372396da0d',destfile = 'archive.zip', mode='wb', cacheOK=FALSE)
  unzip('archive.zip')
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
      target <- as.numeric(vectors[vectors[, "word"] == input$word1,])[-1]- as.numeric(vectors[vectors[, "word"] == input$word2,])[-1] + as.numeric(vectors[vectors[, "word"] == input$word3,])[-1]

      cos_sim = sim2(x = as.matrix(subset(vectors, select = -c(word))), y = t(as.matrix(target)), method = "cosine", norm = "l2")

      output_data = as.data.frame(head(sort(cos_sim[,1], decreasing = TRUE), 10))

      setDT(output_data, keep.rownames = TRUE)[]

      colnames(output_data) <- c("words", "cosine similarity")
    })
    
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