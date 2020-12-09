options(shiny.maxRequestSize=30*1024^2) 

# Import libraries that are needed for processing in this module.
library(shiny)
library(dplyr)
library(data.table)
library(R.utils)
library(Rtsne)
library(plotly)

set.seed(42)

normalize = function(m, norm = c("l1", "l2", "none")) {
  stopifnot(inherits(m, "matrix") || inherits(m, "sparseMatrix"))
  norm = match.arg(norm)

  if (norm == "none")
    return(m)

  norm_vec = switch(norm,
                    l1 = 1 / rowSums(m),
                    l2 = 1 / sqrt(rowSums(m ^ 2))
  )
  # case when sum row elements == 0
  norm_vec[is.infinite(norm_vec)] = 0

  if(inherits(m, "sparseMatrix"))
    Diagonal(x = norm_vec) %*% m
  else
    m * norm_vec
}

sim2 = function(x, y = NULL, method = c("cosine", "jaccard"),
                norm = c("l2", "none")) {
  norm = match.arg(norm)
  method = match.arg(method)
  # check first matrix
  stopifnot(inherits(x, "matrix") || inherits(x, "Matrix"))

  FLAG_TWO_MATRICES_INPUT = FALSE
  if (!is.null(y)) {
    FLAG_TWO_MATRICES_INPUT = TRUE
  }
  # check second matrix
  if (FLAG_TWO_MATRICES_INPUT) {
    stopifnot(inherits(y, "matrix") || inherits(y, "Matrix"))
    stopifnot(ncol(x) == ncol(y))
    stopifnot(colnames(x) == colnames(y))
  }

  RESULT = NULL

  if (method == "cosine") {
    x = normalize(x, norm)
    if (FLAG_TWO_MATRICES_INPUT) {
      y = normalize(y, norm)
      RESULT = tcrossprod(x, y)
    }
    else
      RESULT = tcrossprod(x)
  }

  if (method == "jaccard") {
    if (!inherits(x, "sparseMatrix"))
      stop("at the moment jaccard distance defined only for sparse matrices")

    if (norm != "none") {
      msg = paste(norm, "norm provided. Howewer matrix will be converted to binary (0,1) automatically.")
      msg = paste(msg, "'jaccard' can be computed only on sets which should be encoded as sparse matrices of 0, 1.")
      logger$warn(msg)
    }
    x@x = sign(x@x)
    if (FLAG_TWO_MATRICES_INPUT) {
      y@x = sign(y@x)
    }
    RESULT = jaccard_sim(x, y)
  }
  RESULT
}

vectors = data.table::fread('glove.6B.50d.txt', data.table = F,  encoding = 'UTF-8')

rownames(vectors) <- vectors$V1

# Define server logic required to summarize and view the result of word embeddings  
shinyServer(function(input, output, session) {     
# display the original text inputted by the user 

  results <- eventReactive(input$goButton,{
    print("Computing!")
 
    withProgress(message = 'Computing...', value = 0, {
      target <- as.numeric(vectors[vectors[, "V1"] == tolower(input$word1),])[-1]

      cos_sim = sim2(x = as.matrix(subset(vectors, select = -c(V1))), y = t(as.matrix(target)), method = "cosine", norm = "l2")

      output_data = as.data.frame(head(sort(cos_sim[,1], decreasing = TRUE), 50))

      rm("cos_sim")
      gc()

      embedding_matrix <- vectors %>% filter(V1 %in% row.names(output_data))

      rm("output_data")
      gc()

      tsne <- Rtsne(embedding_matrix, perplexity = 5, pca = FALSE)
      
      target_index <- which(rownames(embedding_matrix)==input$word1)

      columns_name <- row.names(embedding_matrix)

      rm("embedding_matrix")
      gc()

      results <- plot_ly(width=1250, height=750) %>%
      add_trace(results, mode="text", name="tsne glove", type="scatter", x=tsne$Y[,1], y=tsne$Y[,2], text=columns_name, hoverinfo="text") %>%
      add_trace(results, mode="markers", marker=list(symbol="star-open", size=30), name="text you have entered", type="scatter", x=tsne$Y[target_index, 1], y=tsne$Y[target_index, 2], text="text you have entered", hoverinfo="text")

      #tsne_plot <- layout(tsne_plot, ) #, title=layout$title, xaxis=layout$xaxis, yaxis=layout$yaxis

      rm("target", "tsne")
      gc()

    })

    return (results)
  })

  # Display the top words with highest consine similarity
  output$plot_result <- renderPlotly({ 
    if (is.null(results()))
      return (NULL)
  # Displaying the result
  return(results())    
  }) 

}) 