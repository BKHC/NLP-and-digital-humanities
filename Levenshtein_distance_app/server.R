#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

load("translation.bin") # contains the dictionary, parsed as a double list

library(shiny)
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  tr <- function(text){ # translates text into current language
    sapply(text,function(s) translation[[s]][[input$language]], USE.NAMES=FALSE)
  }
  # display the original text inputted by the user 
  output$uiWord1 <- renderUI({
    textInput(inputId   = "word1",
                label     = tr("Please enter first word:"),
              value = "apricot")
  })
  output$uiWord2 <- renderUI({
    textInput(inputId   = "word2",
              label     = tr("Please enter second word:"),
              value = "alphabet")
  })
  
  output$inputlabel <- renderText({ 
    paste(tr("You have entered the following words:"))
  }) 
  
  output$transformationlabel <- renderText({ 
    paste(tr("Transformation:"))
  }) 
  
  output$matrixlabel <- renderText({ 
    paste(tr("Matrix:"))
  }) 
  
  output$resultlabel <- renderText({ 
    paste(tr("The Results:"))
  }) 
  
  output$word1 <- renderText({ 
    return(input$word1)
  }) 
  output$word2 <- renderText({ 
    return(input$word2)
  }) 
  
  m <- function(){
    length1 <- nchar(input$word1)
    length2 <- nchar(input$word2)
    
    # Define a matrix of the right dimensions, filled with 0s
    m <- matrix(data = 0, nrow=length2+1, ncol=length1+1)
    
    # Give names to the rows and columns, using the characters of the two words
    colnames(m) <- c("", strsplit(input$word1,"")[[1]]) # Word 1 for columns
    rownames(m) <- c("", strsplit(input$word2,"")[[1]]) # Word 2 for rows
    
    # Fill the first column with numbers from 0 to how many characters there are in Word 2
    for (i in 0:length2)
      m[i+1,1] <- i 
    
    # Fill the first line with numbers from 0 to how many characters there are in Word 1
    for (i in 0:length1)
      m[1,i+1] <- i 
    
    # Fill the matrix with a dynamic programming approach
    for (i in 2:(length2+1)) # We move from one row to the next, frop top to bottom
      for (j in 2:(length1+1)) { # Fo each row, we move from one column to the next, from left to right 
        diag <- m[i-1,j-1] # Value in the left & upper cell
        if (colnames(m)[j] != rownames(m)[i]) # We check if the character in position (j-1) for Word 1 equals the character in position (i-1) for Word 2 (remember there are an additional initial column and an additional initial line in the matrix) 
          diag <- diag + 1 # If yes, we add 1 to the value (which means one character is replaced by another)
        
        m[i,j] <- min(m[i-1,j]+1, m[i, j-1]+1, diag) # We choose the minimum value among diag, the value of the cell on the left, and the value of the cell above
      }
    return(m)
  }
  
  convert_directions_to_transformations <- function(path, m) {
    transformations <- NULL
    print(path) # We print the sequence of directions taken in the matrix
    
    i <- 1
    j <- 1
    for (k in 1:length(path)) { # We are going to analyze the various transformations
      if (path[k] == "diag") {
        if (m[i, j] != m[i+1,j+1])
          transformations <- c(transformations, paste0(colnames(m)[j+1], " -> ", rownames(m)[i+1]))
        else
          transformations <- c(transformations, paste0("keep ", colnames(m)[j+1]))
        i <- i+1
        j <- j+1
      }
      if (path[k] == "right") {
        transformations <- c(transformations, paste0("delete ", colnames(m)[j+1]))
        j <- j+1
      }
      if (path[k]== "down") {
        transformations <- c(transformations, paste0("insert ", rownames(m)[i+1]))
        i <- i+1
      }
    }
    
    print(transformations)
    print("")
  }
  
  transformation_path <- function(i, j, m, path) {
    
    # This is what will be done when reaching the top left cell
    if (i == 1 && j == 1) {
      convert_directions_to_transformations(path, m) # We call another function, which will derive the transformations from the path in the matrix
      return (1);
    }
    
    # This is what will be done when reaching the first row (but without being in the left cell, since this has been covered in the previous test)
    if (i == 1) { # Here, we can only move to the left cell, since there are no more cells above
      path <- c("right", path)
      transformation_path(i, j-1, m, path)
      return (1);
    }
    
    # This is what will be done when reaching the first column (but without being in the top cell, since this has been covered in the first test)
    if (j == 1) { # Here, we can only move to the above cell, since there are no more cells on the left
      path <- path("down", path)
      #path <- path("down", path)
      transformation_path(i-1, j, m, path)
      return (1);
    } 
    
    # What follows next is what happens when we are in the matrix, but not in the first row or in the first column
    min_value <- min(m[i-1, j], m[i, j-1], m[i-1, j-1])
    
    if (m[i-1, j-1] == min_value) {
      path <- c("diag", path)
      transformation_path(i-1, j-1, m, path)
      path <- path[2:length(path)]
    }
    
    if (m[i-1, j] == min_value) {
      path <- c("down", path)
      transformation_path(i-1, j, m, path)
      path <- path[2:length(path)]
    }
    
    if (m[i, j-1] == min_value) {
      path <- c("right", path)
      transformation_path(i, j-1, m, path)
      path <- path[2:length(path)]
    }
  }
  
  output$m <- renderPrint({
    print(m())
  })
  output$distance <- renderText({
    length1 <- nchar(input$word1)
    length2 <- nchar(input$word2)
    return(m()[length2+1, length1+1])
  })
  
  output$transformation <- renderPrint({
    # One may be willing to extract all possible transformative paths to go from Word 1 to Word 2
    # This is best achieved with something called a recursive function. This is quite common in programming, but still not that easy to grasp
    # A recursive function is a function which calls itself. It's a way of exploring all paths with getting lost if there are many of them
    m <- m() 
    return(transformation_path(nrow(m), ncol(m), m, NULL))
  })
})
