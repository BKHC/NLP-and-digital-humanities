#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
options(warn = -1)

library(googledrive)

library(plyr)
library(data.table)
library(tidyverse)
library(tidytext)
library(quanteda)
library(syuzhet)
library(textrank)
library(topicmodels)
library(ldatuning)
library(ggplot2)
library(wordcloud2)
library(igraph)
library(ggraph)
library(stm)
library(ggforce)
library(textdata)
library(udpipe)


# **************************************
# Defining some functions for later ####
# **************************************

my.sentiments <- NULL 
# Two useful functions to draw figures
reorder.within <-
  function(x,
           by,
           within,
           fun = mean,
           sep = "___",
           ...) {
    new.x <- paste(x, within, sep = sep)
    stats::reorder(new.x, by, FUN = fun)
  }

scale.x.reordered <- function(..., sep = "___") {
  reg <- paste0(sep, ".+$")
  ggplot2::scale_x_discrete(
    labels = function(x)
      gsub(reg, "", x),
    ...
  )
}

# A function to select a subset of documents according to several criteria:
# - a choice of categories at a first level of classification (by default all categories are selected)
# - a choice of categories at a second level of classification (by default all categories are selected)
# - a set of syntactic categories (by defaut all categories are selected)
# - a set of words/tokens to exclude (by default no token is excluded)

select.subset <-
  function(content,
           categories.1 = "all",
           categories.2 = "all",
           upos.categories = NULL,
           stop.words = NULL) {
    if (categories.1 != "all" &&
        any(!categories.1 %in% unique(content %>% pull(class_1))))
      stop(
        "Beware! At least one of your target categories at the first level of classification does not appear in the actual list of occuring categories"
      )
    
    if (categories.2 != "all" &&
        any(!categories.2 %in% unique(content %>% pull(class_2))))
      stop(
        "Beware! At least one of your target categories at the second level of classification does not appear in the actual list of occuring categories"
      )
    
    selected.content <- content
    
    if (!is.null(upos.categories))
      selected.content <-
      selected.content %>% filter(upos %in% upos.categories)
    if (!is.null(stop.words))
      selected.content <-
      selected.content %>% filter(!lemma %in% stop.words)
    
    if (length(categories.1) != 1 || categories.1 != "all")
      selected.content <-
      selected.content %>% filter(class_1 %in% categories.1)
    
    if (length(categories.2) != 1 || categories.2 != "all")
      selected.content <-
      selected.content %>% filter(class_2 %in% categories.2)
    
    #
    #  if (! is.null(upos.categories))
    #    selected.content <- subset(selected.content, upos %in% upos.categories)
    #  if (! is.null(stop.words))
    #    selected.content <- subset(selected.content, ! lemma %in% stop.words)
    
    #  if (length(categories.1) != 1 || categories.1 != "all")
    #    selected.content <- selected.content[selected.content[, "class_1"] %in% categories.1, ]
    
    #  if (length(categories.2) != 1 || categories.2 != "all")
    #    selected.content <- selected.content[selected.content[, "class_2"] %in% categories.2, ]
    
    if (nrow(selected.content) == 0)
      print("Beware! Empty table, no rows were selected. There is a problem with your categories...")
    
    selected.content <- droplevels(selected.content)
    
    return (selected.content)
  }

# A function to extract the locations of the limits of the categories among the sentences of a set of documents
# It is used to display the evolution of sentiments or the dispersion of terms below
extract.positions.categories <-
  function(sentences, results.classification) {
    my.classif <- sentences %>% pull(results.classification)
    category.limits <- which(my.classif != lead(my.classif, 1)) + 1
    category.labels <-
      as.character(my.classif[category.limits - 1])
    category.limits <- c(category.limits, length(my.classif))
    category.labels <-
      c(category.labels, as.character(my.classif[length(my.classif)]))
    
    my.limits <-
      data.frame(
        index = category.limits,
        present = rep(0, length(category.limits)),
        labels = category.labels,
        stringsAsFactors = F
      )
    
    return(my.limits)
  }

# Define server logic 
shinyServer(function(input, output, session) {
  observeEvent(input$selected_language, {
    update_lang(session, input$selected_language)
  })
  
  content <- eventReactive(input$my.choice, {
    print("Computing!")
    withProgress(message = 'Loading Corpus...', value = 0, {
      print("hi")
      #current <- dirname(rstudioapi::getActiveDocumentContext()$path)
      #setwd(dir = paste0(current, "/parsed_data")) # Setting the working directory
      
      content <-
        readRDS(paste0(paste0("./parsed_data/", input$my.choice), ".rds")) # Loading the data
      output$content <- renderTable({
        head(content, n = 20)
      })
      
      output$class_1 <- renderPrint({
        summary(content$class_1)
      })
      output$class_2 <- renderPrint({
        summary(content$class_2)
      })
      
      #input for third tab (categories and terms)
      output$categores_1_choices = renderUI({
        selectInput(
          'categories.1',
          'Categories 1',
          unique(content$class_1),
          multiple = TRUE,
          selected = unique(content$class_1)
        )
      })
      
      #Output for tenth tab (Dispersion of terms)
      output$categores_1_choices_dis = renderUI({
        selectInput(
          'categories.1_dis',
          'Categories 1',
          unique(content$class_1),
          multiple = TRUE,
          selected = unique(content$class_1)
        )
      })
      
      output$categores_2_choices_dis = renderUI({
        selectInput(
          'categories.2_dis',
          'Categories 2',
          unique(content$class_2),
          multiple = TRUE,
          selected = unique(content$class_2)
        )
      })
      
      #Output for eleventh tab (Topic modelling) 
      output$categores_1_choices_tm = renderUI({
        selectInput(
          'categories.1_dis',
          'Categories 1',
          unique(content$class_1),
          multiple = TRUE,
          selected = unique(content$class_1)
        )
      })
      output$categores_2_choices_tm = renderUI({
        selectInput(
          'categories.2_dis',
          'Categories 2',
          unique(content$class_2),
          multiple = TRUE,
          selected = unique(content$class_2)
        )
      })
      
      #Tab three outputs 
      output$analysis3 <- renderPrint({
        categories.2 <- "all"
        selected.content <-
          select.subset(content(), input$categories.1, categories.2,input$upos.category, input$my.stop.words) # Selecting a subset of data
        print(selected.content %>% filter(upos == "X") %>% pull(token))
      })
      
      output$analysis4 <- renderPrint(({
        categories.2 <- "all"
        selected.content <-
          select.subset(content(), input$categories.1, categories.2,input$upos.category, input$my.stop.words) # Selecting a subset of data
        
        print(selected.content %>% filter(upos == "X") %>% pull(sentence) %>% unique())
      }))
      
      #pospart
      #Tab four input (Part-Of-Speech Distribution)
      output$categores_1_choices_pos = renderUI({
        selectInput(
          'categories.1.pos',
          'Categories 1',
          unique(content$class_1),
          multiple = TRUE,
          selected = unique(content$class_1)
        )
      })
      output$categores_1_choices_richness = renderUI({
        selectInput(
          'categories.1.richness',
          'Categories 1',
          unique(content$class_1),
          multiple = TRUE,
          selected = unique(content$class_1)
        )
      })
      
      #Tab four output (Part-Of-Speech Distribution)
      output$pos1 <- renderPlot({
        categories.2 <- "all"
        selected.content <-
          select.subset(content, input$categories.1.pos, categories.2) # Selecting a subset of data
        
        stats.upos <-
          txt_freq(selected.content$upos) # This extracts the frequencies of the different upos category
        # stats.upos # seeing the results as a table...
        max.pct <-
          max(stats.upos$freq_pct) + 15 # This is to prepare a nice figure
        return(
          ggplot(stats.upos, aes(
            x = reorder(key, freq_pct), y = freq_pct
          )) +
            geom_bar(
              stat = "identity",
              fill = 'white',
              color = "grey50"
            ) +
            coord_flip(ylim = c(0, max.pct)) +
            scale_y_continuous(labels = scales::number_format()) +
            labs(x = "Grammatical categories", y = "Frequency of occurrence (in % of the number of tokens)") +
            geom_text(
              aes(label = round(freq_pct, 2)),
              hjust = -0.5,
              size = 8
            ) +
            theme_grey(base_size = 18)
        )
      })
      
      output$pos2 <- renderPlot({
        categories.2 <- "all"
        selected.content <-
          select.subset(content, input$categories.1.pos, categories.2) # Selecting a subset of data
        stats.upos <-
          txt_freq(selected.content$upos) # This extracts the frequencies of the different upos category
        # Graphic focusing on open words versus closed words
        open.words.pct <-
          sum(stats.upos[stats.upos$key %in% c("VERB", "NOUN", "PROPN", "ADJ", "ADV", "INTERJ"), "freq_pct"])
        closed.words.pct <-
          sum(stats.upos[stats.upos$key %in% c("ADP",
                                               "DET",
                                               "PART",
                                               "CCONJ",
                                               "PRON",
                                               "AUX",
                                               "SCONJ",
                                               "NUM"), "freq_pct"])
        other.pct <-
          sum(stats.upos[stats.upos$key %in% c("SYM", "PUNCT", "X"), "freq_pct"])
        
        output$closed.words.pct <-
          renderText({
            paste("Closed Words:", closed.words.pct, "%")
          })
        output$open.words.pct <-
          renderText({
            paste("Open Words:", open.words.pct, "%")
          })
        output$other.pct <-
          renderText({
            paste("Other Words:", other.pct, "%")
          })
        
        df <-
          tibble(
            key = c(
              "open class words",
              "closed class words",
              "other (PUNCT, ...)"
            ),
            freq_pct = c(open.words.pct, closed.words.pct, other.pct)
          )
        return(
        df %>% mutate(Corpus = "") %>%
            ggplot(aes(
              x = Corpus,
              y = freq_pct,
              fill = key
            )) +
              geom_bar(stat = "identity", color = "grey50") +
              labs(x = "", y = "Frequency of occurrence") +
              theme_grey(base_size = 18)
          )
      })
      
      #Fifth tab output (Measures of lexical richness) 
      output$richness <- renderPlot({
        print("button3")
        categories.2 <- "all"
        
        selected.content <- select.subset(content, input$categories.1.richness, categories.2, input$upos.category_richness) # Selecting a subset of data

        # We need to prepare a document-term matrix to feed it to the function we are interested in:
        content.tmp <- document_term_frequencies(selected.content %>% select(input$results.classification, input$target.term_richness))
        content.tmp$term <- factor(content.tmp$term)
        content.tmp$doc_id <- factor(content.tmp$doc_id)
        dtm <- document_term_matrix(x = content.tmp)
        dfm <- as.dfm(dtm)
        
        # The following function, textstat_lexdiv, returns the type to token ratio (TTR)
        # This is the best-known measure of lexical diversity, but there are many others that we leave aside
        # We need to provide the previous document-term matrix to the function:
        my.diversity.measures <- textstat_lexdiv(dfm, measure="TTR") 
        colnames(my.diversity.measures)[1] <- "category"
        my.diversity.measures <- melt(my.diversity.measures)
          return(ggplot(data=my.diversity.measures, aes(x=category, y=value)) +
                   geom_bar(stat="identity", position=position_dodge()) + facet_wrap(~ variable, scales = "free") +
                   coord_flip() + theme_grey(base_size = 14) + ggtitle(paste0("TTR for ", paste(input$upos.category_richness, collapse=", "))))
      })
      
      #Sixth tab output (Frequency of occurrence)  
      output$categores_1_choices_frequency = renderUI({
        selectInput(
          'categories.1.frequency',
          'Categories 1',
          unique(content$class_1),
          multiple = TRUE,
          selected = unique(content$class_1)
        )
      })
      
      output$frequency1 <- renderPlot({
        
        categories.2 <- "all"
        selected.content <- select.subset(content, input$categories.1.frequency, categories.2, input$upos.category_frequency, input$my.stop.words_frequency) # Selecting a subset of data
        # Let's find the most frequent lemmas in the subset of documents:
        nb.lemmas <- txt_freq(selected.content$lemma, order=T)
        nb.lemmas$key <- factor(nb.lemmas$key, levels = rev(nb.lemmas$key))
          return(hist(nb.lemmas$freq, breaks=100))
      })
      
      
      output$frequency2 <- renderPlot({
        categories.2 <- "all"
        selected.content <- select.subset(content, input$categories.1.frequency, categories.2, input$upos.category_frequency, input$my.stop.words_frequency) # Selecting a subset of data
        # Let's find the most frequent lemmas in the subset of documents:
        nb.lemmas <- txt_freq(selected.content$lemma, order=T)
        nb.lemmas$key <- factor(nb.lemmas$key, levels = rev(nb.lemmas$key))
          return(ggplot(nb.lemmas[1:input$nb.words.barplot,], aes(x=reorder(key, freq), y=freq)) +
                   geom_bar(stat="identity", fill='white', color="grey50") +
                   coord_flip(ylim = c(0, max(nb.lemmas$freq)*1.2)) +
                   labs(x="Most frequent lemmas", y="Frequency") +
                   geom_text(aes(label=round(freq,2)), size=3, hjust=-0.5) + theme_grey(base_size = 12)
          )
        })
      
      
      #Seventh tab output (Collocations)
      output$categores_1_choices_collocation = renderUI({
      selectInput(
        'categores_1_choices.collocation',
        'Categories 1',
        unique(content$class_1),
        multiple = TRUE,
        selected = unique(content$class_1)
      )
    })
      
      output$collocations1 <- renderPlot({
        categories.2 <- "all"
        selected.content <- select.subset(content, input$categores_1_choices.collocation, categories.2, input$upos.category_collocation, input$my.stop.words_collocation) # Selecting a subset of data
        
        # We first call the function keywords_collocation which extract collocations 
        my.collocations <- keywords_collocation(selected.content, term = "lemma", group = "class_1", ngram_max = input$nb.gram)
        my.collocations <- my.collocations[order(-my.collocations$lfmd),] # we sort the collocations in decreasing order according to LFMD
        my.collocations$keyword <- factor(my.collocations$keyword, levels = rev(my.collocations$keyword)) # We create a factor
        
        return(# A nicer barplot to display the most meaningful collocations
        ggplot(my.collocations[1:input$nb.collocations.displayed,], aes(x=keyword, y=lfmd)) +
          geom_bar(stat="identity", fill='white', color="grey50") +
          coord_flip(ylim = c(min(my.collocations[1:input$nb.collocations.displayed,"lfmd"]), 0)) +
          labs(x=paste0(input$nb.collocations.displayed, " most relevant combinations"), y="Log-Frequency biased MD (LFMD)") +
          geom_text(aes(label=round(lfmd,2)), size=3, hjust=-0.5) + theme_grey(base_size = 12)
      )
      })
      
      output$collocations2 <- renderPlot({
        categories.2 <- "all"
        selected.content <- select.subset(content, input$categores_1_choices.collocation, categories.2, input$upos.category_collocation, input$my.stop.words_collocation) # Selecting a subset of data
        my.collocations <- keywords_collocation(selected.content, term = "lemma", group = "class_1", ngram_max = input$nb.gram)
        my.collocations <- my.collocations[order(-my.collocations$lfmd),] # we sort the collocations in decreasing order according to LFMD
        my.collocations$keyword <- factor(my.collocations$keyword, levels = rev(my.collocations$keyword)) # We create a factor
        # We can also prepare a network graph with the strongest collocations
        tmp <- my.collocations[,c("left", "right", "lfmd")] # We transform the format of the data to feed them to the function creating the components of the graph
        colnames(tmp) <- c("term1", "term2", "lfmd")
        collocations.network <- graph_from_data_frame(head(tmp, input$nb.collocations.displayed.network)) # Builds the graph
        return(# Plotting the network:
          ggraph(collocations.network, layout = "nicely") +
            geom_edge_link(aes(edge_alpha = lfmd), edge_colour = "red", linejoin = "round",
                           check_overlap = TRUE, lineend="round", width=1.5) +
            geom_node_text(aes(label = name), col = "gray20", size=4) +
            labs(x="", y="") +  theme_void() + theme(legend.position = "none") +
            labs(title = "Collocations within document(s) (according to lfmd)")
        )
      })
      
      #Eighth tab output (Co-occurrence)
      output$categores_1_choices_cooccurrence = renderUI({
        selectInput(
          'categores_1_choices.cooccurence',
          'Categories 1',
          unique(content$class_1),
          multiple = TRUE,
          selected = unique(content$class_1)
        )
      })
      
      output$cooccurrence1 <- renderPlot({
        categories.2 ="all"
        selected.content <- select.subset(content, input$categores_1_choices.cooccurence, categories.2, input$upos.categories_cooccurence, input$my.stop.words_cooccurence) # Selecting a subset of data
        # Calling the main function to extract the co-occurrences.
        my.cooccurrences <- cooccurrence(x = selected.content$lemma, group = "sentence_id", order= TRUE, skipgram = input$nb.gram_cooccurence)
        
        # Simple text display of the 30 most frequent cooccurrences
        head(my.cooccurrences, n=30)
        
        # We can further prepare a network graph with the most frequent cooccurrences
        
        cooccurrences.network <-
          graph_from_data_frame(head(my.cooccurrences, input$nb.cooccurrences.displayed.network)) # First preparing the components of the network
        print(cooccurrences.network)
        g <- ggraph(cooccurrences.network, layout = "nicely") +
          geom_edge_link(
            aes(edge_alpha = cooc),
            edge_colour = "red",
            linejoin = "round",
            check_overlap = TRUE,
            lineend = "round",
            width = 1.5
          ) +
          geom_node_text(aes(label = name), col = "gray20", size = 4) +
          labs(x = "", y = "") +  theme_void() + theme(legend.position = "none") +
          labs(title = "Cooccurrences within document(s)")
        # Displaying the network:
        return(g)
      })
      
      #Nineth tab output (Sentiment Analysis)
      output$categores_1_choices_sentiment <- renderUI({
        selectInput(
          'categores_1_choices.sentiment',
          'Categories 1',
          unique(content$class_1),
          multiple = TRUE,
          selected = unique(content$class_1)[1]
        )
      })
      
      end_time <- Sys.time()
    })
    print("hi2")
    return (content)
  })
  
  observe(content())
  observe(sentences())
  
  sentences <- eventReactive(input$goButton_sentiment, {
    print("huhuuuuuu!")
    withProgress(message = 'Computing...', value = 0, {
      categories.2 = "all"
      content <- content()
      selected.content <- select.subset(content, input$categores_1_choices.sentiment, categories.2) # Selecting a subset of data
      # We first need to deal with rare but "problematic" sentences returned by the morphosyntactic analyzer: those made only of punctuation signs
      # To do so, we compute both the number of tokens and the number of punctuation signs in each sentence 
      sentences <- as_tibble(selected.content) %>% group_by(class_1, class_2, sentence_id) %>% slice(1) %>% ungroup %>%
        mutate(is.punct = as.numeric(upos == "PUNCT"))
      
      counts <- as_tibble(selected.content) %>% 
        group_by(class_1, class_2, sentence_id) %>% 
        mutate(is.punct = as.numeric(upos == "PUNCT")) %>% 
        summarize(nb.tokens = n(), nb.punct = sum(is.punct)) %>% ungroup %>% select(nb.tokens, nb.punct)
      
      # Now we can remove sentences where the number of tokens is equal to the number of punctuation signs...
      sentences <- sentences %>% bind_cols(counts) %>% filter(nb.tokens != nb.punct) %>% select(-nb.tokens, -nb.punct, -is.punct)
      
      # We compute the number of words in each sentence (which means we have to exclude punctuation signs)
      # We do this to be able to better compare documents with different numbers of tokens (since more words also means more "sentiment words")
      nb.words <- as_tibble(selected.content) %>% 
        filter(upos != "PUNCT") %>% # Dropping punctuation marks
        group_by(class_1, class_2, sentence_id) %>% # Dropping punctuation marks
        summarize(nb.words = n()) %>% ungroup %>% select(nb.words)
      sentences <- sentences %>% bind_cols(nb.words)
      print(sentences)
      # These are the 8 sentiments that are returned by the sentiment analyzer (in addition to the overall valence = +/-)
      eight.sentiments <- c("anger", "anticipation", "disgust", "fear", "joy", "sadness", "surprise", "trust", "negative", "positive")
      
      # First we compute the valence, i.e. whether the texts are overall positive or negative
      my.valences <- get_sentiment(sentences$sentence, method="nrc", language = "english") # sentences <- sentences %>% ungroup %>% mutate(sentiment.valence = get_sentiment(sentence, method="nrc", language = "english")) 
      my.valences <- data.frame(valence=my.valences)

      # Second, we compute the occurrences of the 8 previous sentiments
      my.sentiments <- get_nrc_sentiment(sentences$sentence, language = "english")
      
      # We add everything to our main table
      sentences <- sentences %>% bind_cols(my.valences) %>%  bind_cols(my.sentiments) 
      print("2")
      print(sentences)

      return(sentences)
    })
  })
  
  
  output$sentiment1 <- renderPrint({
    # Let's look at the most positive and negative sentences...
    print("imhere")
    sentences <- sentences()
    # Most positive sentences
    most.positive.sentences <- sentences %>% group_by(category = get(input$results.classification_sentiment)) %>% filter(valence == max(valence)) %>% select(category, sentence, valence)

    return(print(most.positive.sentences$sentence))
  })
  
  output$sentiment2 <- renderPrint({
    sentences <- sentences()
    most.negative.sentences <- sentences %>% group_by(category = get(input$results.classification_sentiment)) %>% filter(valence == min(valence)) %>% select(category, sentence, valence)
    
    # Let's look at the most positive and negative sentences...
    # Most positive sentences
    return(print(most.negative.sentences$sentence))
  })
  
  output$sentiment3 <- renderPrint({
    sentences <- sentences()
    most.negative.sentences <- sentences$most.negative.sentences
    my.words <- get_tokens(most.negative.sentences$sentence, pattern = "\\W")
    word.valences <- get_sentiment(my.words, method="nrc", language = "english")
    
    return(print(my.words[word.valences==1])) # The words which are analyzed as positive
  })
  
  output$sentiment4 <- renderPrint({
    sentences <- sentences()
    most.negative.sentences <- sentences$most.negative.sentences
    my.words <- get_tokens(most.negative.sentences$sentence, pattern = "\\W")
    word.valences <- get_sentiment(my.words, method="nrc", language = "english")
    return(print(my.words[word.valences==-1]))# The words which are analyzed as negative
  })
  output$sentiment5 <- renderPrint({
    sentences <- sentences()
    most.extreme.sentences <- sentences %>% group_by(category = get(input$results.classification)) %>%
      filter(get(input$my.emotion) == max(get(input$my.emotion))) %>% select(category, sentence, input$my.emotion)
    return(print(most.extreme.sentences %>% pull(sentence)))# The words which are analyzed as negative
  })
  
  output$sentiment6 <- renderPrint({
    sentences <- sentences()
    most.extreme.sentences <- sentences %>% group_by(category = get(input$results.classification)) %>%
      filter(get(input$my.emotion) == max(get(input$my.emotion))) %>% select(category, sentence, input$my.emotion)
    my.words <- get_tokens(most.extreme.sentences$sentence, pattern = "\\W")
    word.sentiments <- my.words %>% get_nrc_sentiment(language = "english") %>% select(input$my.emotion)
    return(print(my.words[word.sentiments==1]))# The words which are analyzed as negative
  })
  
  display_sentiment <- function(){
    sentences <- sentences()
    cat.summary <- sentences %>% group_by(category = get(input$results.classification)) %>% 
      summarize_at(vars(nb.words:positive), sum) 
    
    cat.summary <- cat.summary %>% 
      mutate_at(vars(valence:positive), list(~ ./nb.words)) %>%
      gather(key="sentiment", value="level", 3:13)
    
    return(cat.summary)
  }
  
  output$sentiment7 <- renderPlot({
    cat.summary <- display_sentiment()
    cat.summary.valence <- cat.summary %>% filter(sentiment == "valence")

    return(ggplot(data=cat.summary.valence, aes(x=category, y=level)) +
             geom_bar(stat="identity", position=position_dodge()) +
             theme_grey(base_size = 14) + coord_flip() + xlab("Documents") +  ylab("Sentiment valence")
    )# The words which are analyzed as negative
  })
  
  output$sentiment8 <- renderPlot({
    cat.summary <- display_sentiment()
    cat.summary.sentiments <- cat.summary %>% filter(sentiment != "valence") 
    eight.sentiments <- c("anger", "anticipation", "disgust", "fear", "joy", "sadness", "surprise", "trust", "negative", "positive")
    cat.summary.sentiments <- cat.summary.sentiments %>% mutate(sentiment = factor(sentiment, levels = eight.sentiments))
    
    return(ggplot(data=cat.summary.sentiments, aes(x=category, y=level)) +
             geom_bar(stat="identity", position=position_dodge()) + facet_wrap(~ sentiment, scales = "free") +
             theme_grey(base_size = 14) + coord_flip()
    )# The words which are analyzed as negative
  })
  
  output$sentiment9 <- renderPlot({
    cat.summary <- display_sentiment()
    cat.summary.sentiments <- cat.summary %>% filter(sentiment != "valence") 
    eight.sentiments <- c("anger", "anticipation", "disgust", "fear", "joy", "sadness", "surprise", "trust", "negative", "positive")
    
    cat.summary.sentiments <- cat.summary.sentiments %>% mutate(sentiment = factor(sentiment, levels = eight.sentiments))
    
    return(ggplot(data=cat.summary.sentiments, aes(x=category, y=level, fill=sentiment)) +
             geom_bar(stat="identity", position=position_dodge()) +
             theme_grey(base_size = 14) + coord_flip()
    )# The words which are analyzed as negative
  })
  
  output$sentiment10 <- renderPlot({
    sentences <- sentences()
    sentences$index <- 1:nrow(sentences) # Creating an index corresponding to the order of the sentences
    selected.sentiments <- sentences %>% gather(key="sentiment", value="level", 10:20)  # Modifying the format of the data for proper display
    
    return(ggplot(data=selected.sentiments, aes(x=index, y=level, colour=sentiment)) + 
      geom_line(size=0.5) + ggtitle("Evolution of emotions throughout the documents") + 
      xlab("Story time") + ylab("Intensity") + theme_grey(base_size = 12)
    )
  })
  
  output$sentiment11 <- renderPlot({
    sentences <- sentences()
    sentence.sentiments <- sentences %>% select(10:20) #Extract the sentiments before smoothing their value
    
    # Let's smooth the values and draw a new graph
    x.len <- min(500, nrow(sentences) / 10) 
    emotion.smoothed <- as.data.frame(sapply(sentence.sentiments, FUN = get_dct_transform, 
                                             low_pass_size = input$my.smoothing, x_reverse_len = x.len,
                                             scale_vals = FALSE, scale_range = FALSE))
    emotion.smoothed$index <- (1:nrow(emotion.smoothed)) * nrow(sentences) / nrow(emotion.smoothed)
    
    emotions.gathered <- gather(as.data.frame(emotion.smoothed), key="sentiment", value="intensity", 1:11)
    emotions.gathered$sentiment <- as.factor(emotions.gathered$sentiment)
    
    
    # Display positive and negative sentiments
    
    # We call our extract.positions.categories function to get the limits of the categories
    my.limits <- extract.positions.categories(sentences, input$results.classification)
    valences <- emotions.gathered %>% filter(sentiment %in% c("positive", "negative")) # Selecting only the "positive" and "negative" sentiments
    
    # Plotting the graph with smoothed values
    return(ggplot(data=valences, aes(x=index, y=intensity, colour=sentiment)) + 
      geom_line(size=1) + ggtitle("Evolution of positive and negative emotions throughout the documents") + 
      xlab("Story time") + ylab("Intensity") + theme_grey(base_size = 18) +
      geom_vline(xintercept = my.limits$index) +
      annotate("text", x = my.limits$index, y = 0, size = 3, label = my.limits$labels, hjust = 0, vjust = -1, angle = 90)
    )
  })
  
  output$sentiment12 <- renderPlot({
    sentences <- sentences()
    sentence.sentiments <- sentences %>% select(10:20) #Extract the sentiments before smoothing their value
    
    # Let's smooth the values and draw a new graph
    x.len <- min(500, nrow(sentences) / 10) 
    emotion.smoothed <- as.data.frame(sapply(sentence.sentiments, FUN = get_dct_transform, 
                                             low_pass_size = input$my.smoothing, x_reverse_len = x.len,
                                             scale_vals = FALSE, scale_range = FALSE))
    emotion.smoothed$index <- (1:nrow(emotion.smoothed)) * nrow(sentences) / nrow(emotion.smoothed)
    
    emotions.gathered <- gather(as.data.frame(emotion.smoothed), key="sentiment", value="intensity", 1:11)
    emotions.gathered$sentiment <- as.factor(emotions.gathered$sentiment)
    
    
    # Display positive and negative sentiments
    
    my.limits <- extract.positions.categories(sentences, input$results.classification)
    selected.sentiments <- emotions.gathered %>% filter(sentiment %in% my.sentiments)
    
    # Plotting the graph with the selected sentiments
    return(ggplot(data=selected.sentiments, aes(x=index, y=intensity, color=sentiment)) + 
      geom_line(size=1) + ggtitle("Evolution of positive and negative emotions throughout the document(s)") + 
      xlab("Story time") + ylab("Intensity") + theme_grey(base_size = 18) + 
      geom_vline(xintercept = my.limits$index) +
      annotate("text", x = my.limits$index, y = 0, size = 3, label = my.limits$labels, hjust = 0, vjust = -1, angle = 90)
    )
  })
  
  
  output$dispersion <- renderPlot({
    my.stop.words = c(
      "be",
      "have",
      "do",
      "would",
      "will",
      "can",
      "know",
      "see",
      "may",
      "should",
      "get",
      "think",
      "go",
      "make",
      "must",
      "take",
      "come",
      "say",
      "put",
      "want",
      "look",
      "find",
      "need",
      "like",
      "set",
      "use",
      "become",
      "let",
      "feel",
      "try",
      "shall",
      "ask",
      "seem",
      "same",
      "most",
      "few",
      "other",
      "own",
      "many"
    )
    content <- content()
    selected.content <- select.subset(content, input$categories.1_dis, input$categories.2_dis, upos.cat = input$upos.categories_dis, stop.words = my.stop.words) # Selecting a subset of data
    
    # Depending on whether you want to see the most frequent lemmas or your target lemmas, the lines below prepare the right data object
    if (input$display.most.frequent == "yes") {
      nb.lemmas <- txt_freq(selected.content$lemma, order=T)
      nb.lemmas$key <- factor(nb.lemmas$key, levels = rev(nb.lemmas$key))
      head(nb.lemmas, n=nb.displayed.lemmas) # The most frequent lemmas
      target.lemmas <- head(nb.lemmas, n=input$nb.displayed.most.frequent.lemmas)$key
    } else {
      target.lemmas <- input$my.own.lemmas
    }
    
    nb.displayed.lemmas <- length(target.lemmas)
    
    # We now prepare the data objects for the display
    selected.content.all <- select.subset(content, input$categories.1_dis, input$categories.2_dis) #
    sentences <- as_tibble(selected.content.all) %>% group_by(input$class_1, input$class_2, sentence_id) %>% 
      slice(1) %>% ungroup %>% select(class_1, class_2, sentence)
    sentences$index <- 1:nrow(sentences)
    
    lemmas.occurrences <- data.frame(lapply(target.lemmas, function(lemma, sentences) grepl(lemma, sentences$sentence, fixed=T), sentences=sentences))
    colnames(lemmas.occurrences) <- target.lemmas
    
    lemmas.occurrences <- lemmas.occurrences*1
    sentences <- sentences %>% bind_cols(lemmas.occurrences)
    
    lemmas.occurrences <- gather(sentences, key="lemma", value="present", 5:(5+nb.displayed.lemmas-1)) %>% select(index, lemma, present)
    
    # We create something to add vertical lines delimitating the categories
    my.limits <- extract.positions.categories(sentences, input$results.classification_dis)
    
    # Plotting the dispersion of the target lemmas throughout the documents
  
    return(ggplot(data=lemmas.occurrences, aes(x=index, y=present)) + 
             geom_line(size=1) + ylim(0, 1) + 
             xlab("Sentence") + theme_grey(base_size = 18) +
             theme(axis.text.y=element_blank(),  axis.ticks.y=element_blank(), axis.title.x = element_blank()) +
             geom_segment(data=my.limits, aes(x=index, xend=index, y = 0, yend=1), color="red") +
             #geom_vline(xintercept = category.limits, color="blue") +
             facet_wrap(~ lemma, scales = "free") + 
             geom_text(data=my.limits, aes(label = labels), color = "red", size=3, hjust=-0.05, vjust = -0.5, angle = 90) 
    )# The words which are analyzed as negative
  })
  
  output$tm <- renderPlot({
    seed <-list(2003,5,63,100001,765)
    my.stop.words = c(
      "be",
      "have",
      "do",
      "would",
      "will",
      "can",
      "know",
      "see",
      "may",
      "should",
      "get",
      "think",
      "go",
      "make",
      "must",
      "take",
      "come",
      "say",
      "put",
      "want",
      "look",
      "find",
      "need",
      "like",
      "set",
      "use",
      "become",
      "let",
      "feel",
      "try",
      "shall",
      "ask",
      "seem",
      "same",
      "most",
      "few",
      "other",
      "own",
      "many"
    )
    content <- content()
    selected.content <- select.subset(content, input$categories.1_tm, input$categories.2_tm, upos.cat = input$upos.categories_tm, stop.words = my.stop.words) # Selecting a subset of data
    
    # We prepare a document-term matrix, with the document being the different sections of the book
    content.tmp <- document_term_frequencies(selected.content[, c(input$results.classification_tm, "lemma")])
    content.tmp$term <- factor(content.tmp$term)
    content.tmp$doc_id <- factor(content.tmp$doc_id)
    dtm <- document_term_matrix(x = content.tmp)
    
    nrow(dtm) # As many rows as sections
    ncol(dtm) # As many columns as different lemmas
    
    print("im here")
    # We delete rarely occurring terms
    dtm_clean <- dtm_remove_lowfreq(dtm, minfreq = 5)
    
    # We now only keep the 2000 most useful lemmas in terms of tf-idf (so too frequent and therefore little informative terms will disappear)
    dtm_clean <- dtm_remove_tfidf(dtm_clean, top = input$nb.selected.lemmas)
    
    # We can have a quick look at the terms which are the most informative when it comes to separating the selected categories of text documents
    my.terms <- dtm_tfidf(dtm_clean)
    my.terms <- sort(my.terms, decreasing=T)
    (differentiating.lemmas <- names(my.terms)[1:40]) # We display the 40 most informative ones, using TF-IDF
    
    
    # Now that we have the dtm we want, we are going to feed it to algorithms computing topics
    # We first need to find the best number of topics, as LDA does not give you this value but requires it
    # We call a complex functions to do so...
    resultat.nb.topics <- FindTopicsNumber(
      dtm_clean,
      topics = seq(from = input$search.from, to = input$search.to, by = 1),
      metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
      method = "Gibbs",
      control = list(nstart = input$nstart, seed = input$seed, best = input$best, burnin = input$burnin, iter = input$iter, thin=input$thin),
      mc.cores = 4L,
      verbose = TRUE
    )
    
    # We plot various curves which can be used to select the right number of topics
    # A good number of topics should minimize the values of the two metrics on the upper graph
    # And maximize the values of the two metrics on the lower graph
    
    return(FindTopicsNumber_plot(resultat.nb.topics))# The words which are analyzed as negative
  })
  
  
  output$tm2 <- renderPlot({
    seed <-list(2003,5,63,100001,765)
    # Run LDA using Gibbs sampling (we give our tdm matrix to the function, as well as the number of topics)
    ldaOut <- LDA(dtm_clean, input$nb.topics, method="Gibbs", 
                  control=list(nstart = input$nstart, seed = seed, best = input$best, burnin = input$burnin, iter = input$iter, thin=input$thin))
    
    terms(ldaOut, k=30) # 30 most frequent terms for each topic
    
    # The most likely topics for each section
    topics(ldaOut)
    
    # The previous associations are over-simplistic, we could rather investigate how strongly each topic is asociated to words and categories
    
    # Let's draw the highest word probabilities for each topic 
    nb.words.displayed <- 15 # You can change this (but if you choose too many words, your graph will be hard to read)
    
    terms <- as.data.frame(t(posterior(ldaOut)$terms))
    terms$term <- rownames(terms)
    terms <- gather(terms, key="topic", value="beta", 1:input$nb.topics)
    terms$term <- as.factor(terms$term)
    terms <- lapply(split(terms, terms$topic), function(y) head(y[order(-y$beta),], nb.words.displayed))
    terms <- do.call("rbind", terms)
    terms$topic <- as.factor(terms$topic)
    terms <- mutate(terms, topic = paste0("Topic ", topic))
    
    # Plotting the graph for each topic
    return(ggplot(terms, aes(reorder_within(term, beta, topic), beta, fill = factor(topic))) +
      geom_col(alpha = 0.8, show.legend = FALSE) +
      facet_wrap(~ topic, scales = "free") +
      coord_flip() + scale_x_reordered() + 
      labs(x = NULL, y = expression(beta),
           title = "Highest word probabilities for each topic", subtitle = "Different words are associated with different topics") +
      theme_grey(base_size = 12))
  })
  
  output$tm3 <- renderPlot({
    content <- content()
    seed <-list(2003,5,63,100001,765)
    # Run LDA using Gibbs sampling (we give our tdm matrix to the function, as well as the number of topics)
    ldaOut <- LDA(dtm_clean, input$nb.topics, method="Gibbs", 
                  control=list(nstart = input$nstart, seed = seed, best = input$best, burnin = input$burnin, iter = input$iter, thin=input$thin))
    
    terms <- as.data.frame(ldaOut@gamma)
    nb.docs <- nrow(terms)
    colnames(terms)[1:input$nb.topics] <- paste0("Topic ", 1:input$nb.topics)
    my.stop.words = c(
      "be",
      "have",
      "do",
      "would",
      "will",
      "can",
      "know",
      "see",
      "may",
      "should",
      "get",
      "think",
      "go",
      "make",
      "must",
      "take",
      "come",
      "say",
      "put",
      "want",
      "look",
      "find",
      "need",
      "like",
      "set",
      "use",
      "become",
      "let",
      "feel",
      "try",
      "shall",
      "ask",
      "seem",
      "same",
      "most",
      "few",
      "other",
      "own",
      "many"
    )
    selected.content <- select.subset(content, input$categories.1_tm, input$categories.2_tm, upos.cat = input$upos.categories_tm, stop.words = my.stop.words) # Selecting a subset of data
    terms$section <- levels(as.factor(selected.content[,input$results.classification_tm]))
    terms <- gather(terms, key="topic", value="gamma", 1:input$nb.topics)
    terms$topic <- as.factor(terms$topic)
    terms$section <- as.factor(terms$section)
    terms <- lapply(split(terms, terms$topic), function(y) head(y[order(-y$gamma),], nb.docs))
    terms <- do.call("rbind", terms)
    
    # Plotting the graph for each topic
    return (ggplot(terms, aes(topic, gamma, fill = factor(section))) +
      geom_col(alpha = 0.8, show.legend = FALSE) +
      facet_wrap(~ section, scales = "free") +
      coord_flip() + scale_x_reordered() + 
      labs(x = NULL, y = expression(beta),
           title = "Topic probabilities for each section") +
      theme_grey(base_size = 14)
    )
    })
  
  analysis <- eventReactive(input$goButton1, {
    categories.2 <- "all"
    selected.content <-
      select.subset(content(), input$categories.1, categories.2,input$upos.category, input$my.stop.words) # Selecting a subset of data
    analysis <-
      selected.content %>% filter(upos == input$upos.category) %>% pull(input$see.as) %>% unique()
    return(analysis)
  })
  
  analysis2 <- eventReactive(input$goButton2, {
    categories.2 <- "all"
    selected.content <-
      select.subset(content(), input$categories.1, categories.2,input$upos.category, input$my.stop.words) # Selecting a subset of data
    analysis2 <-
      selected.content %>% filter(get(input$target.term) == input$checked.term) %>% pull(sentence)
    return(analysis2)
  })
  observe(content())
  
  frequency1 <- eventReactive(input$goButton3, {
    categories.2 <- "all"
    selected.content <- select.subset(content(), input$categories.1.frequency, categories.2, input$upos.category_frequency, input$my.stop.words_frequency) # Selecting a subset of data
    # Let's find the most frequent lemmas in the subset of documents:
    nb.lemmas <- txt_freq(selected.content$lemma, order=T)
    nb.lemmas$key <- factor(nb.lemmas$key, levels = rev(nb.lemmas$key))
    return(nb.lemmas)
  })
  
  
  output$frequency3 <- renderWordcloud2({
    if (is.null(frequency1())) {
      return (NULL)
    } else{
      frequenct <- frequency1()
      wordcloud2(data = head(frequenct, n=input$nb.words.wordcloud), size = 0.7) # changing size will draw words bigger/smaller (one has to adjust to the window size)
    }
  })
  
  output$analysis <- renderPrint({
    if (is.null(analysis())) {
      return (NULL)
    } else{
      return(print(analysis()))
    }
  })
  
  output$analysis2 <- renderPrint({
    if (is.null(analysis2())) {
      return (NULL)
    } else{
      return(print(analysis2()))
    }
  })
  
  output$content <- renderTable({
    if (is.null(content())) {
      return (NULL)
    } else{
      head(content(), n = 20)
    }
  })
  
})
