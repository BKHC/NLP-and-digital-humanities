# list.of.packages <- c("shiny", "ggplot2", "tidyverse")
# new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# if(length(new.packages)) install.packages(new.packages)

library(tidyverse)
library(shiny)
library(shiny.i18n)
library(showtext)

rm(list = ls()) # Emptying the memory
showtext_auto()
i18n <- Translator$new(translation_json_path = "translation.json")
i18n$set_translation_language("en")

vowels <- c("a", "i", "u") # Possible vowels
consonants <- c("p", "b", "k") # Possible consonants

# Choose a phoneme randomly among a number of options
get_phoneme <- function(options) {
  id <- sample(1:length(options), 1)
  return (options[id])
}

# Create a CVCV word with consonants and vowels
create_word <- function(consonants, vowels) {
  word <- paste0(get_phoneme(consonants),get_phoneme(vowels),
                 get_phoneme(consonants),get_phoneme(vowels))
  return (word)
}

# Description of the interaction between a speaker and a listener around a predefined concept
interact <- function(speaker, listener, chosen_concept, p_create, p_learn, memories) {
  #print(paste0("Speaker: ", speaker))
  #print(paste0("Listener: ", listener))
  #print(paste0("Chosen concept: ", chosen_concept))
  
  memories <- memories %>% mutate(row_id = row_number()) # Preparing ids to later modify some of the entries in the global memory of the game
  #print(memories)
  
  # SPEAKER'S SIDE
  
  speaker_memory <- memories %>% filter(agent == speaker, concept == chosen_concept) # Extracting all the entries about the concept in speaker's memory

  if (nrow(speaker_memory) == 0) { # No available word yet
    if (runif(1) < p_create) { # Assessing whether to create a new word
      new_word <- create_word(consonants, vowels)
      memories <- memories %>% add_row(agent = speaker, concept = chosen_concept, word = new_word, score = 0.5) # Adding the new word in the speaker's memory in association with the concept
      #print(paste0("New word added: ", new_word))
    }
    else {
      #print("Nothing happens")
    }
    return (list(memories = memories %>% select(-row_id), success = 0)) # Failure of the interaction; returning the global memory and the 0 success
  }
  
  # Pick one word among the associations with the highest score for the concept in the speaker's memory
  possible_options <- speaker_memory %>% filter(score == max(speaker_memory %>% pull(score))) # Getting all the best memory entries (possibly 1 only)
  chosen_option <- possible_options %>% sample_n(1) # Choosing one memory entry among the best

  #print(possible_options)
  #print(chosen_option)

  chosen_word <- chosen_option %>% pull(word) # Extracting the word to be conveyed to the listener
  chosen_row_speaker  <- chosen_option %>% pull(row_id) # Getting the row of the speaker's memory entry for later update
  
  #print(paste0("Chosen Word: ", chosen_word))
  #print(paste0("Chosen Row: ", chosen_row_speaker))
  
  # LISTENER'S SIDE
  
  # Checking the listener now...
  listener_memory <- memories %>% filter(agent == listener, word == chosen_word) # Looking if the word is in the listener's memory

  if (nrow(listener_memory) == 0) { # Unknown word yet
    # The listener does not know the word
    if (runif(1) < p_learn) { # Assessing whether to learn the word and its relation with the chosen concept
      memories <- memories %>% add_row(agent = listener, concept = chosen_concept, word = chosen_word, score = 0.5) # Adding the new word in the listener's memory in association with the concept
      #print("The word is learnt")
    }
    else {
      #print("The word is not learnt")
    }
    memories[chosen_row_speaker, ]$score <-  max(0, memories[chosen_row_speaker, ]$score - 0.2)
    return (list(memories = memories %>% select(-row_id), success = 0)) # Failure of the interaction; returning the global memory and the 0 success
    
  }
  
  # The listener knows the word
  # Pick one word among the associations with the highest score for the word in the listener's memory
  possible_options <- listener_memory %>% filter(score == max(listener_memory %>% pull(score))) # Getting all the best memory entries (possibly 1 only)
  possible_option <- possible_options %>% sample_n(1) # Choosing one memory entry among the best
  
  chosen_concept_listener <- possible_option %>% pull(concept) # Extracting the concept to be compared to the chosen concept
  chosen_row_listener <- possible_option %>% pull(row_id) # Getting the row of the listener's memory entry for later update
  
  #print(chosenConceptListener)
  #print(chosenRowListener)
    
  if (chosen_concept == chosen_concept_listener) { # The speaker and listener have the same favorite word for the chosen concept
    # Success
    #print("Success")
    memories[chosen_row_listener, ]$score <- min(memories[chosen_row_listener, ]$score + 0.2, 1) # Increasing the score of the speaker's association
    memories[chosen_row_speaker, ]$score <- min(memories[chosen_row_speaker, ]$score + 0.2, 1) # Increasing the score of the listener's association
    return (list(memories = memories %>% select(-row_id), success = 1)) # Success of the interaction; returning the global memory and the 1 success
    
  }
  else { # The speaker and listener have different words for the chosen concept
    # Failure
    #print("Failure")
    memories[chosen_row_listener, ]$score <- max(memories[chosen_row_listener, ]$score - 0.2, 0) # Decreasing the score of the speaker's association
    memories[chosen_row_speaker, ]$score <-  max(memories[chosen_row_speaker, ]$score - 0.2, 0) # Decreasing the score of the listener's association

    return (list(memories = memories %>% select(-row_id), success = 0)) # Failure of the interaction; returning the global memory and the 0 success
  }

}


# Simulating one time step, i.e. a number of pair interactions
compute_one_step <- function(nb_agents, nb_concepts, p_create, p_learn, memories) {
  my_pairs <- sample(1:nb_agents, nb_agents, replace=F) # Create a randomly vector of agents' ids
    
  nb_pairs <- floor(nb_agents/2) # Compute the number of possible pairs (the last agent)
  average_success <- 0
    
  for (j in 1:nb_pairs) { # Go through the pairs
    speaker <- my_pairs[2*j-1] # Identify the speaker
    listener <- my_pairs[2*j] # Identify the listener
    chosen_concept <- sample(1:nb_concepts, 1) # Choose a concept
      
    output <- interact(speaker, listener, chosen_concept, p_create, p_learn, memories) # Perform the interaction
    memories <- output$memories # Update the global memory
    success <- output$success
    
    average_success <- average_success + success
  }
  
  # Compute the average score of the 'best' words
  av_score_best_words <- memories %>% group_by(concept, agent) %>% filter(score == max(score)) %>% ungroup %>%
      group_by(concept) %>% summarize(avg_value = mean(score)) %>% ungroup %>%
      select(avg_value)
  av_score_best_words <- mean(av_score_best_words$avg_value)
  
  # Return the global memory, the average success over the pairs, and the average score of the best words
  return (list(memories = memories, success = average_success / nb_pairs, av_score_best_words = av_score_best_words))
}

# Simulating all time steps
evolve <- function(nb_steps, nb_agents, nb_concepts, p_create, p_learn) {

  # Declare a global memory, containing all the memories of all the agents
  memories <- tibble(concept = integer(), agent = integer(), word = character(),  score = double())
  
  # Declare a tibble to collect the measures
  my_measures <- tibble(time = integer(), success = double(), av_score_best_words = integer())
  
  for (i in 1:nb_steps) {
    if (i %% 100 == 0)
      print(i)

    incProgress(1/nb_steps) # Increase the progress bar

    one_step_results <- compute_one_step(nb_agents, nb_concepts, p_create, p_learn, memories) # Simulate one time step
    memories <- one_step_results$memories # Update the global memory
    
    # Update the table of measures
    my_measures <- my_measures %>% add_row(time = i, success = one_step_results$success, av_score_best_words = one_step_results$av_score_best_words)
  }

  return (my_measures)
}

#results <- evolve(nb_steps, nbAgents, nbConcepts, pCreate, pLearn)

#p <- ggplot(data=results, aes(x=time, y=success)) + geom_line() + ylim(0,1) + geom_smooth(method = loess)
#p <- p + xlab("Time") + ylab("Success") + ggtitle("Evolution of success")
#p <- p + theme(text = element_text(size=15, family="serif"))
#p

#p <- ggplot(data=results, aes(x=time, y=memory_size)) + geom_line() + ylim(0,1) + geom_smooth(method = loess)
#p <- p + xlab("Time") + ylab("Average score of the best words across concepts") + ggtitle("Evolution of the scores of the best words")
#p <- p + theme(text = element_text(size=15, family="serif"))
#p

# memories %>% arrange(concept, agent) %>% select(word) %>% unique() %>% nrow()
# memories %>% arrange(word) %>% select(word) %>% unique() %>% nrow()
# memories %>% arrange(concept, agent) %>% tbl_df %>% print(n = nrow(.))
# memories %>% group_by(concept, agent) %>% filter(score == max(score)) %>% ungroup %>% arrange(concept, agent) %>% tbl_df %>% print(n = nrow(.))
# memories %>% group_by(concept, agent) %>% filter(score == min(score)) %>% ungroup %>% arrange(concept, agent) %>% tbl_df %>% print(n = nrow(.))


# Define the interface with the various parameters and the two displays
ui <- fluidPage(
  shiny.i18n::usei18n(i18n),
  div( style = "float: right;",
       selectInput('selected_language',
                   i18n$t("Change language"),
                   choices = i18n$get_languages(),
                   selected = i18n$get_key_translation())
  ),
  titlePanel(i18n$t("*** Naming Games ***")),
  
  sidebarLayout(
    sidebarPanel(h4(i18n$t("Parameters of the simulation")),
                 numericInput(inputId = "nb_steps", label = i18n$t("Number of time steps"), value = 500, min = 10, max = 5000, step=1),
                 h4(i18n$t("Parameters of the linguistic model")),
                 numericInput(inputId = "nb_agents", label = i18n$t("Number of agents"), value = 5, min = 2, max = 100, step=1),
                 numericInput(inputId = "nb_concepts", label = i18n$t("Number of concepts"), value = 5, min = 1, max = 50, step=1),
                 numericInput(inputId = "p_create", label = i18n$t("Probability to create a new word"), value = 0.3, min = 0, max = 1, step = 0.05),
                 numericInput(inputId = "p_learn", label = i18n$t("Probability to learn a new word"), value = 0.3, min = 0, max = 1, step = 0.05),
                 h4(i18n$t("Run simulation")),
                 actionButton("goButton", i18n$t("Compute!"))
    ),
    mainPanel(h3(i18n$t("Evolution of the success rate")), plotOutput("plot_evolution"),
              h3(i18n$t("Evolution of the scores of the best words")), plotOutput("plot_scores"))
  )
)


# Code for the server
server <- function(input, output, session) {
  showtext_auto()
  shiny.i18n::usei18n(i18n)
  observeEvent(input$selected_language, {
    update_lang(session, input$selected_language)
  })
  results <- eventReactive(input$goButton,{
    print("Computing!")
    
    withProgress(message = 'Computing...', value = 0, {
      start_time <- Sys.time()
      results <- evolve(input$nb_steps, input$nb_agents, input$nb_concepts, input$p_create, input$p_learn)
      end_time <- Sys.time()
      print(end_time - start_time)
    })
    
    return (results)
  })
  output$plot_evolution <- renderPlot({
    if (is.null(results()))
      return (NULL)
    p <- ggplot(data=results(), aes(x=time, y=success)) + geom_line() + ylim(0,1) + geom_smooth(method = loess)
    p <- p + xlab(i18n$t("Time")) + ylab(i18n$t("Success")) + ggtitle(i18n$t("Evolution of success"))
    p <- p + theme(text = element_text(size=15, family="serif"))
    p
    
  })
  
  output$plot_scores <- renderPlot({
    if (is.null(results()))
      return (NULL)
    p <- ggplot(data=results(), aes(x=time, y=av_score_best_words)) + geom_line() + ylim(0,1) + geom_smooth(method = loess)
    p <- p + xlab(i18n$t("Time")) + ylab(i18n$t("Average score of the best words across concepts")) + ggtitle(i18n$t("Evolution of the scores of the best words"))
    p <- p + theme(text = element_text(size=15, family="serif"))
    p
  })
  
}

shinyApp(ui, server)



