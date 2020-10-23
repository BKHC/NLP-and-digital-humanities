list.of.packages <- c("shiny", "statnet", "igraph", "ggplot2", "tidyverse", "ggraph")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(shiny)
library(statnet)
library(igraph)
library(ggplot2)
library(tidyverse)
library(ggraph)

rm(list = ls()) # Emptying the memory

extrafont::loadfonts() # Load some extra fonts

# A function to darken a set of colors
darken <- function(color, factor=1.4){
  col <- col2rgb(color)
  col <- col/factor
  col <- rgb(t(col), maxColorValue=255)
  return (col)
}

# Define the colors
my_colors <- rev(heat.colors(5, alpha=1))
my_colors <- darken(my_colors)
names(my_colors) <- as.character(1:5)

# Function to choose the colors when displaying the graphs
custom_colors <- function(universe, ...){
  col.names <- c(...)
  cols <- sapply(col.names, FUN=function(x) universe[[x]])
  return(unname(cols))
}

# Function to display a graph
display_graph <- function(g, col) {
  V(g)$color <- custom_colors(col, as.character(V(g)$age)) # Assigning colors to vertices according to age
  V(g)$label <- ifelse(V(g)$variant == T, "B", "A") # Displayin 0 and 1 as A and B
  #V(g)$shape <- ifelse(V(g)$variant == T, "square", "circle") # Assigning a shape to vertices according to the variant
  
  set.seed(12345) # This is to ensure stability of the display across time steps
  # g %>% plot(vertex.size=5, edge.arrow.size = rep(0.3, ecount(g)), layout = layout_nicely) # plot with igraph
  # plot with ggraph
  lay = create_layout(g, layout = "nicely")
  my_graph <- ggraph(lay) + geom_edge_link(color = "blue", width = 0.2) +
              geom_node_point(aes(shape = label, color=factor(age)), size=5) + 
              theme_graph(base_family="sans") + scale_colour_manual(values = col)
  set.seed(NULL) # This is to ensure randomness elsewhere
  
  return (my_graph)
}

# Function to display the evolution of the percentage of variant 1/B through time
display_evolution <- function(percents) {
  p <- ggplot(data=percents, aes(x=time, y=percent, group=run)) + geom_line(aes(color=run)) + ylim(0,1)
  p <- p + xlab("Time") + ylab("Percent B") + ggtitle("Evolution through time")
  p <- p + theme(text = element_text(size=15, family="serif"))
  return (p)
}


# Function to compute the variant of a vertex according to its neighbors
# The four last arguments are here to speed up the computations
update_variant <- function(neighbors, a, b, p_imperfect_learning, status_0, status_1, variant_0, variant_1) {
  
  impact_0 <- sum(variant_0[neighbors])^a * sum(status_0[neighbors]) # Impact of variant 0/A
  impact_1 <- b * sum(variant_1[neighbors])^a * sum(status_1[neighbors]) # Impact of variant 1/B
  
  # Updating the variant
  
  
  
  if (impact_0 == impact_1) { # If both variant have the same weight, one is chosen randomly
    # print("equal")
    if (runif(1) < 0.5)
      return (F)
    else
      return (T)
  }
  
  #print("test")
  
  if (impact_0 > impact_1)
    final <- F
  else
    final <- T
  
  # Imperfect learning
  if (runif(1) < p_imperfect_learning) { # Assess whether there is imperfect learning
    #print("imperfect learning")
    final <- ! final
  }
  
  #print(final)
  return (final)
}


# Update the graph for one-time step
update_graph <- function(g, a, b, p_imperfect_learning, modify_status_at_birth) {

  # Children (ages 1) have no impact --> Compute status and variant temporary variables for which children are discarded
  # We compute these temporary variables once only, and them will use them to update the variants in children and teenagers much faster
  
  influencers <- V(g)$age > 1
  status_0 <- V(g)$status * influencers * !V(g)$variant
  status_1 <- V(g)$status * influencers * V(g)$variant
  
  variant_0 <- ! V(g)$variant & influencers
  variant_1 <- V(g)$variant & influencers
  
  # Update children (aged 1) and teenagers (aged 2) 
  young_people <- V(g)$age < 3
  young_people_vertices <- V(g)[young_people]
  
  #print(paste0("Variants  ", paste0(V(g)$variant, collapse=" ")))
  #print(paste0("Age       ", paste0(V(g)$age, collapse=" ")))
  #print(paste0("Status    ", paste0(V(g)$status, collapse=" ")))
  #print(paste0("Imp_st_0  ", paste0(status_0, collapse=" ")))
  #print(paste0("Imp_st_1  ", paste0(status_1, collapse=" ")))
  #print(paste0("Imp_var_0 ", paste0(variant_0, collapse=" ")))
  #print(paste0("Imp_var_1 ", paste0(variant_1, collapse=" ")))
  
  # For each 'young people', collect the neighbors
  neighbors <- adjacent_vertices(g, v = young_people_vertices, mode = "all")
  neighbors <- lapply(neighbors, as.numeric)
  
  # Update status for each young person given his/her neighbors
  # V(g)[young_people]$variant <- mapply(update_variant, young_people_vertices, neighbors, MoreArgs=list(a, b, p_imperfect_learning))
  V(g)[young_people]$variant <- sapply(neighbors, update_variant, a=a, b=b, p_imperfect_learning=p_imperfect_learning,
                                       status_0, status_1, variant_0, variant_1) # Much faster than mapply
  # It is much faster to give lists of integers than lists of vertices to sapply (time divided by 2)
  
  # Update age
  V(g)$age <- V(g)$age + 1
  
  # Replace dying agents with children
  dying <- V(g)$age==6
  nb_dying <- sum(dying)
  V(g)$age <- replace(V(g)$age, dying, 1)
  
  if (modify_status_at_birth) # Assessing whether newborns should inherit their status or get a new random one
    V(g)$status <- replace(V(g)$status, dying, sample(1:5, nb_dying, replace=T))
  
  return (g)
}

# Run the simulation
# Beware: if p instead of p_imperfect_learning, the value can be changed when creating certain types of networks...
run_simulation <- function(upper_time_limit, nb_runs, network_type, num_nodes, a, b, p_imperfect_learning, modify_status_at_birth) {
  
  time <- 1:(upper_time_limit-1)
  percents <- tibble(run = integer(), time = integer(), percent = double()) # Create a structure to store the percentages of variant B in the vertices
  stored_graphs <- vector("list", nb_runs) # Create the structure to store graphs
  
  # Simulate the number of runs
  for (j in 1:nb_runs) {
    #set.seed(NULL)
    
    # Create a graph
    # We define graphs so that the average degree is always (nearly) 4, regardless of the architecture
    # This makes graphs more comparable
    switch(network_type, 
           sw={ g <- sample_smallworld(dim = 1, size = num_nodes, nei = 2, p = 0.05) }, # average degree 4
           er={ g <- erdos.renyi.game(n = num_nodes, p.or.m = num_nodes * 2, type="gnm") }, # average degree 4
           ba={ g <- barabasi.game(n = num_nodes, m = 2, directed = F) } # average degree 3.94
           # re={ g <- sample_k_regular(num_nodes, 5, directed = FALSE, multiple = FALSE) }
           # ne={ g <- connect.neighborhood(graph.ring(num_nodes), 4) }
           # tr={ g <- graph.tree(num_nodes, children = 2) }
           # rg={ g <- make_ring(num_nodes) }
    )
    
    # Define ages, status and variants
    ages <- sample(1:5,num_nodes,replace=T) 
    status <- sample(1:5,num_nodes,replace=T)
    variants <- rep(F, num_nodes)
    #variants[sample(1:num_nodes,1)] <- T
  
    # Assign properties to the vertices
    vertex_attr(g) <- list(age = ages, status = status, variant = variants)
  
    # Declare a temporary structure to store graphs for a given run
    stored_graphs_sub <- vector("list", upper_time_limit)
    stored_graphs_sub[[1]] <- g # Store the initial graph
    
    # Simulate the time steps
    for (i in time) {
      g_tmp <- update_graph(g, a, b, p_imperfect_learning, modify_status_at_birth) # Compute the updated graph
      stored_graphs_sub[[i+1]] <- g_tmp; # Store the updated graph
      g <- g_tmp # Update the graph
      percents <- percents %>% add_row(run = j, time = i, percent = sum(V(g)$variant) / length(V(g)$variant)) # Store the percentages
    }
    
    stored_graphs[[j]] <- stored_graphs_sub # Store the graphs of the current run
    incProgress(1/nb_runs) # Increase the progress bar
  }
 
  percents <- percents %>% mutate(run = as.factor(run)) # Consider run as a factor
  
  return (list(evolution = percents, graphs = stored_graphs))
}

# Code for the server
server <- function(input, output, session) {

  observeEvent(input$selected_language, {
    update_lang(session, input$selected_language)
  })

  results <- eventReactive(input$goButton,{
    print("Computing!")
 
    withProgress(message = 'Computing...', value = 0, {
      start_time <- Sys.time()
      results <- run_simulation(input$upper_time_limit, input$nb_runs, 
                                input$network_type, input$num_nodes, 
                                input$a, input$b, input$p_imperfect_learning, 
                                ifelse(input$modify_status_at_birth == "yes", T, F)
                                )
      end_time <- Sys.time()
      print(end_time - start_time)
    })
    
    return (results)
  })
  
  output$plot_evolution <- renderPlot({
    if (is.null(results()))
      return (NULL)
    display_evolution(results()$evolution)
  })
  
  output$plot_network <- renderPlot({
    which_run <- input$which_run
    slice <- input$slice
    
    if (which_run > input$nb_runs)
      which_run <- input$nb_runs
    
    if (slice > input$upper_time_limit)
      slice <- input$upper_time_limit
    
    if (is.null(results()))
      return (NULL)
    results()$graphs[[which_run]][[slice]] %>% display_graph(my_colors)
  })
  
}
