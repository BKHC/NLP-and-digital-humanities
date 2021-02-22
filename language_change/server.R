#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(ggplot2)
library(shiny)
options(warn = -1)
library(igraph)
library(netdiffuseR)
library(diffusr)
library(network)

regular_sim = NULL
random_sim = NULL
small_world_sim = NULL
scale_free_sim = NULL

scale_free = NULL
regular = NULL
random = NULL
scale_free = NULL
# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  observeEvent(input$selected_language, {
    update_lang(session, input$selected_language)
  })
  simulation <-
    function(g,
             number_of_runs,
             functional_bias,
             number_of_nodes) {
      number_of_changed = c()
      for (i in 1:number_of_runs) {
        new_state = c()
        for (node in V(g)) {
          if (V(g)$age[node] <= 2) {
            #learner
            if (compute_fitness(g, node, functional_bias) > 0) {
              new_state <- c(new_state, 1)
            } else{
              new_state <- c(new_state, -1)
            }
          } else{
            #adult
            new_state <- c(new_state, V(g)$state[node])
          }
        }
        V(g)$state <- new_state
        V(g)$age <- V(g)$age %% 5 + 1
        number_of_changed = c(number_of_changed, c(table(V(g)$state)["-1"] /
                                                     number_of_nodes))
        print(i)
      }
      
      return(number_of_changed)
    }
  
  compute_fitness <-
    function(g, v, functional_bias) {
      #positive then unchanged
      neighbors = V(g)$state[neighbors(g, v)] > 0
      sum <-
        length(neighbors[neighbors == TRUE]) - length(neighbors[neighbors == FALSE]) *
        functional_bias
      return(sum)
    }
  
  regular_sim <- eventReactive(input$goButton, {
    print("Computing!")
    
    withProgress(message = 'Running Regular Network Simulation...', value = 0, {
      start_time <- Sys.time()
      
      set.seed(1)
      regular <-
        sample_k_regular(
          input$number_of_nodes,
          input$degree,
          directed = FALSE,
          multiple = FALSE
        )
      V(regular)$age <-
        c(
          sample(
            1:2,
            input$number_of_nodes * input$ratio_learner,
            replace = T
          ),
          sample(
            3:5,
            input$number_of_nodes * (1 - input$ratio_learner) - input$number_of_innovator,
            replace = T
          ),
          3
        )
      V(regular)$state <-
        c(
          rep(1, input$number_of_nodes - input$number_of_innovator),
          rep(-1, input$number_of_innovator)
        )
      
      regular$layout <- layout.fruchterman.reingold
      
      regular$layout <- layout.fruchterman.reingold
      V(regular)$color = V(regular)$age + 1 
      
      #color 
      V(regular)[V(regular)$state==-1]$shape <- "square"
      V(regular)[V(regular)$state==1]$shape <- "circle"
      
      output$regular_graph <-
        renderPlot(
          plot(
            regular,
            vertex.label = NA,
            vertex.size = 2
          ),
          height = 700,
          width = 800
        )
      
      output$regular_max_degree <-
        renderText({
          paste("Maximum Degree:", max(degree(regular)))
        })
      output$regular_min_degree <-
        renderText({
          paste("Minimum Degree:", min(degree(regular)))
        })
      output$regular_mean_degree <-
        renderText({
          paste("Mean Degree:", mean(degree(regular)))
        })
      output$regular_transitivity <-
        renderText({
          paste("Transitivity:", transitivity(regular))
        })
      output$regular_mean_distance <-
        renderText({
          paste("Mean Distance:",
                mean_distance(regular, directed = FALSE, unconnected = TRUE))
        })
      output$regular_diameter <-
        renderText({
          paste("Diameter:", diameter(regular, weights = NA))
        })
      output$regular_assortativity <-
        renderText({
          paste("Assortativity:",
                assortativity.nominal(regular, as.numeric(as.factor(
                  V(regular)$state
                ))))
        })
      
      regular_sim <-
        simulation(regular,
                   input$number_of_runs,
                   input$functional_bias,
                   input$number_of_nodes)
      end_time <- Sys.time()
      print(end_time - start_time)
    })
    return (regular_sim)
  })
  
  random_sim <- eventReactive(input$goButton, {
    print("Computing!")
    
    withProgress(message = 'Running Random Network Simulation...', value = 0, {
      start_time <- Sys.time()
      
      set.seed(1)
      random <-
        erdos.renyi.game(
          input$number_of_nodes,
          p.or.m = input$degree * input$number_of_nodes / (input$number_of_nodes *
                                                             (input$number_of_nodes - 1))
        )
      V(random)$age <-
        c(
          sample(
            1:2,
            input$number_of_nodes * input$ratio_learner,
            replace = T
          ),
          sample(
            3:5,
            input$number_of_nodes * (1 - input$ratio_learner) - input$number_of_innovator,
            replace = T
          ),
          3
        )
      V(random)$state <-
        c(
          rep(1, input$number_of_nodes - input$number_of_innovator),
          rep(-1, input$number_of_innovator)
        )
      
      random$layout <- layout.fruchterman.reingold
      V(random)$color = V(random)$age + 1 
      V(random)[V(random)$age>2]$color <- "1"
      V(random)[V(random)$age<=2]$color <- "5"
      
      #color 
      V(random)[V(random)$state==-1]$shape <- "square"
      V(random)[V(random)$state==1]$shape <- "circle"
      
      output$random_graph <-
        renderPlot(
          plot(
            random,
            vertex.label = NA,
            vertex.size = 2
          ),
          height = 700,
          width = 800
        )
      
      output$random_max_degree <-
        renderText({
          paste("Maximum Degree:", max(degree(random)))
        })
      output$random_min_degree <-
        renderText({
          paste("Minimum Degree:", min(degree(random)))
        })
      output$random_mean_degree <-
        renderText({
          paste("Mean Degree:", mean(degree(random)))
        })
      output$random_transitivity <-
        renderText({
          paste("Transitivity:", transitivity(random))
        })
      output$random_mean_distance <-
        renderText({
          paste("Mean Distance:",
                mean_distance(random, directed = FALSE, unconnected = TRUE))
        })
      output$random_diameter <-
        renderText({
          paste("Diameter:", diameter(random, weights = NA))
        })
      output$random_assortativity <-
        renderText({
          paste("Assortativity:",
                assortativity.nominal(random, as.numeric(as.factor(
                  V(random)$state
                ))))
        })
      
      random_sim <-
        simulation(random,
                   input$number_of_runs,
                   input$functional_bias,
                   input$number_of_nodes)
      
      end_time <- Sys.time()
      print(end_time - start_time)
    })
    return (random_sim)
  })
  
  small_world_sim <- eventReactive(input$goButton, {
    print("Computing!")
    
    withProgress(message = 'Running Small World Network Simulation...', value = 0, {
      start_time <- Sys.time()
      
      set.seed(1)
      small_world <-
        sample_smallworld(1,
                          input$number_of_nodes,
                          input$degree / 2,
                          input$rewiring_p)
      V(small_world)$age <-
        c(
          sample(
            1:2,
            input$number_of_nodes * input$ratio_learner,
            replace = T
          ),
          sample(
            3:5,
            input$number_of_nodes * (1 - input$ratio_learner) - input$number_of_innovator,
            replace = T
          ),
          3
        )
      V(small_world)$state <-
        c(
          rep(1, input$number_of_nodes - input$number_of_innovator),
          rep(-1, input$number_of_innovator)
        )
      
      small_world$layout <- layout.fruchterman.reingold
      V(small_world)$color = V(small_world)$age + 1 
      
      #color 
      V(small_world)[V(small_world)$state==-1]$shape <- "square"
      V(small_world)[V(small_world)$state==1]$shape <- "circle"
      
      output$small_world_graph <-
        renderPlot(
          plot(
            small_world,
            vertex.label = NA,
            vertex.size = 2
          ),
          height = 700,
          width = 800
        )
      
      output$small_world_max_degree <-
        renderText({
          paste("Maximum Degree:", max(degree(small_world)))
        })
      output$small_world_min_degree <-
        renderText({
          paste("Minumum Degree:", min(degree(small_world)))
        })
      output$small_world_mean_degree <-
        renderText({
          paste("Mean Degree:", mean(degree(small_world)))
        })
      output$small_world_transitivity <-
        renderText({
          paste("Transitivity:", transitivity(small_world))
        })
      output$small_world_mean_distance <-
        renderText({
          paste(
            "Mean Distance:",
            mean_distance(
              small_world,
              directed = FALSE,
              unconnected = TRUE
            )
          )
        })
      output$small_world_diameter <-
        renderText({
          paste("Diameter:", diameter(small_world, weights = NA))
        })
      output$small_world_assortativity <-
        renderText({
          paste("Assortativity:",
                assortativity.nominal(small_world, as.numeric(as.factor(
                  V(small_world)$state
                ))))
        })
      
      
      small_world_sim <-
        simulation(
          small_world,
          input$number_of_runs,
          input$functional_bias,
          input$number_of_nodes
        )
      
      end_time <- Sys.time()
      print(end_time - start_time)
    })
    return (small_world_sim)
  })
  
  observeEvent(input$goButton, {
    updateTabsetPanel(session, "inTabset",
                      selected = "panel1")
  })
  
  scale_free_sim <- eventReactive(input$goButton, {
    print("Computing!")
    
    withProgress(message = 'Running Scale Free Network Simulation...', value = 0, {
      start_time <- Sys.time()
      set.seed(1)
      scale_free <-
        sample_pa(input$number_of_nodes,
                  m = input$degree / 2,
                  directed = FALSE)
      V(scale_free)$age <-
        c(
          sample(
            1:2,
            input$number_of_nodes * input$ratio_learner,
            replace = T
          ),
          sample(
            3:5,
            input$number_of_nodes * (1 - input$ratio_learner) - input$number_of_innovator,
            replace = T
          ),
          3
        )
      V(scale_free)$state <-
        c(
          rep(1, input$number_of_nodes - input$number_of_innovator),
          rep(-1, input$number_of_innovator)
        )
      
      scale_free$layout <- layout.fruchterman.reingold
      V(scale_free)$color = V(scale_free)$age + 1 
      
      #color 
      V(scale_free)[V(scale_free)$state==-1]$shape <- "square"
      V(scale_free)[V(scale_free)$state==1]$shape <- "circle"
      
      output$scale_free_graph <-
        renderPlot(
          plot(
            scale_free,
            vertex.label = NA,
            vertex.size = 2
          ),
          height = 700,
          width = 800
        )
      
      output$scale_free_max_degree <-
        renderText({
          paste("Maximum Degree:", max(degree(scale_free)))
        })
      output$scale_free_min_degree <-
        renderText({
          paste("Minimum Degree:", min(degree(scale_free)))
        })
      output$scale_free_mean_degree <-
        renderText({
          paste("Mean Degree:", mean(degree(scale_free)))
        })
      output$scale_free_transitivity <-
        renderText({
          paste("Transitity:", transitivity(scale_free))
        })
      output$scale_free_mean_distance <-
        renderText({
          paste("Distance:",
                mean_distance(
                  scale_free,
                  directed = FALSE,
                  unconnected = TRUE
                ))
        })
      output$scale_free_diameter <-
        renderText({
          paste("Diameter:", diameter(scale_free, weights = NA))
        })
      output$scale_free_assortativity <-
        renderText({
          paste("Assortativity:",
                assortativity.nominal(scale_free, as.numeric(as.factor(
                  V(scale_free)$state
                ))))
        })
      
      scale_free_sim <-
        simulation(
          scale_free,
          input$number_of_runs,
          input$functional_bias,
          input$number_of_nodes
        )
      
      end_time <- Sys.time()
      print(end_time - start_time)
    })
    return (scale_free_sim)
  })
  
  output$regular_plot <- renderPlot({
    if (is.null(regular_sim())) {
      return (NULL)
    } else{
      plot(regular_sim(),
           type = "o",
           ylab = "Percentaget of C",
           xlab = "Generation")
    }
  })
  
  output$random_plot <- renderPlot({
    if (is.null(random_sim())) {
      return (NULL)
    } else{
      plot(random_sim(),
           type = "o",
           ylab = "Percentaget of C",
           xlab = "Generation")
    }
  })
  
  output$small_world_plot <- renderPlot({
    if (is.null(small_world_sim()))
      return (NULL)
    plot(
      small_world_sim(),
      type = "o",
      ylab = "Percentaget of C",
      xlab = "Generation"
    )
  })
  
  output$scale_free_plot <- renderPlot({
    if (is.null(scale_free_sim())) {
      return (NULL)
    } else{
      plot(
        scale_free_sim(),
        type = "o",
        ylab = "Percentaget of C",
        xlab = "Generation"
      )
    }
  })
})
