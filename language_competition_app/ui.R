library(shiny)
library(shiny.i18n)

# add translator from json file 
i18n <- Translator$new(translation_json_path = "translation.json")
i18n$set_translation_language("en")

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

# Define the interface with the various parameters and the displays
ui <- fluidPage(

    # language selection
    shiny.i18n::usei18n(i18n),
    div( style = "float: right;",
        selectInput('selected_language',
                    i18n$t("Change language"),
                    choices = i18n$get_languages(),
                    selected = i18n$get_key_translation())
    ),

  titlePanel(i18n$t("Competition of two linguistic variants on social networks")),
  
  sidebarLayout(
    sidebarPanel(h4(i18n$t("Parameters")),
                 strong(div(i18n$t("Simulation"), style = "color:blue")),
                 numericInput(inputId = "upper_time_limit", label = i18n$t("Time limit"), value = 100, min = 1, max = 1000, step=1),
                 sliderInput("nb_runs", i18n$t("Number of runs (how many simulations with the same values of the parameters)"), 1, 10, 3),
                 #br(),
                 strong(div(i18n$t("Network"), style = "color:blue")),
                 radioButtons("network_type", i18n$t("Type of network"), 
                              c("random" = "er", "small-world" = "sw", "scale-free" = "ba"), 
                              selected = "er"),
                 numericInput(inputId = "num_nodes", label = i18n$t("Number of nodes"), value = 100, min = 1, max = 1000, step=1),
                 #br(),
                 strong(div(i18n$t("Linguistic model"), style = "color:blue")),
                 numericInput(inputId = "a", label = i18n$t("a (exponent in the formula to compute impact)"), value = 1, min = 0, max = 5, step=0.1),
                 numericInput(inputId = "b", label = i18n$t("Functional bias in favor of variant B"), value = 1, min = 0, max = 10, step=0.1),
                 numericInput(inputId = "p_imperfect_learning", label = i18n$t("Probability of imperfect learning"), value = 0, min = 0, max = 1, step=0.01),
                 radioButtons("modify_status_at_birth", i18n$t("Modify status at birth"), 
                              c("yes" = "yes", "no" = "no"), 
                              selected = "no"),
                 #numericInput(inputId = "seed_nb", label = "Initial numbers of adopters", value = 1, min = 1, max = 10, step = 1),
                 #br(),
                 strong(div(i18n$t("Visualization of the network"), style = "color:blue")),
                 sliderInput("which_run", i18n$t("Run to visualize (if more than actual number of runs, show you the last run)"), 1, 10, 1),
                 numericInput("slice", i18n$t("Time step to visualize"), value = 1, min = 1, max = 1000, step = 1),
                 h4(i18n$t("Run simulation")),
                 actionButton("goButton", "Compute!")
    ),
    
    mainPanel(h3(i18n$t("Evolution of the linguistic system")), plotOutput("plot_evolution"), 
              h3(i18n$t("Network")), plotOutput("plot_network"))
  )
)


