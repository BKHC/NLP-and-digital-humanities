library(shiny)
library(shiny.i18n)

# add translator from json file
i18n <- Translator$new(translation_json_path = "translation.json")
i18n$set_translation_language("en")

# Define the interface with the various parameters and the displays
ui <- fluidPage(
  # language selection
  shiny.i18n::usei18n(i18n),
  div(
    style = "float: right;",
    selectInput(
      'selected_language',
      i18n$t("Change language"),
      choices = i18n$get_languages(),
      selected = i18n$get_key_translation()
    )
  ),
  
  titlePanel(i18n$t("Models of Language Change")),
  
  sidebarLayout(
    sidebarPanel(
      h4(i18n$t("Parameters")),
      numericInput(
        inputId = "number_of_nodes",
        label = i18n$t("Number of nodes"),
        value = 500,
        min = 50,
        max = 500,
        step = 1
      ),
      numericInput(
        inputId = "ratio_learner",
        label = i18n$t("Ratio of Learner"),
        value = 0.4,
        min = 0.01,
        max = 1,
        step = 0.01
      ),
      numericInput(
        inputId = "functional_bias",
        label = i18n$t("Functional bias"),
        value = 20,
        min = 1,
        max = 100,
        step = 1
      ),
      numericInput(
        inputId = "number_of_innovator",
        label = i18n$t("Number of adult innovator"),
        value = 1,
        min = 1,
        max = 40,
        step = 1
      ),
      numericInput(
        inputId = "degree",
        label = i18n$t("Mean degree in the network"),
        value = 20,
        min = 1,
        max = 40,
        step = 1
      ),
      numericInput(
        inputId = "rewiring_p",
        label = i18n$t("Rewiring Probability (for Small-world)"),
        value = 0.01,
        min = 0,
        max = 1,
        step = 0.01
      ),
      sliderInput(
        inputId = "number_of_runs",
        label = i18n$t("Number of runs"),
        value = 20,
        min = 1,
        max = 100,
        step = 1
      ),
      #br(),
      h4(i18n$t("Run simulation")),
      actionButton("goButton", i18n$t("Compute!"))
    ),
    
    mainPanel(
      tabsetPanel(
        id = "inTabset",
        tabPanel(
          title = i18n$t("Result"),
          value = "panel1",
          fluidRow(
            column(width = 6, h4(i18n$t("Regular Network")), plotOutput("regular_plot", height = 325)),
            column(width = 6, h4(i18n$t("Random Network")), plotOutput("random_plot", height = 325))
          ),
          fluidRow(
            column(
              width = 6,
              h4(i18n$t("Small-world")),
              plotOutput("small_world_plot", height = 325)
            ),
            column(
              width = 6,
              h4(i18n$t("Scale-free")),
              plotOutput("scale_free_plot", height = 325)
            )
          )
        ),
        tabPanel(
          title = i18n$t("Network Statistics"),
          fluidRow(
            column(
              width = 6,
              height = 325,
              h4(i18n$t("Regular Network")),
              textOutput("regular_max_degree"),
              textOutput("regular_min_degree"),
              textOutput("regular_mean_degree"),
              textOutput("regular_transitivity"),
              textOutput("regular_mean_distance"),
              textOutput("regular_diameter"),
              textOutput("regular_assortativity")
            ),
            column(
              width = 6,
              height = 325,
              h4(i18n$t("Random Network")),
              textOutput("random_max_degree"),
              textOutput("random_min_degree"),
              textOutput("random_mean_degree"),
              textOutput("random_transitivity"),
              textOutput("random_mean_distance"),
              textOutput("random_diameter"),
              textOutput("random_assortativity")
            )
          ),
          fluidRow(br()),
          fluidRow(
            column(
              width = 6,
              height = 325,
              h4(i18n$t("Small-world")),
              textOutput("small_world_max_degree"),
              textOutput("small_world_min_degree"),
              textOutput("small_world_mean_degree"),
              textOutput("small_world_transitivity"),
              textOutput("small_world_mean_distance"),
              textOutput("small_world_diameter"),
              textOutput("small_world_assortativity")
            ),
            column(
              width = 6,
              height = 325,
              h4(i18n$t("Scale-free")),
              textOutput("scale_free_max_degree"),
              textOutput("scale_free_min_degree"),
              textOutput("scale_free_mean_degree"),
              textOutput("scale_free_transitivity"),
              textOutput("scale_free_mean_distance"),
              textOutput("scale_free_diameter"),
              textOutput("scale_free_assortativity")
            )
          )
        ),
        tabPanel(title = i18n$t("Regular Network"),
                 h4(i18n$t("Initial Graph")),
                 fluidRow(plotOutput("regular_graph")),                 p(i18n$t("Square: Innovator")), 
                 p(i18n$t("Circle: Non-innovator")), 
                 p(i18n$t("Orange: Learner")), 
                 p(i18n$t("Blue: Non-learner"))),
        tabPanel(title = i18n$t("Random Network"),
                 h4(i18n$t("Initial Graph")),
                 fluidRow(plotOutput("random_graph")),
                 p(i18n$t("Square: Innovator")), 
                 p(i18n$t("Circle: Non-innovator")), 
                 p(i18n$t("Orange: Learner")), 
                 p(i18n$t("Blue: Non-learner"))),
        tabPanel(title = i18n$t("Small-world"),
                 h4(i18n$t("Initial Graph")),
                 fluidRow(plotOutput(
                   "small_world_graph"
                 )),
                 p(i18n$t("Square: Innovator")), 
                 p(i18n$t("Circle: Non-innovator")), 
                 p(i18n$t("Orange: Learner")), 
                 p(i18n$t("Blue: Non-learner"))
                 ),
        tabPanel(title = i18n$t("Scale-free"),
                 h4(i18n$t("Initial Graph")),
                 fluidRow(plotOutput("scale_free_graph")),
                 p(i18n$t("Square: Innovator")), 
                 p(i18n$t("Circle: Non-innovator")), 
                 p(i18n$t("Orange: Learner")), 
                 p(i18n$t("Blue: Non-learner"))
                 )
      )
    )
  )
)