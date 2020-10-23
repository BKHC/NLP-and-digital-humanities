#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shiny.i18n)
i18n <- Translator$new(translation_json_path = "translation.json")
i18n$set_translation_language("en")

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  shiny.i18n::usei18n(i18n),
  div( style = "float: right;",
       selectInput('selected_language',
                   i18n$t("Change language"),
                   choices = i18n$get_languages(),
                   selected = i18n$get_key_translation())
  ),
  
  titlePanel(i18n$t("*** Lotka Volterra Predator-Prey relations ***")),
  
  sidebarLayout(
    sidebarPanel(
      tabPanel(title="Lotka Volterra Predator-Prey",
               helpText(h3(i18n$t("Set parameters for the L-V predator-prey model"))),
               
               helpText(h4(i18n$t("Set starting population sizes"))),
               numericInput("N",label=p(i18n$t("Select a value for N (starting population of prey)")),value=25,min=1,max=50),
               numericInput("P",label=p(i18n$t("Select a value for P (starting population of predator)")),value=10,min=1,max=25),
               
               helpText(h4(i18n$t("Prey carrying capacity"))),
               radioButtons("Prey_K", label="",
                            choices = list("No" = 1,
                                           "Yes" = 2),
                            selected = 1),
               # If users select "Prey carrying capacity" above, then generate the input option
               htmlOutput("UIpreyk"),
               
               helpText(h4(i18n$t("Set demographic parameters"))),
               sliderInput("r", 
                           label = i18n$t("Choose a value for r (prey intrinsic growth rate)"),
                           min = 0.01, max = 1.99, value=1.0, step = NULL),
               sliderInput("a", 
                           label = i18n$t("Choose a value for a (predation efficiency)"),
                           min = .0001, max = .5, value=.1, step = NULL),
               sliderInput("d", 
                           label = i18n$t("Choose a value for d (predator death rate)"),
                           min = 0.01, max = 1, value=0.6, step = NULL),
               sliderInput("b", 
                           label = i18n$t("Choose a value for b (conversion efficiency)"),
                           min = 0.01, max = 1, value=.5, step = NULL),
               
               numericInput("time",label=i18n$t("Number of time steps to run the model"),value = 100, min=1),
               actionButton("goButton", i18n$t("Go!"))
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(title = "Equations",
                 h4("Model Introduction"),
                 p("The Lotka-Volterra predator-prey model describes the population dynamics of a pair of species interacting as predator and prey. The basic model assumes that the prey population grows exponentially (i.e. without a carrying capacity) in the absence of the predator; in other words, predators are the only control on prey population in the basic model. Predator population growth rate is a function of prey availability, the 'conversion efficiency' of prey into predator (i.e. how many individuals of the prey are needed to make an additional member of the predator population), and some intrinsic death rate."),

                 h4("These are the equations behind the L-V predator prey model"),
                 p(withMathJax("$$ \\frac{dN}{dt} = rN - aNP $$")),
                 p(withMathJax("$$ \\frac{dP}{dt} = baNP - dP $$")),
                 p(withMathJax("Where \\(r\\) is the per capita growth rate of the prey, \\(a\\) is the prey conversion efficiency, \\(b\\) is predation efficiency, and \\(d\\) is predator death rate.")),
                 br(),
                 p("The equilibrium isoclines can be computed with these equations:"),
                 p(withMathJax("$$ \\frac{dN}{dt} = 0 \\text{  when  } P = \\frac{r}{a} $$")),
                 p(withMathJax("$$ \\frac{dP}{dt} = 0 \\text{  when  } N = \\frac{d}{ab}$$")),
                 br(),
                 br(),
                 h4("Model modifications:"),
                 p("A carrying capacity can be introduced onto the prey:"),
                 p(withMathJax("$$ \\frac{dN}{dt} = rN - aNP - \\left(1-\\frac{N}{K} \\right) $$")),
                 p("This changes the equilibrium isocline for the prey:"),
                 p(withMathJax("$$ \\frac{dN}{dt}=0 \\text{  when  } P = \\frac{r}{a}-\\frac{rP}{aK} $$"))
        ),
        
        tabPanel(title = i18n$t("Plots"),
                 h4(i18n$t("Here are plots showing changes in populations")),
                 fluidRow(
                   column(width = 6,
                          plotOutput("plot1", height = 400)),
                   column(width = 6,
                          plotOutput("plot2", height = 400, brush = brushOpts(
                            id = "plot2_brush",
                            resetOnNew = TRUE
                          )))),
                 fluidRow(plotOutput("plot3"))
                 
        ),
        tabPanel(title = "Credit",
                 h4("Developed under the supervision of Dr. Christophe Coupe, The University of Hong Kong")
        )
      )
    )
  )))