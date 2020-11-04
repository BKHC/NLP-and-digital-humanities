library(shiny) 
library(shiny.i18n)

# add translator from json file 
i18n <- Translator$new(translation_json_path = "translation.json")
i18n$set_translation_language("en")

# Define UI for dataset viewer application 
shinyUI(fluidPage( 

    # language selection
    shiny.i18n::usei18n(i18n),
    div( style = "float: right;",
        selectInput('selected_interface_language',
                    i18n$t("Change interface language"),
                    choices = i18n$get_languages(),
                    selected = i18n$get_key_translation())
    ),

    div( style = "float: right;",
        selectInput('selected_model_language',
                    i18n$t("Change model language"),
                    choices = i18n$get_languages(),
                    selected = i18n$get_key_translation())
    ),

    # Application title. 
    titlePanel(i18n$t("*** Morphosyntax Analysis ***")), 
    
    mainPanel( 
        textInput("obs", i18n$t("Please enter your statement:")),  
        helpText(i18n$t("The Shiny App will perform Morphosyntax Analysis on the entered statement and show you the results."), style = "color:blue"),
        helpText(i18n$t("After you enter your statement, please press 'Start the Morphosyntax Analysis Process' below:"), style = "color:blue"),
        
        br(),
        actionButton("goButton", i18n$t("Start the Morphosyntax Analysis Process")),
        br(),

        h5(i18n$t("You have entered the following statement:"), style = "color:blue"),
        br(), 
        textOutput("Original"), 
        br(), 
        h3(i18n$t("The output of Morphosyntax Analysis:"), style = "color:green"), 
        tableOutput("analysis_result") 
    ) 
) 
) 