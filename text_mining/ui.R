library(shiny)
library(shiny.i18n)
library(wordcloud2)

# add translator from json file
i18n <- Translator$new(translation_json_path = "translation.json")
i18n$set_translation_language("en")

tags$style(type="text/css",
           ".shiny-output-error { visibility: hidden; }",
           ".shiny-output-error:before { visibility: hidden; }"
)

# Define the interface with the various parameters and the displays
ui <- fluidPage(
  # language selection
  shiny.i18n::usei18n(i18n),
  div(
    style = "float: right;display:inline-block",
    selectInput(
      'selected_language',
      i18n$t("Change language"),
      choices = i18n$get_languages(),
      selected = i18n$get_key_translation()
    )
  ),
  div(style = "float: right;display:inline-block",
      selectInput(
        'my.choice',
        i18n$t("Select corpus"),
        choices = c(
          "chopin_awakening",
          "harry_potter",
          "movies_hk",
          "movies_romance",
          "movies_usa",
          "jules_verne",
          "bbc",
          "lord_rings",
          "gothic_and_modern_novels",
          "linkin_park"
        )
      )),
  
  titlePanel(i18n$t("Book Analysis")),
  #the main panel 
  mainPanel(
    tabsetPanel(
      id = "inTabset",
      ### Tab 1 #######
      tabPanel(
        #First tab showing the corpus 
        value = "panel1",
        title = i18n$t("Corpus"),
        ##show corpus
        h4("The various grammatical categories"),
        h5("Possible categories are: "),
        HTML(
          "Open word classes:<ul>
          <li>\"PROPN\": proper name</li>
          <li> \"NOUN\": noun</li>
          <li>\"VERB\": verb</li>
          <li>\"ADJ\": adjective</li>
          <li>\"ADV\": adverb</li>
          <li>\"INTJ\": interjection </li>
          </ul>"
        ),
        HTML(
          "Closed word classes:<ul>
          <li>\"ADP\": adposition</li>
          <li>\"DET\": determinant</li>
          <li>\"PART\": particle</li>
          <li>\"CCONJ\": coordinating conjunction</li>
          <li>\"PRON\": pronoun</li>
          <li>\"AUX\": auxiliary </li>
          <li>\"SCONJ\": subordinating conjunction </li>
          <li>\"NUM\": numeral </li>
          </ul>"
        ),
        
        HTML(
          "Other:<ul>
          <li>\"SYM\": symbol</li>
          <li> \"PUNCT\": puncutation</li>
          <li>\"X\": unclassified</li>
          </ul>"
        ),
        
        HTML(
          "We use these categories for various purposes, and especially to select some categories of words for our analyses and figures."
        ),
        
        h4(i18n$t("Display the first 20 rows")),
        tableOutput("content")
        ),
      
      #Second tab showing the two levels of classifications 
      tabPanel(
        value = "panel2",
        title = i18n$t("Two levels of classification"),
        h4("1st-level and 2nd-level categories in the documents"),
        
        h5("What are the categories at the two levels of classification?"),
        p(
          "This will depend on the corpus you choose to work with.For example, for the Harry Potter novels, the first level will correspond to the 7 books, and the second level to the chatpers in these 7 books.
          For example, for the movie plots, the first level will correspond to the genre of the movies (action, comedy etc.), and the second lebel to the individual plots of the movies
          You may conduct interesting analyses at either of these two levels.
          These two levels of classificationis will be useful to select some subsets of text data, and to specify at which level you want to investigate the content of the documents
          "
        ),
        HTML(
          "Beware:<ul><li>If your second level of classifications contains hundreds or even thousands of categories, trying to display them all will likely create problems. Some analyses should therefore sometimes be restricted to the first level of classification </li>
          </ul>"
        ),
        
        h5("1st Level"),
        verbatimTextOutput("class_1"),
        # Each category is associated with a number of text tokens (words, but also punctation marks)
        
        h5("2nd Level"),
        verbatimTextOutput("class_2") # Each document is associated with a number of text tokens (words, but also punctation marks)
        ),
      
      #Third tab showing the categories and terms  
      tabPanel(
        value = "panel3",
        title = i18n$t("Categories/ Terms"),
        h3("Looking for specific grammatical categories or terms"),
        p(
          "A lot of different options of analysis are available once the morphosyntactic analysis has been conducted. Let's investigate some of them..."
        ),
        p("Looking for specific grammatical categories or terms"),
        h5("How to select a set of documents to perform the analysis?"),
        p(
          "For many functions below, you will have the option to select a subset of documents to perform the analysis. You can select at the first level of the classification, and at the second one."
        ),
        uiOutput('categores_1_choices'),
        h5("Defining stopwords"),
        p(
          "This is a list of stopwords to be excluded from analysis, as they are not very informative, you can add your own words if you want"
        ),
        selectizeInput(
          "my.stop.words",
          "Stop words",
          multiple = TRUE,
          options = list(create = TRUE),
          choices = c(
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
          ),
          selected = c(
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
        ),
        br(),
        h4("1. Select a upos category to investigate"),
        
        selectizeInput(
          "upos.category",
          "Upos Category",
          multiple = TRUE, 
          choices = c(
            "PROPN",
            "NOUN",
            "VERB",
            "ADJ",
            "ADV",
            "INTJ",
            "ADP",
            "DET",
            "PART",
            "CCONJ",
            "PRON",
            "AUX",
            "SCONJ",
            "NUM",
            "SYM",
            "PUNCT",
            "X"
          )
        ),
        selectizeInput("see.as",
                       "Seen As",
                       choices = c("token", "lemma")),
        actionButton("goButton1", i18n$t("Press to show results")),
        verbatimTextOutput("analysis"),
        br(),
        h4("2. Select a term to see its occurrences"),
        selectInput("target.term",
                    "Target term",
                    choices = c("token", "lemma")),
        h5("Define the term you want to investigate:"),
        textInput("checked.term", "Term", "word"),
        #br(),
        actionButton("goButton2", i18n$t("Press to show results")),
        verbatimTextOutput("analysis2"),
        br(),
        p("When investigating the ouput of the morphosyntacic parsing, we can see some errors. One option would be to analyse these mistakes carrefully, then correct the initial text before performing the morphosyntactic analysis. Such mistakes do not always mean that the involved lemmas will not correspond to the related tokens. If the number of mistakes is low, the impact will be limited on the analyses which at least partly rest on morphosyntactic processing (for those which do not rely on it, no problem anyway). Let's look at the tokens (and the sentence in which they appear) which could not be classified in the selected set of documents
."),
        h4("3. Errors"), 
        verbatimTextOutput("analysis3"),
        verbatimTextOutput("analysis4")

      ),
      
      #Forth tab showing the part-of-speech distribution  
      tabPanel(
        value = "panel4",
        title = i18n$t("Part-Of-Speech Distribution"),
        h3(i18n$t("Distribution of the POS (Part-Of-Speech)")),
        h5("Selecting a subset of text to perform the analysis"),
        uiOutput('categores_1_choices_pos'),
        plotOutput("pos1"),
        plotOutput("pos2"), 
        textOutput("closed.words.pct"),
        textOutput("open.words.pct"),
        textOutput("other.pct"),
        br()
      ),
      
      #Fifth tab showing the Measures of lexical richness 
      tabPanel(
        value = "panel5",
        title = i18n$t("Measures of lexical richness"),
        h3(i18n$t("Distribution of the POS (Part-Of-Speech)")),
        h5("Selecting a subset of text to perform the analysis"),
        uiOutput("categores_1_choices_richness"),
        selectizeInput(
          "upos.category_richness",
          "Select the upos categories to investigate",
          multiple = TRUE, 
          choices = c(
            "PROPN",
            "NOUN",
            "VERB",
            "ADJ",
            "ADV",
            "INTJ",
            "ADP",
            "DET",
            "PART",
            "CCONJ",
            "PRON",
            "AUX",
            "SCONJ",
            "NUM",
            "SYM",
            "PUNCT",
            "X"
          )
        ),
        selectizeInput(
          "target.term_richness",
          "Do you want to assess lexical richness at the level of tokens or of lemmas?",
          choices = c("token", "lemma"
          )
        ),
        selectizeInput(
          "results.classification",
          "At which level of classification do you want to see the results?",
          choices = c("class_1", "class_2"
          )
        ),
        h4("Result"),
        plotOutput("richness")
        
      ),
      
      #Sixth tab showing Frequency of occurrence
      tabPanel(
        value = "panel6",
        title = i18n$t("Frequency of occurrence"),
        h3(i18n$t("Selecting a subset of text to perform the analysis")),
        h5("Selecting a subset of text to perform the analysis"),
        uiOutput("categores_1_choices_frequency"),
        selectizeInput(
          "upos.category_frequency",
          "Select the upos categories to investigate",
          multiple = TRUE, 
          choices = c(
            "PROPN",
            "NOUN",
            "VERB",
            "ADJ",
            "ADV",
            "INTJ",
            "ADP",
            "DET",
            "PART",
            "CCONJ",
            "PRON",
            "AUX",
            "SCONJ",
            "NUM",
            "SYM",
            "PUNCT",
            "X"
          )
        ),
        selectizeInput(
          "my.stop.words_frequency",
          "Stop words",
          multiple = TRUE,
          options = list(create = TRUE),
          choices = c(
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
          ),
          selected = c(
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
        ),
        selectizeInput("see.as_frequency",
                       "Seen As",
                       choices = c("token", "lemma")),
        numericInput(
          inputId = "nb.words.barplot",
          label = i18n$t("How many words do you want to see in the barplot"),
          value = 40,
          min = 1,
          max = 100,
          step = 1
        ),
        numericInput(
          inputId = "nb.words.wordcloud",
          label = i18n$t("How many words do you want to see in the word cloud"),
          value = 10,
          min = 50,
          max = 200,
          step = 1
        ),
        p("Plotting an histogram of the frequencies of occurrences of the selected lemma. This is to confirm that the distribution will be heavily skewed: there are only a few very frequent lemmas, and many rarely occurring ones."),
        plotOutput("frequency1"),
        h5("A barplot to  assess the frequency of occurrence of the most frequent lemmas"), 
        plotOutput("frequency2"),
        h5("Word Cloud"),
        actionButton("goButton3", i18n$t("Show/ Refresh Word Cloud")),
        wordcloud2Output("frequency3")
      ),
      
      #Seventh tab showing Collocations
      tabPanel(
        value = "panel7",
        title = i18n$t("Collocations"),
        h4("Collocations and network of collocations"),
        p(i18n$t("Collocations are pairs (or triplets, quadruplets etc.) of terms which appear together more than what chance alone would predict. There are different ways to assess this and choose collocations. Here, we rely on a statistical approach named Log-frequence biased MD (LFMD)")),
        h5("Selecting a subset of text to perform the analysis"),
        uiOutput("categores_1_choices_collocation"),
        selectizeInput(
          "upos.category_collocation",
          "Select the upos categories to investigate",
          multiple = TRUE, 
          choices = c(
            "PROPN",
            "NOUN",
            "VERB",
            "ADJ",
            "ADV",
            "INTJ",
            "ADP",
            "DET",
            "PART",
            "CCONJ",
            "PRON",
            "AUX",
            "SCONJ",
            "NUM",
            "SYM",
            "PUNCT",
            "X"
          )
        ),
        selectizeInput(
          "my.stop.words_frequency",
          "Stop words",
          multiple = TRUE,
          options = list(create = TRUE),
          choices = c(
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
          ),
          selected = c(
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
        ),
        numericInput(
          inputId = "nb.gram",
          label = i18n$t("This defines the largest possible collocations"),
          value = 2,
          min = 1,
          max = 10,
          step = 1
        ),
        numericInput(
          inputId = "nb.collocations.displayed",
          label = i18n$t("How many collocations you want to see"),
          value = 30,
          min = 1,
          max = 100,
          step = 1
        ),
        numericInput(
          inputId = "nb.collocations.displayed.network",
          label = i18n$t("How many cooccurrences you want to see in the network"),
          value = 200,
          min = 100,
          max = 500,
          step = 1
        ),
        h5("Barplot to display the most meaningful collocations"),
        plotOutput("collocations1"), 
        h5("We can also prepare a network graph with the strongest collocations"),
        plotOutput("collocations2")
      ),
      
      #Eighth tab showing Co-occurrence
      tabPanel(
        value = "panel8",
        title = i18n$t("Co-occurrence"),
        h4(i18n$t("Co-occurrences and network of co-occurrences")),
        p(i18n$t("Cooccurrences differ from collocations in the sense that we just look at how frequently two terms appear together, without further statistical assessment")),
        h5("Selecting a subset of text to perform the analysis"),
        uiOutput("categores_1_choices_cooccurrence"),
        selectizeInput(
          "upos.category_cooccurrence",
          "Select the upos categories to investigate",
          multiple = TRUE, 
          choices = c(
            "PROPN",
            "NOUN",
            "VERB",
            "ADJ",
            "ADV",
            "INTJ",
            "ADP",
            "DET",
            "PART",
            "CCONJ",
            "PRON",
            "AUX",
            "SCONJ",
            "NUM",
            "SYM",
            "PUNCT",
            "X"
          )
        ),
        selectizeInput(
          "my.stop.words_cooccurrence",
          "Stop words",
          multiple = TRUE,
          options = list(create = TRUE),
          choices = c(
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
          ),
          selected = c(
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
        ),
        numericInput(
          inputId = "nb.gram_cooccurence",
          label = i18n$t("This defines a window around a woord to look for other occurring words (different values, different graphs)"),
          value = 4,
          min = 1,
          max = 10,
          step = 1
        ),
        numericInput(
          inputId = "nb.cooccurrences.displayed.network",
          label = i18n$t("How many cooccurrences you want to see in the network"),
          value = 200,
          min = 100,
          max = 500,
          step = 1
        ),
        h5("A network graph with the most frequent cooccurrences"),
        plotOutput("cooccurrence1")
      ),
      
      #Nineth tab showing Sentiment Analysis
      tabPanel(
        value = "panel9",
        title = i18n$t("Sentiment Analysis"),
        h4(i18n$t("Sentiment Analysis")),
        p(i18n$t("Works well if there are not too many categories in the level of classification you want to investigate (i.e. beware if your select class_2 that you don't have too many categories")),
        h5("Selecting a subset of text to perform the analysis"),
        uiOutput("categores_1_choices_sentiment"),
        selectizeInput(
          "results.classification_sentiment",
          "At which level of classification do you want to see the results?",
          choices = c("class_1", "class_2"
          )
        ),
        actionButton("goButton_sentiment", i18n$t("Compute")),
        
        h5("Let's look at the most positive and negative sentences..."),
        verbatimTextOutput("sentiment1"),
        verbatimTextOutput("sentiment2"),
        p("What are the positive and negative words in the most positive sentences? It is not easy to guess which words contribute to the overal valence or to specific sentiments. We can, however, extract them and display them."),
        verbatimTextOutput("sentiment3"),
        verbatimTextOutput("sentiment4"),
        br(), 
        p("Choose a sentiment to find the most extreme sentences with respect to it"), 
        selectizeInput(
          "my.emotion",
          multiple = TRUE,
          "At which level of classification do you want to see the results?",
          choices = c("anger", "anticipation", "disgust", "fear", "joy", "sadness", "surprise", "trust"
          ),
          selected = "anger"
        ),
        verbatimTextOutput("sentiment5"), 
        p("What are the words explaining the high level of the selected emotion?"), 
        verbatimTextOutput("sentiment6"),
        p("Displaying the sentiments"),
        p("First displaying sentiment valence. If there is only 1 category to display, you will see only 1 bar (not very informative, since comparisons are what matter)"), 
        plotOutput("sentiment7"), 
        br(), 
        h4("Display sentiments by category"), 
        p("If there is only 1 category to display, you will see only 1 bar for each sentiment (not very informative, since comparisons are what matter)"),
        plotOutput("sentiment8"), 
        p("If there is only 1 category to display, this figure won't be shown properly"), 
        plotOutput("sentiment9"), 
        h4("Sentiment analysis through the sentences of the document"), 
        p("BEWARE: Only makes sense if the documents are ordered"), 
        #plotOutput("sentiment10"), 
        p("We don't see anything... it's \"moving too fast\" = values are too different from one sentence to the next"), 
        p("We need to smoothen the curves, that is look a the evolution with a sliding window of several sentences"),
        p("Choose the amount of smoothing"), 
        p("The higher the number, the more complex the curves will be"), 
        p("There is no \"right\" value, and it much depends on the size of your corpus - try different values!"), 
        numericInput(
          inputId = "my.smoothing",
          label = i18n$t("Smoothing"),
          value = 70,
          min = 1,
          max = 1000,
          step = 1
        ),
        p("Display positive and negative sentiments, plotting the graph with smoothed values."), 
        plotOutput("sentiment11"), 
        
        p("Display specific sentiments"), 
        plotOutput("sentiment12")
      ),
      
      #Tenth tab showing Dispersion of terms
      tabPanel(
        value = "panel10",
        title = i18n$t("Dispersion of terms"), 
        h4("Only makes sense if your categories are ordered"),
        h4("Selecting a subset of text to perform the analysis"),
        p("At the first level of classification, which groups do you want to keep?"),
        uiOutput('categores_1_choices_dis'),
        p("At the second level of classification, which groups do you want to keep?"),
        uiOutput('categores_2_choices_dis'),
        
        selectizeInput(
          "upos.categories_dis",
          "Select the upos categories to investigate",
          multiple = TRUE, 
          choices = c(
            "PROPN",
            "NOUN",
            "VERB",
            "ADJ",
            "ADV",
            "INTJ",
            "ADP",
            "DET",
            "PART",
            "CCONJ",
            "PRON",
            "AUX",
            "SCONJ",
            "NUM",
            "SYM",
            "PUNCT",
            "X"
          )
        ),
        selectizeInput(
          "results.classification_dis",
          "At which level of classification do you want to see the results?",
          choices = c("class_1", "class_2"
          )
        ),
        selectizeInput(
          "display.most.frequent",
          "At which level of classification do you want to see the results?",
          choices = c("yes", "no"
          ), 
          selected = "no"
        ),

        numericInput(
          inputId = "nb.displayed.most.frequent.lemmas",
          label = i18n$t("If you chose \"yes\" above, you can choose how many most frequent lemmas you want to see (don't ask for too many)"),
          value = 10,
          min = 1,
          max = 20,
          step = 1
        ),
        
        selectizeInput(
          "my.own.lemmas",
          "If you chose \"yes\" above, you can choose how many most frequent lemmas you want to see (don't ask for too many)",
          multiple = TRUE,
          options = list(create = TRUE),
          choices = c("Nautilus", "sun"),
          selected = c("Nautilus", "sun")
        ),
        h4("Plotting the dispersion of the target lemmas throughout the documents"),
        plotOutput("dispersion")
        
        
      ),
      
      #Eleventh tab showing Topic modelling
      tabPanel(
        value = "panel11",
        title = i18n$t("Topic modelling"), 
        h4("Topic modelling (with Latent Dirichlet Analysis or LDA)"),
        h4("Selecting a subset of text to perform the analysis"), 
        
        p("At the first level of classification, which groups do you want to keep?"),
        uiOutput('categores_1_choices_tm'),
        p("At the second level of classification, which groups do you want to keep?"),
        uiOutput('categores_2_choices_tm'),
        
        
        selectizeInput(
          "upos.categories_tm",
          "Select the upos categories to investigate",
          choices = c(
            "PROPN",
            "NOUN",
            "VERB",
            "ADJ",
            "ADV",
            "INTJ",
            "ADP",
            "DET",
            "PART",
            "CCONJ",
            "PRON",
            "AUX",
            "SCONJ",
            "NUM",
            "SYM",
            "PUNCT",
            "X"
          ),
          selected = c("NOUN", "VERB", "ADJ"), 
          multiple = TRUE
        ),
        
        selectizeInput(
          "results.classification_tm",
          "At which level of classification do you want to see the results?",
          choices = c("class_1", "class_2"
          )
        ),

        h4("Below are the parameters for the LDA algorithm (more precisely for the Gibbs sampling). You can play with them, but beware... The algorithm is complex..."), 
        
        numericInput(
          inputId = "burnin",
          label = i18n$t("burnin"),
          value = 4000,
          min = 1,
          max = 10000,
          step = 100
        ),
        numericInput(
          inputId = "iter",
          label = i18n$t("iter"),
          value = 2000,
          min = 1,
          max = 5000,
          step = 100
        ),
        
        numericInput(
          inputId = "thin",
          label = i18n$t("thin"),
          value = 2000,
          min = 1,
          max = 5000,
          step = 100
        ),
        
        numericInput(
          inputId = "nstart",
          label = i18n$t("nstart"),
          value = 5,
          min = 1,
          max = 20,
          step = 1
        ),
        
        selectizeInput(
          "best",
          "best",
          choices = c(TRUE, FALSE
          ),
          selected = TRUE
        ),

        numericInput(
          inputId = "search.from",
          label = i18n$t("search.from"),
          value = 2,
          min = 1,
          max = 50,
          step = 1
        ),
        
        numericInput(
          inputId = "search.to",
          label = i18n$t("search.to"),
          value = 15,
          min = 1,
          max = 50,
          step = 1
        ),
        
        p("you may decrease search.to if looking for the right number of topics takes too much time, these two values define the range of numbers of topics that is being investigated"), 

        br(),
        p("How many lemmas with high tf-idf scores do you want to keep to look for topics? The less you keep, the faster the topic-modelling algorithms will run"), 
        numericInput(
          inputId = "nb.selected.lemmas",
          label = i18n$t("nb.selected.lemmas"),
          value = 2000,
          min = 1,
          max = 50000,
          step = 100
        ),
        p("We plot various curves which can be used to select the right number of topics"),
        p("A good number of topics should minimize the values of the two metrics on the upper graph"),
        p("And maximize the values of the two metrics on the lower graph"),
        plotOutput("tm"),
        p("It is often not easy to find the best value, because the different metrics usually tell (slightly or not) different stories..."), 
        br(), 
        p("Choosing the number of topics (given your analysis of the previous graph)"), 
        numericInput(
          inputId = "nb.topics",
          label = i18n$t("nb.topics"),
          value = 7,
          min = 1,
          max = 50,
          step = 1
        ),
        h4("Plotting the graph for each topic"),
        plotOutput("tm2"),
        
        br(),
        h4("Let's now draw the probabilities of the different topics for each category"),
        h4("BEWARE : This will be ok only if there are not too many categories at the selected level of classification..."), 
        plotOutput("tm3")
        )
      ),
    style = 'width: 100%'
    #style='width: 1000px; height: 1000px'
      )
  )