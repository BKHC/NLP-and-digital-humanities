
# Loading some pakages to access their functions ####
# If they have not yet be downloaded, you need to do that first!

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


# Two useful functions to draw figures
reorder.within <- function(x, by, within, fun = mean, sep = "___", ...) {
  new.x <- paste(x, within, sep = sep)
  stats::reorder(new.x, by, FUN = fun)
}

scale.x.reordered <- function(..., sep = "___") {
  reg <- paste0(sep, ".+$")
  ggplot2::scale_x_discrete(labels = function(x) gsub(reg, "", x), ...)
}

# A function to select a subset of documents according to several criteria:
# - a choice of categories at a first level of classification (by default all categories are selected)
# - a choice of categories at a second level of classification (by default all categories are selected)
# - a set of syntactic categories (by defaut all categories are selected)
# - a set of words/tokens to exclude (by default no token is excluded)

select.subset <- function(content, categories.1 = "all", categories.2 = "all", upos.categories = NULL, stop.words = NULL) {
  if (categories.1 != "all" && any(! categories.1 %in% unique(content %>% pull(class_1))))
    stop("Beware! At least one of your target categories at the first level of classification does not appear in the actual list of occuring categories")
  
  if (categories.2 != "all" && any(! categories.2 %in% unique(content %>% pull(class_2))))
    stop("Beware! At least one of your target categories at the second level of classification does not appear in the actual list of occuring categories")
  
  selected.content <- content
  
  if (! is.null(upos.categories))
    selected.content <- selected.content %>% filter(upos %in% upos.categories)
  if (! is.null(stop.words))
    selected.content <- selected.content %>% filter(! lemma %in% stop.words)
  
  if (length(categories.1) != 1 || categories.1 != "all")
    selected.content <- selected.content %>% filter(class_1 %in% categories.1)
  
  if (length(categories.2) != 1 || categories.2 != "all")
    selected.content <- selected.content %>% filter(class_2 %in% categories.2)
  
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
extract.positions.categories <- function(sentences, results.classification) {
  my.classif <- sentences %>% pull(results.classification)
  category.limits <- which(my.classif != lead(my.classif,1)) + 1
  category.labels <-  as.character(my.classif[category.limits - 1])
  category.limits <- c(category.limits, length(my.classif))
  category.labels <- c(category.labels, as.character(my.classif[length(my.classif)]))
  
  my.limits <- data.frame(index=category.limits, present = rep(0,length(category.limits)), labels = category.labels, stringsAsFactors = F)

  return(my.limits)
}


# ********************************
# Choosing the corpus to load ####
# ********************************

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# Select the corpus you want to study by dropping the # in front of it

#my.choice <- "chopin_awakening"
#my.choice <- "harry_potter"
#my.choice <- "movies_hk"
#my.choice <- "movies_romance"
#my.choice <- "movies_usa"
#my.choice <- "jules_verne"
#my.choice <- "bbc"
#my.choice <- "lord_rings"
#my.choice <- "gothic_and_modern_novels"
my.choice <- "linkin_park"

# Select the working directory which contains the RData files

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
current = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dir = paste0(current, "/parsed_data")) # Setting the working directory
content <- readRDS(paste0(my.choice, ".rds")) # Loading the data
str(content) # Let's take a look at the content of the data structure
head(content, n=20) # Display the first 20 rows

# **************************************
# The various grammatical categories ####
# **************************************

# Possible categories are: 
# Open word classes:
#   "PROPN": proper name
#   "NOUN": noun
#   "VERB": verb
#   "ADJ": adjective
#   "ADV": adverb
#   "INTJ": interjection 

# Closed word classes:
#   "ADP": adposition
#   "DET": determinant
#   "PART": particle 
#   "CCONJ": coordinating conjunction
#   "PRON": pronoun
#   "AUX": auxiliary
#   "SCONJ": subordinating conjunction
#   "NUM": numeral

# Other:
#   "SYM": symbol
#   "PUNCT": puncutation
#   "X": unclassified

# We use these categories below for various purposes, and especially to select some categories of words for our analyses and figures

# ********************************************************
# 1st-level and 2nd-level categories in the documents ####
# ********************************************************

# What are the categories at the two levels of classification?
# This will depend on the corpus you choose to work with.
# For example, for the Harry Potter novels, the first level will correspond to the 7 books, and the second level to the chatpers in these 7 books.
# For example, for the movie plots, the first level will correspond to the genre of the movies (action, comedy etc.), and the second lebel to the individual plots of the movies
# You may conduct interesting analyses at either of these two levels.
# These two levels of classificationis will be useful to select some subsets of text data, and to specify at which level you want to investigate the content of the documents

# Beware: If your second level of classifications contains hundreds or even thousands of categories, trying to display them all will likely create problems... 
# Some analyses should therefore sometimes be restricted to the first level of classification 

# 1st level
summary(content$class_1) # Each category is associated with a number of text tokens (words, but also punctation marks)

# 2nd level
summary(content$class_2) # Each document is associated with a number of text tokens (words, but also punctation marks)


# **************************************************************
# How to select a set of documents to perform the analysis? ####
# **************************************************************

# For many functions below, you will have the option to select a subset of documents to perform the analysis
# You can select at the first level of the classification, and at the second one
# At either level, you can select in three different ways:

# Selecting a single element
# categories.1 <- "comedy"
# Note that categories.1 <- c("comedy") will work fine too!

# Selecting two or more elements
# categories.1 <- c("comedy", "action") # you can choose one element, two, three

# Keeping all elements
# categories.1 <- "all"

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


# ***********************
# Defining stopwords ####
# ***********************
# This is a list of stopwords to be excluded from analysis, as they are not very informative
# You can add your own words if you want
my.stop.words <- c("be", "have", "do", "would", "will", "can", "know", "see", "may", "should", "get",
                   "think", "go", "make", "must", "take", "come", "say", "put", "want", "look", "find",
                   "need", "like", "set", "use", "become", "let", "feel", "try", "shall", "ask", "seem",
                   "same", "most", "few", "other", "own", "many")

# *************
# Analyses ####
# *************

# A lot of different options of analysis are available once the morphosyntactic analysis has been conducted. 
# Let's investigate some of them...



# * Looking for specific grammatical categories or terms ####

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# Selecting a subset of text to perform the analysis

# At the first level of classification, which groups do you want to keep?
categories.1 <- "all" # you can choose one element, two, three
# At the second level of classification, which groups do you want to keep?
categories.2 <- "all" 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

selected.content <- select.subset(content, categories.1, categories.2) # Selecting a subset of data

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# Select a upos category to investigate
# Possible categories are: PROPN, NOUN, VERB, ADJ, ADV, INTJ", ADP, DET, PART, CCONJ, PRON, AUX, SCONJ, NUM, SYM, PUNCT, X
upos.category <- "VERB"
see.as <- "token" # you can choose "token" or "lemma"
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

selected.content %>% filter(upos == upos.category) %>% pull(see.as) %>% unique()
selected.content

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# Select a term to see its occurrences
# First, do you want to select a lemma or a token (i.e. an inflected form)?
target.term <- "lemma" # you can choose "token" or "lemma"

# Define the term you want to investigate:
checked.term <- "word"
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

selected.content %>% filter(get(target.term) == checked.term) %>% pull(sentence)

# When investigating the ouput of the morphosyntacic parsing, we can see some errors.
# One option would be to analyse these mistakes carrefully, then correct the initial text before performing the morphosyntactic analysis
# Such mistakes do not always mean that the involved lemmas will not correspond to the related tokens
# If the number of mistakes is low, the impact will be limited on the analyses which at least partly 
# rest on morphosyntactic processing (for those which do not rely on it, no problem anyway)

# Let's look at the tokens (and the sentence in which they appear) which could not be classified in the selected set of documents

selected.content %>% filter(upos == "X") %>% pull(token)
selected.content %>% filter(upos == "X") %>% pull(sentence) %>% unique()


# * Distribution of the POS (Part-Of-Speech) ####

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# Selecting a subset of text to perform the analysis

# At the first level of classification, which groups do you want to keep?
categories.1 <- "all" # you can choose one element, two, three
# At the second level of classification, which groups do you want to keep?
categories.2 <- "all"
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

selected.content <- select.subset(content, categories.1, categories.2) # Selecting a subset of data

stats.upos <- txt_freq(selected.content$upos) # This extracts the frequencies of the different upos category
# stats.upos # seeing the results as a table...
max.pct <- max(stats.upos$freq_pct) + 15 # This is to prepare a nice figure

# Graphic for the frequencies of the different grammatical categories
ggplot(stats.upos, aes(x=reorder(key, freq_pct), y=freq_pct)) +
  geom_bar(stat="identity", fill='white', color="grey50") +
  coord_flip(ylim = c(0, max.pct)) +
  scale_y_continuous(labels = scales::number_format()) + 
  labs(x="Grammatical categories", y="Frequency of occurrence (in % of the number of tokens)") +
  geom_text(aes(label=round(freq_pct, 2)), hjust = -0.5, size=8) +
  theme_grey(base_size = 18)

# Graphic focusing on open words versus closed words 
(open.words.pct <- sum(stats.upos[stats.upos$key %in% c("VERB", "NOUN", "PROPN", "ADJ", "ADV", "INTERJ"), "freq_pct"]))
(closed.words.pct <- sum(stats.upos[stats.upos$key %in% c("ADP", "DET", "PART", "CCONJ", "PRON", "AUX", "SCONJ", "NUM"), "freq_pct"]))
(other.pct <- sum(stats.upos[stats.upos$key %in% c("SYM", "PUNCT", "X"), "freq_pct"]))

df <- tibble(key=c("open class words", "closed class words", "other (PUNCT, ...)"), 
             freq_pct=c(open.words.pct, closed.words.pct, other.pct))
df %>% mutate(Corpus = "") %>%
ggplot(aes(x=Corpus, y=freq_pct, fill=key)) +
  geom_bar(stat="identity", color="grey50") +
  labs(x="", y="Frequency of occurrence") +
  theme_grey(base_size = 18)



# * Measures of lexical richness ####

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# Selecting a subset of text to perform the analysis

# At the first level of classification, which groups do you want to keep?
categories.1 <- "all" # you can choose one element, two, three
# At the second level of classification, which groups do you want to keep?
categories.2 <- "all"

# Select the upos categories to investigate
# Possible categories are: PROPN, NOUN, VERB, ADJ, ADV, INTJ", ADP, DET, PART, CCONJ, PRON, AUX, SCONJ, NUM, SYM, PUNCT, X
upos.categories <- c("VERB", "NOUN", "ADJ", "PROPN", "ADV")

# Do you want to assess lexical richness at the level of tokens or of lemmas?
target.term <- "token" # you can choose "token" or "lemma"

# At which level of classification do you want to see the results?
results.classification <- "class_1" # class_1 or class_2
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

selected.content <- select.subset(content, categories.1, categories.2, upos.categories) # Selecting a subset of data

# We need to prepare a document-term matrix to feed it to the function we are interested in:
content.tmp <- document_term_frequencies(selected.content %>% select(results.classification, target.term))
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

# Plotting the TTR for the categories at the selected level of classification
ggplot(data=my.diversity.measures, aes(x=category, y=value)) +
  geom_bar(stat="identity", position=position_dodge()) + facet_wrap(~ variable, scales = "free") +
  coord_flip() + theme_grey(base_size = 14) + ggtitle(paste0("TTR for ", paste(upos.categories, collapse=", ")))


# * Frequency of occurrence of the most frequent lemmas (barplots and word clouds) ####

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# Selecting a subset of text to perform the analysis

# At the first level of classification, which groups do you want to keep?
categories.1 <- "all" # you can choose one element, two, three
# At the second level of classification, which groups do you want to keep?
categories.2 <- "all"

# Choosing upos categories and stop words
upos.categories <- c("NOUN", "ADJ", "VERB") # Here you can choose the syntactic categories you want to focus on

# Here you can choose words you don't want to consider
nb.words.barplot <- 40 # How many words do you want to see in the barplot
nb.words.wordcloud <- 100 # How many words do you want to see in the word cloud
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

selected.content <- select.subset(content, categories.1, categories.2, upos.categories, my.stop.words) # Selecting a subset of data

# Let's find the most frequent lemmas in the subset of documents:
nb.lemmas <- txt_freq(selected.content$lemma, order=T)
nb.lemmas$key <- factor(nb.lemmas$key, levels = rev(nb.lemmas$key))

nrow(nb.lemmas) # The number of different lemmas

# Plotting an histogram of the frequencies of occurrences of the selected lemma
# This is to confirm that the distribution will be heavily skewed: there are only a few very frequent lemmas, and many rarely occurring ones...
hist(nb.lemmas$freq, breaks=100)

# Let's create a barplot to better assess the frequency of occurrence of the most frequent lemmas
ggplot(nb.lemmas[1:nb.words.barplot,], aes(x=reorder(key, freq), y=freq)) +
  geom_bar(stat="identity", fill='white', color="grey50") +
  coord_flip(ylim = c(0, max(nb.lemmas$freq)*1.2)) +
  labs(x="Most frequent lemmas", y="Frequency") +
  geom_text(aes(label=round(freq,2)), size=3, hjust=-0.5) + theme_grey(base_size = 12)

# Let's now create a word cloud to visualize things a bit differently
wordcloud2(data = head(nb.lemmas, n=nb.words.wordcloud), size = 0.7) # changing size will draw words bigger/smaller (one has to adjust to the window size)



# * Collocations and network of collocations ####

# Collocations are pairs (or triplets, quadruplets etc.) of terms which appear together more than what chance alone would predict
# There are different ways to assess this and choose collocations. Here, we rely on a statistical approach named Log-frequence biased MD (LFMD)

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# Selecting a subset of text to perform the analysis

# At the first level of classification, which groups do you want to keep?
categories.1 <- "all" # you can choose one element, two, three
# At the second level of classification, which groups do you want to keep?
categories.2 <- "all"

# Choosing upos categories and stop words
upos.categories <- c("NOUN", "ADJ", "VERB") # Here you can choose the syntactic categories you want to focus on
# Here you can choose words you don't want to consider

nb.gram <- 2 # This defines the largest possible collocations
nb.collocations.displayed <- 30 # How many collocations you want to see
nb.collocations.displayed.network <- 200 # How many cooccurrences you want to see in the network

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

selected.content <- select.subset(content, categories.1, categories.2, upos.categories, my.stop.words) # Selecting a subset of data

# We first call the function keywords_collocation which extract collocations 
my.collocations <- keywords_collocation(selected.content, term = "lemma", group = "class_1", ngram_max = nb.gram)
my.collocations <- my.collocations[order(-my.collocations$lfmd),] # we sort the collocations in decreasing order according to LFMD
my.collocations$keyword <- factor(my.collocations$keyword, levels = rev(my.collocations$keyword)) # We create a factor

# Simple text display the strongest collocations:
head(my.collocations, n=nb.collocations.displayed) # The combinations with the highest "Log-frequence biased MD"

# A nicer barplot to display the most meaningful collocations
ggplot(my.collocations[1:nb.collocations.displayed,], aes(x=keyword, y=lfmd)) +
  geom_bar(stat="identity", fill='white', color="grey50") +
  coord_flip(ylim = c(min(my.collocations[1:nb.collocations.displayed,"lfmd"]), 0)) +
  labs(x=paste0(nb.collocations.displayed, " most relevant combinations"), y="Log-Frequency biased MD (LFMD)") +
  geom_text(aes(label=round(lfmd,2)), size=3, hjust=-0.5) + theme_grey(base_size = 12)


# We can also prepare a network graph with the strongest collocations
tmp <- my.collocations[,c("left", "right", "lfmd")] # We transform the format of the data to feed them to the function creating the components of the graph
colnames(tmp) <- c("term1", "term2", "lfmd")
collocations.network <- graph_from_data_frame(head(tmp, nb.collocations.displayed.network)) # Builds the graph

# Plotting the network:
ggraph(collocations.network, layout = "nicely") +
  geom_edge_link(aes(edge_alpha = lfmd), edge_colour = "red", linejoin = "round",
                 check_overlap = TRUE, lineend="round", width=1.5) +
  geom_node_text(aes(label = name), col = "gray20", size=4) +
  labs(x="", y="") +  theme_void() + theme(legend.position = "none") +
  labs(title = "Collocations within document(s) (according to lfmd)")



# * Co-occurrences and network of co-occurrences ####

# Cooccurrences differ from collocations in the sense that we just look at how frequently two terms appear together, without further statistical assessment

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# Selecting a subset of text to perform the analysis

# At the first level of classification, which groups do you want to keep?
categories.1 <- "all" # you can choose one element, two, three
# At the second level of classification, which groups do you want to keep?
categories.2 <- "all"

# Select the upos categories to investigate
# Possible categories are: PROPN, NOUN, VERB, ADJ, ADV, INTJ", ADP, DET, PART, CCONJ, PRON, AUX, SCONJ, NUM, SYM, PUNCT, X
upos.categories <- c("NOUN")

nb.gram <- 4 # This defines a window around a woord to look for other occurring words (different values, different graphs)
nb.cooccurrences.displayed.network <- 200 # How many cooccurrences you want to see in the network
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

selected.content <- select.subset(content, categories.1, categories.2, upos.categories, my.stop.words) # Selecting a subset of data

# Calling the main function to extract the co-occurrences.
my.cooccurrences <- cooccurrence(x = selected.content$lemma, group = "sentence_id", order= TRUE, skipgram = nb.gram)

# Simple text display of the 30 most frequent cooccurrences
head(my.cooccurrences, n=30)

# We can further prepare a network graph with the most frequent cooccurrences
cooccurrences.network <- graph_from_data_frame(head(my.cooccurrences, nb.cooccurrences.displayed.network)) # First preparing the components of the network

# Displaying the network:
ggraph(cooccurrences.network, layout = "nicely") +
  geom_edge_link(aes(edge_alpha = cooc), edge_colour = "red", linejoin = "round",
                 check_overlap = TRUE, lineend="round", width=1.5) +
  geom_node_text(aes(label = name), col = "gray20", size=4) +
  labs(x="", y="") +  theme_void() + theme(legend.position = "none") +
  labs(title = "Cooccurrences within document(s)")



# * Sentiment analysis ####

# Works well if there are not too many categories in the level of classification you want to investigate
# (i.e. beware if your select class_2 that you don"t have too many categories)

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# Selecting a subset of text to perform the analysis

# At the first level of classification, which groups do you want to keep?
categories.1 <- "Twenty Thousand Leagues under the Sea" # you can choose one element, two, three
# At the second level of classification, which groups do you want to keep?
categories.2 <- "all"

# At which level of classification do you want to see the results?
results.classification <- "class_2" # class_1 or class_2
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

selected.content <- select.subset(content, categories.1, categories.2) # Selecting a subset of data

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

# These are the 8 sentiments that are returned by the sentiment analyzer (in addition to the overall valence = +/-)
eight.sentiments <- c("anger", "anticipation", "disgust", "fear", "joy", "sadness", "surprise", "trust", "negative", "positive")

# First we compute the valence, i.e. whether the texts are overall positive or negative
my.valences <- get_sentiment(sentences$sentence, method="nrc", language = "english") # sentences <- sentences %>% ungroup %>% mutate(sentiment.valence = get_sentiment(sentence, method="nrc", language = "english")) 
my.valences <- data.frame(valence=my.valences)

# Second, we compute the occurrences of the 8 previous sentiments
my.sentiments <- get_nrc_sentiment(sentences$sentence, language = "english")

# We add everything to our main table
sentences <- sentences %>% bind_cols(my.valences) %>%  bind_cols(my.sentiments) 


# Let's look at the most positive and negative sentences...

# Most positive sentences
most.positive.sentences <- sentences %>% group_by(category = get(results.classification)) %>% filter(valence == max(valence)) %>% select(category, sentence, valence)
most.negative.sentences <- sentences %>% group_by(category = get(results.classification)) %>% filter(valence == min(valence)) %>% select(category, sentence, valence)

most.positive.sentences
most.positive.sentences$sentence

# What are the positive and negative words in the most positive sentences?
# It is not easy to guess which words contribute to the overal valence or to specific sentiments. We can, however, extract them and display them

my.words <- get_tokens(most.positive.sentences$sentence, pattern = "\\W")
word.valences <- get_sentiment(my.words, method="nrc", language = "english")

my.words[word.valences==1] # The words which are analyzed as positive
my.words[word.valences==-1]# The words which are analyzed as negative


# Most negative sentences

most.negative.sentences
most.negative.sentences$sentence

# What are the positive and negative words in the most negative sentences?

my.words <- get_tokens(most.negative.sentences$sentence, pattern = "\\W")
word.valences <- get_sentiment(my.words, method="nrc", language = "english")

my.words[word.valences==1] # The words which are analyzed as positive
my.words[word.valences==-1]# The words which are analyzed as negative

# Start from here 
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# Choose a sentiment to find the most extreme sentences with respect to it
my.emotion <- "fear" # anger, anticipation, disgust, fear, joy, sadness, surprise, trust
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

most.extreme.sentences <- sentences %>% group_by(category = get(results.classification)) %>%
  filter(get(my.emotion) == max(get(my.emotion))) %>% select(category, sentence, my.emotion)
most.extreme.sentences
most.extreme.sentences %>% pull(sentence)

# What are the words explaining the high level of the selected emotion?
my.words <- get_tokens(most.extreme.sentences$sentence, pattern = "\\W")
word.sentiments <- my.words %>% get_nrc_sentiment(language = "english") %>% select(my.emotion)

my.words[word.sentiments==1] # Words which are analyzed as related to the selected sentiment


# Displaying the sentiments
# We first need to construct new data objects

cat.summary <- sentences %>% group_by(category = get(results.classification)) %>% 
  summarize_at(vars(nb.words:positive), sum) 

cat.summary <- cat.summary %>% 
  mutate_at(vars(valence:positive), list(~ ./nb.words)) %>%
  gather(key="sentiment", value="level", 3:13)


cat.summary.valence <- cat.summary %>% filter(sentiment == "valence")
cat.summary.sentiments <- cat.summary %>% filter(sentiment != "valence") 
cat.summary.sentiments <- cat.summary.sentiments %>% mutate(sentiment = factor(sentiment, levels = eight.sentiments))

# First displaying sentiment valence
# If there is only 1 category to display, you will see only 1 bar (not very informative, since comparisons are what matter)
ggplot(data=cat.summary.valence, aes(x=category, y=level)) +
  geom_bar(stat="identity", position=position_dodge()) +
  theme_grey(base_size = 14) + coord_flip() + xlab("Documents") +  ylab("Sentiment valence")

# Display sentiments by category

# If there is only 1 category to display, you will see only 1 bar for each sentiment (not very informative, since comparisons are what matter)
ggplot(data=cat.summary.sentiments, aes(x=category, y=level)) +
  geom_bar(stat="identity", position=position_dodge()) + facet_wrap(~ sentiment, scales = "free") +
  theme_grey(base_size = 14) + coord_flip()

# If there is only 1 category to display, this figure won't be shown properly
ggplot(data=cat.summary.sentiments, aes(x=category, y=level, fill=sentiment)) +
  geom_bar(stat="identity", position=position_dodge()) +
  theme_grey(base_size = 14) + coord_flip()


##########start here 2####

# Sentiment analysis through the sentences of the document #
# BEWARE: Only makes sense if the documents are ordered

sentences$index <- 1:nrow(sentences) # Creating an index corresponding to the order of the sentences
selected.sentiments <- sentences %>% gather(key="sentiment", value="level", 10:20)  # Modifying the format of the data for proper display

ggplot(data=selected.sentiments, aes(x=index, y=level, colour=sentiment)) + 
  geom_line(size=0.5) + ggtitle("Evolution of emotions throughout the documents") + 
  xlab("Story time") + ylab("Intensity") + theme_grey(base_size = 12)

# We don't see anything... it's "moving too fast" = values are too different from one sentence to the next
# We need to smoothen the curves, that is look a the evolution with a sliding window of several sentences

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# Choose the amount of smoothing
# The higher the number, the more complex the curves will be
# There is no "right" value, and it much depends on the size of your corpus - try different values!
my.smoothing <- 70
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

sentence.sentiments <- sentences %>% select(10:20) #Extract the sentiments before smoothing their value

# Let's smooth the values and draw a new graph
x.len <- min(500, nrow(sentences) / 10) 
emotion.smoothed <- as.data.frame(sapply(sentence.sentiments, FUN = get_dct_transform, 
                                         low_pass_size = my.smoothing, x_reverse_len = x.len,
                                         scale_vals = FALSE, scale_range = FALSE))
emotion.smoothed$index <- (1:nrow(emotion.smoothed)) * nrow(sentences) / nrow(emotion.smoothed)

emotions.gathered <- gather(as.data.frame(emotion.smoothed), key="sentiment", value="intensity", 1:11)
emotions.gathered$sentiment <- as.factor(emotions.gathered$sentiment)


# Display positive and negative sentiments

# We call our extract.positions.categories function to get the limits of the categories
my.limits <- extract.positions.categories(sentences, results.classification)
valences <- emotions.gathered %>% filter(sentiment %in% c("positive", "negative")) # Selecting only the "positive" and "negative" sentiments

# Plotting the graph with smoothed values
ggplot(data=valences, aes(x=index, y=intensity, colour=sentiment)) + 
  geom_line(size=1) + ggtitle("Evolution of positive and negative emotions throughout the documents") + 
  xlab("Story time") + ylab("Intensity") + theme_grey(base_size = 18) +
  geom_vline(xintercept = my.limits$index) +
  annotate("text", x = my.limits$index, y = 0, size = 3, label = my.limits$labels, hjust = 0, vjust = -1, angle = 90)


# Display specific sentiments

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# Choose the sentiments to be analyzed
my.sentiments <- c("fear", "joy", "anger") # anger, anticipation, disgust, fear, joy, sadness, surprise, trust
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

my.limits <- extract.positions.categories(sentences, results.classification)
selected.sentiments <- emotions.gathered %>% filter(sentiment %in% my.sentiments)

# Plotting the graph with the selected sentiments
ggplot(data=selected.sentiments, aes(x=index, y=intensity, color=sentiment)) + 
  geom_line(size=1) + ggtitle("Evolution of positive and negative emotions throughout the document(s)") + 
  xlab("Story time") + ylab("Intensity") + theme_grey(base_size = 18) + 
  geom_vline(xintercept = my.limits$index) +
  annotate("text", x = my.limits$index, y = 0, size = 3, label = my.limits$labels, hjust = 0, vjust = -1, angle = 90)

######start here 3####
# * Dispersion of terms ####

# Only makes sense if your categories are ordered

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# Selecting a subset of text to perform the analysis

# At the first level of classification, which groups do you want to keep?
categories.1 <- "Twenty Thousand Leagues under the Sea" # you can choose one element, two, three
# At the second level of classification, which groups do you want to keep?
categories.2 <- "all"

# Select the upos categories to investigate
# Possible categories are: PROPN, NOUN, VERB, ADJ, ADV, INTJ", ADP, DET, PART, CCONJ, PRON, AUX, SCONJ, NUM, SYM, PUNCT, X
upos.categories <- c("PROPN", "NOUN")

# At which level of classification do you want to see the results?
results.classification <- "class_2" # class_1 or class_2

display.most.frequent <- "no" # "yes" or "no"

nb.displayed.most.frequent.lemmas <- 10 # If you chose "yes" above, you can choose how many most frequent lemmas you want to see (don't ask for too many)

my.own.lemmas <- c("Nautilus", "sun") # If you chose "no" above, you can choose you own lemmas to investigate
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

selected.content <- select.subset(content, categories.1, categories.2, upos.cat = upos.categories, stop.words = my.stop.words) # Selecting a subset of data

# Depending on whether you want to see the most frequent lemmas or your target lemmas, the lines below prepare the right data object
if (display.most.frequent == "yes") {
  nb.lemmas <- txt_freq(selected.content$lemma, order=T)
  nb.lemmas$key <- factor(nb.lemmas$key, levels = rev(nb.lemmas$key))
  head(nb.lemmas, n=nb.displayed.lemmas) # The most frequent lemmas
  target.lemmas <- head(nb.lemmas, n=nb.displayed.most.frequent.lemmas)$key
} else {
  target.lemmas <- my.own.lemmas
}

nb.displayed.lemmas <- length(target.lemmas)

# We now prepare the data objects for the display
selected.content.all <- select.subset(content, categories.1, categories.2) #
sentences <- as_tibble(selected.content.all) %>% group_by(class_1, class_2, sentence_id) %>% 
  slice(1) %>% ungroup %>% select(class_1, class_2, sentence)
sentences$index <- 1:nrow(sentences)

lemmas.occurrences <- data.frame(lapply(target.lemmas, function(lemma, sentences) grepl(lemma, sentences$sentence, fixed=T), sentences=sentences))
colnames(lemmas.occurrences) <- target.lemmas
lemmas.occurrences <- lemmas.occurrences*1
sentences <- sentences %>% bind_cols(lemmas.occurrences)

lemmas.occurrences <- gather(sentences, key="lemma", value="present", 5:(5+nb.displayed.lemmas-1)) %>% select(index, lemma, present)

# We create something to add vertical lines delimitating the categories
my.limits <- extract.positions.categories(sentences, results.classification)

# Plotting the dispersion of the target lemmas throughout the documents
ggplot(data=lemmas.occurrences, aes(x=index, y=present)) + 
  geom_line(size=1) + ylim(0, 1) + 
  xlab("Sentence") + theme_grey(base_size = 18) +
  theme(axis.text.y=element_blank(),  axis.ticks.y=element_blank(), axis.title.x = element_blank()) +
  geom_segment(data=my.limits, aes(x=index, xend=index, y = 0, yend=1), color="red") +
  #geom_vline(xintercept = category.limits, color="blue") +
  facet_wrap(~ lemma, scales = "free") + 
  geom_text(data=my.limits, aes(label = labels), color = "red", size=3, hjust=-0.05, vjust = -0.5, angle = 90) 

# * Topic modelling (with Latent Dirichlet Analysis or LDA) ####


# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# Selecting a subset of text to perform the analysis

# At the first level of classification, which groups do you want to keep?
categories.1 <- "all" # you can choose one element, two, three
# At the second level of classification, which groups do you want to keep?
categories.2 <- "all"

# Select the upos categories to investigate
# Possible categories are: PROPN, NOUN, VERB, ADJ, ADV, INTJ", ADP, DET, PART, CCONJ, PRON, AUX, SCONJ, NUM, SYM, PUNCT, X
upos.categories <- c("NOUN", "VERB", "ADJ")

# At which level of classification do you want to see the results?
results.classification <- "class_1" # class_1 or class_2

# Below are the parameters for the LDA algorithm (more precisely for the Gibbs sampling)
# You can play with them, but beware... The algorithm is complex...
burnin <- 4000
iter <- 2000
thin <- 2000
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE

search.from <- 2
search.to <- 15 # you may decrease this value if looking for the right number of topics takes too much time
# these two values define the range of numbers of topics that is being investigated

nb.selected.lemmas <- 2000 # How many lemmas with high tf-idf scores do you want to keep to look for topics?
# The less you keep, the faster the topic-modelling algorithms will run

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

selected.content <- select.subset(content, categories.1, categories.2, upos.cat = upos.categories, stop.words = my.stop.words) # Selecting a subset of data

# We prepare a document-term matrix, with the document being the different sections of the book
content.tmp <- document_term_frequencies(selected.content[, c(results.classification, "lemma")])
content.tmp$term <- factor(content.tmp$term)
content.tmp$doc_id <- factor(content.tmp$doc_id)
dtm <- document_term_matrix(x = content.tmp)

nrow(dtm) # As many rows as sections
ncol(dtm) # As many columns as different lemmas

# We delete rarely occurring terms
dtm_clean <- dtm_remove_lowfreq(dtm, minfreq = 5)

# We now only keep the 2000 most useful lemmas in terms of tf-idf (so too frequent and therefore little informative terms will disappear)
dtm_clean <- dtm_remove_tfidf(dtm_clean, top = nb.selected.lemmas)

# We can have a quick look at the terms which are the most informative when it comes to separating the selected categories of text documents
my.terms <- dtm_tfidf(dtm_clean)
my.terms <- sort(my.terms, decreasing=T)
(differentiating.lemmas <- names(my.terms)[1:40]) # We display the 40 most informative ones, using TF-IDF


# Now that we have the dtm we want, we are going to feed it to algorithms computing topics
# We first need to find the best number of topics, as LDA does not give you this value but requires it
# We call a complex functions to do so...
resultat.nb.topics <- FindTopicsNumber(
  dtm_clean,
  topics = seq(from = search.from, to = search.to, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(nstart = nstart, seed = seed, best = best, burnin = burnin, iter = iter, thin=thin),
  mc.cores = 4L,
  verbose = TRUE
)

# We plot various curves which can be used to select the right number of topics
# A good number of topics should minimize the values of the two metrics on the upper graph
# And maximize the values of the two metrics on the lower graph
FindTopicsNumber_plot(resultat.nb.topics)

# It is often not easy to find the best value, because the different metrics usually tell (slightly or not) different stories...


# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# Choosing the number of topics (given your analysis of the previous graph)
nb.topics <- 7
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Run LDA using Gibbs sampling (we give our tdm matrix to the function, as well as the number of topics)
ldaOut <- LDA(dtm_clean, nb.topics, method="Gibbs", 
              control=list(nstart = nstart, seed = seed, best = best, burnin = burnin, iter = iter, thin=thin))

terms(ldaOut, k=30) # 30 most frequent terms for each topic

# The most likely topics for each section
topics(ldaOut)

# The previous associations are over-simplistic, we could rather investigate how strongly each topic is asociated to words and categories

# Let's draw the highest word probabilities for each topic 
nb.words.displayed <- 15 # You can change this (but if you choose too many words, your graph will be hard to read)

terms <- as.data.frame(t(posterior(ldaOut)$terms))
terms$term <- rownames(terms)
terms <- gather(terms, key="topic", value="beta", 1:nb.topics)
terms$term <- as.factor(terms$term)
terms <- lapply(split(terms, terms$topic), function(y) head(y[order(-y$beta),], nb.words.displayed))
terms <- do.call("rbind", terms)
terms$topic <- as.factor(terms$topic)
terms <- mutate(terms, topic = paste0("Topic ", topic))

# Plotting the graph for each topic
ggplot(terms, aes(reorder_within(term, beta, topic), beta, fill = factor(topic))) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() + scale_x_reordered() + 
  labs(x = NULL, y = expression(beta),
  title = "Highest word probabilities for each topic", subtitle = "Different words are associated with different topics") +
  theme_grey(base_size = 12)


# Let's now draw the probabilities of the different topics for each category
# BEWARE : This will be ok only if there are not too many categories at the selected level of classification...

terms <- as.data.frame(ldaOut@gamma)
nb.docs <- nrow(terms)
colnames(terms)[1:nb.topics] <- paste0("Topic ", 1:nb.topics)
terms$section <- levels(as.factor(selected.content[,results.classification]))
terms <- gather(terms, key="topic", value="gamma", 1:nb.topics)
terms$topic <- as.factor(terms$topic)
terms$section <- as.factor(terms$section)
terms <- lapply(split(terms, terms$topic), function(y) head(y[order(-y$gamma),], nb.docs))
terms <- do.call("rbind", terms)

# Plotting the graph for each topic
ggplot(terms, aes(topic, gamma, fill = factor(section))) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ section, scales = "free") +
  coord_flip() + scale_x_reordered() + 
  labs(x = NULL, y = expression(beta),
       title = "Topic probabilities for each section") +
  theme_grey(base_size = 14)



