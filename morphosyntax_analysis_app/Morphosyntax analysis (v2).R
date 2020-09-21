# This is to install and load the so-called udpipe package of functions
if (!require("udpipe")) install.packages("udpipe")
library(udpipe)

my_folder <- "D:/"

# The first time, you have to download a language model to perform morpho-syntactic analysis
udpipe_download_model(language = "english", model_dir = my_folder)

# After that, you can simply load the language model stored on your computer and ignore the previous command line
my_language_model <- udpipe_load_model(paste0(my_folder, "english-ewt-ud-2.4-190531.udpipe"))

# Enter any text you like
my_text <- "I have just bought two hats in Hong Kong while on vacation."

# Let's call the key function which will analyse our sentence
my_output <- udpipe_annotate(my_language_model, x= my_text, keep_acronyms=TRUE)

# Let's transform the output, so it is easier to inspect...
my_output <- as.data.frame(my_output)

# A data frame is an array with each column containing a specific kind of information
str(my_output)

# Dropping some columns to see things more easily
my_simplified_output <- subset(my_output, select = -c(doc_id, paragraph_id, sentence, deps, misc))

# Displaying the result
my_simplified_output


