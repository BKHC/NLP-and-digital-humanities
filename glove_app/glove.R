library(dplyr)
library(text2vec)

if (!file.exists('glove.6B.zip')) {
  download.file('http://nlp.stanford.edu/data/glove.6B.zip',destfile = 'glove.6B.zip')
  unzip('glove.6B.zip')
}

vectors = data.table::fread('glove.6B.300d.txt', data.table = F,  encoding = 'UTF-8')
colnames(vectors) = c('word',paste('dim',1:300,sep = '_'))

as_tibble(vectors)

berlin <- as.numeric(vectors[vectors[, "word"] == "paris",])[-1] - as.numeric(vectors[vectors[, "word"] == "france",])[-1] + as.numeric(vectors[vectors[, "word"] == "germany",])[-1]

targets = list("berlin", "paris", "munich", "leipzig", "germany")

targetVectors = vectors[vectors$word %in% targets,]

rownames(targetVectors) <- targetVectors$word

cos_sim = sim2(x = as.matrix(subset(targetVectors, select = -c(word))), y = t(as.matrix(berlin)), method = "cosine", norm = "l2")

head(sort(cos_sim[,1], decreasing = TRUE), 5)