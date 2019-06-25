
## From quanteda
library(quanteda)
data("data_corpus_inaugural")
corpus <- data_corpus_inaugural
corpus <- corpus[1:10]

library(udpipe)
udpipe_download_model("english-gum")
udpipe_model <- udpipe_load_model("english-gum-ud-2.3-181115.udpipe")
corpus_udpipe <- udpipe(texts(corpus), udpipe_model)
save(corpus_udpipe, file = "cache/corpus_udpipe.Rdata")

library(spacyr)
spacy_initialize(model = "en")
corpus_spacy <- spacy_parse(corpus)
save(corpus_spacy, file = "cache/corpus_spacy.Rdata")
spacy_finalize()

load("cache/corpus_udpipe.Rdata")
load("cache/corpus_spacy.Rdata")

library(shiny)
library(miniUI)

devtools::load_all("."); texplor_lemmatize(corpus, corpus_udpipe)

library(quanteda)
library(udpipe)
texts <- c("The dog is in the kitchen.", "No", "The cat is sleeping !")
udpipe_model <- udpipe_load_model("english-gum-ud-2.3-181115.udpipe")
test_udpipe <- udpipe(texts, udpipe_model)
test_corpus <- corpus(texts)
devtools::load_all("."); texplor_lemmatize(test_corpus, test_udpipe)


