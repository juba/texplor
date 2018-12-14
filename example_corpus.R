library(tm)
library(quanteda)

## From tm
data("acq")
class(acq)
res_brut <- acq
res <- tm_map(acq, tolower)
res <- tm_map(acq, removeWords, stopwords('english'))
texplor(res, obj_brut = res_brut)
texplor(acq)


qacq <- corpus(acq)
docvars(qacq) <- docvars(qacq) %>%
  mutate(datetimestamp = as.Date(datetimestamp),
         id = as.numeric(id),
         oldid = as.numeric(oldid))
texplor(qacq)


## From quanteda
library(quanteda)
data("data_corpus_inaugural")
res <- data_corpus_inaugural

dict <- list(country = c("country", "nation", "governement"))

stop <- c(stopwords("english"), stopwords("french"))
stop <- stopwords("english")
texplor(res, stopwords = stop, dictionary = dict)



## dfm
library(quanteda)
data("data_corpus_inaugural")
dtm <- dfm(data_corpus_inaugural)
texplor(dtm)


## Manual

docs <- c("This is a text.", "This is another one.", "And this is yet another text", "The house is really nice")
co <- corpus(Corpus(VectorSource(docs)))
dtm <- dfm(co)
dtm
textstat_simil(dtm, margin = "features", selection = "this")
dtm
t(dtm) %*% dtm[,"this"]


