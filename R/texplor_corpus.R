##' @rdname texplor
##' @param raw_corpus optional raw documents corpus
##' @param stopwords stopwords character vector
##' @param dictionary quanteda dictionary list
##' @aliases texplor.Corpus
##' @export

texplor.Corpus <- function(obj, raw_corpus = NULL, stopwords = NULL, dictionary = NULL, ...) {
  
  if (!inherits(obj, "Corpus")) stop(gt("obj must be of class Corpus"))
  
  ## corpus preparation
  corpus <- prepare_results(obj)
  if (!is.null(raw_corpus)) {
    if (!inherits(raw_corpus, "Corpus")) stop(gt("raw_corpus must be of class Corpus"))
    raw_corpus <- prepare_results(raw_corpus)
  }
  
  ## Settings
  settings <- list(raw_corpus = raw_corpus,
    stopwords = stopwords,
    dictionary = dictionary,
    corpus_name = deparse(substitute(obj)),
    raw_corpus_name = deparse(substitute(raw_corpus)),
    stopwords_name = deparse(substitute(stopwords)),
    dictionary_name = deparse(substitute(dictionary)))
  
  ## Launch interface
  texplor_corpus(corpus, settings)
  
}

##' @rdname texplor
##' @aliases texplor.corpus
##' @export

texplor.corpus <- function(obj, raw_corpus = NULL, stopwords = NULL, dictionary = NULL, ...) {
  
  if (!inherits(obj, "corpus")) stop(gt("obj must be of class corpus"))
  
  ## corpus preparation
  corpus <- prepare_results(obj)
  if (!is.null(raw_corpus)) {
    if (!inherits(raw_corpus, "corpus")) stop(gt("raw_corpus must be of class corpus"))
    raw_corpus <- prepare_results(raw_corpus)
  }
  
  ## Settings
  settings <- list(raw_corpus = raw_corpus,
    stopwords = stopwords,
    dictionary = dictionary,
    corpus_name = deparse(substitute(obj)),
    raw_corpus_name = deparse(substitute(raw_corpus)),
    stopwords_name = deparse(substitute(stopwords)),
    dictionary_name = deparse(substitute(dictionary)))
  
  ## Launch interface
  texplor_corpus(corpus, settings)
  
}



##' @import stringi
texplor_corpus_highlight <- function(x, str, ngrams) {
  if (!identical(ngrams, 1)) {
    str <- stri_replace_all_fixed(str, pattern = "_", replacement = " ")
  }
  stringi::stri_replace_all_fixed(x, 
    pattern = str,  
    replacement = paste0("<span class='highlight'>",str,"</span>"),
    vectorize_all = FALSE,
    opts_fixed = stri_opts_fixed(case_insensitive = TRUE))
}


##' @import shiny
##' @import quanteda
##' @importFrom highr hi_html
##' @importFrom SnowballC getStemLanguages
##' @importFrom formatR tidy_source
##' @importFrom shinythemes shinytheme
##' @import shinyWidgets
##' @import dplyr
##' @import ggplot2

texplor_corpus <- function(qco, settings) { 
  app <- shiny::shinyApp(
    ui = texplor_corpus_ui(qco, settings),
    server = texplor_corpus_server(qco, settings)
  )
  app
}