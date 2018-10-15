##' Interface for analysis results exploration
##'
##' This function launches a shiny app in a web browser in order to do
##' interactive visualisation and exploration of a textual corpus.
##'
##' @param obj corpus object
##' @param ... arguments passed to other methods
##' @return
##' The function launches a shiny app in the system web browser.
##' @author Julien Barnier <julien.barnier@@ens-lyon.fr>
##' @export
##' @examples
##' \dontrun{
##' require(quanteda)
##' data("data_corpus_inaugural")
##' explor(data_corpus_inaugural)
##' }

explor <- function(obj, ...) {
  UseMethod("explor")
}

