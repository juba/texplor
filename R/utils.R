if (getRversion() >= "2.15.1")  
  utils::globalVariables(c("group", "nb_docs", "nb_terms", "nom", "prop_docs", "term", "tmp_corpus"))


##' Interface for analysis results exploration
##'
##' This function launches a shiny app in a web browser in order to do
##' interactive visualisation and exploration of a textual corpus.
##'
##' @param obj corpus object
##' @param ... arguments passed to other methods
##' @return
##' The function launches a shiny app in the system web browser.
##' @export
##' @examples
##' \dontrun{
##' require(quanteda)
##' data("data_corpus_inaugural")
##' texplor(data_corpus_inaugural)
##' }

#texplor <- function(obj, ...) {
#  UseMethod("texplor")
#}

##' @import shiny

texplor_css <- function() {
  shiny::HTML("
body {margin: 0;}

.navbar-default {
  border: none;
}

.navbar-header {
  float: right;
}

.navbar-brand {
  padding: 0;
}              

.help-icon {
  margin-left: 6px;
  font-size: 14px;
  color: #888;
  cursor: help;
}

#filters .shiny-input-checkboxgroup .shiny-options-group {
    max-height: 200px; 
    overflow-y: scroll; 
}

#get_r_code {
    height: 100%;
    width: 100%;
}

ul.nav-pills {
    margin-top: 15px;
    margin-bottom: 30px;
}

.checkbox { margin-bottom: 3px;}
input[type=checkbox] { margin: 0;}

.dataTable th, 
.dataTable td {
    font-size: 12px !important;
    padding: 3px 5px !important; 
}
.dataTable th { padding-right: 18px !important }
.dataTables_wrapper {
    max-width: 850px;
    margin-bottom: 2em;
}
.dataTables_wrapper label {
    font-weight: normal;
    font-size: 90%;
}
.dataTables_info, .dataTables_length, 
.dataTables_filter, .dataTables_paginate {
    font-size: 11px !important;
}
.document-content {
    font-size: 12px !important;
    background-color: #EEE;
    border-radius: 8px;
    padding: 10px;
    margin: 20px 5px;
}
.document-content .metadata {
    margin-top: 10px;
    border-left: 2px solid #BBB;
    padding-left: 5px;
    font-size: 90%;
}
.highlight {background-color: yellow;}

.inline-small * {
    display: inline;    
    font-size: 80% !important;
}
.inline-small .btn {
    padding: 3px 5px;
}

/* Syntax highlighting */
span.hl.str { color: #d14;}
span.hl.kwa { color: #099;}
span.hl.num { color: #099;}
span.hl.kwd { color: #333; font-weight: bold;}
span.hl.com { color: #888; font-style: italic;}
")
}

##' @export

texplor_js <- function() {
  "
(function($) {

 $('[data-toggle=\"popover\"]').popover(
    {trigger: 'hover',
     placement: 'top'}
  );

})(jQuery);
  "
}

## Generate correct datatable order option from a column name
order_option <- function(table, name, order="desc") {
  index <- which(names(table) == name) - 1
  list(order = list(list(index, order)))
}

#' @importFrom shinyWidgets materialSwitch

texplor_switch <- function(id, label, value = TRUE, right = TRUE, status = "primary") {
  shinyWidgets::materialSwitch(
    inputId = id,
    label = label,
    value = value,
    right = right,
    status = status
  )
}

