
## FUNCTIONS -------------------------------------------------------------------

doc_corpus_choices <- function(settings) {
  res <- "clean"
  tmp_names <- gettext("Clean corpus")
  if (!is.null(settings$raw_corpus)) {
    res <- c(res, "raw")
    tmp_names <- c(tmp_names, gettext("Raw corpus"))
    names(res) <- tmp_names
  }
  res
}

## Document level variables
vars <- function(corpus) {
  vars <- lapply(docvars(corpus), unique)
  nvalues <- lapply(vars, length)
  classes <- lapply(vars, class)
  vars <- vars[(nvalues > 1 & nvalues < 100) | classes %in% c("numeric", "Date")]
}


freqtermplot_y_choices <- c("freq", "nb_docs", "prop_docs")
names(freqtermplot_y_choices) <- c(gettext("Terms frequency"),
  gettext("Number of documents"),
  gettext("Percentage of documents"))


##' @import stringi
highlight <- function(x, str, ngrams) {
  if (!identical(ngrams, 1)) {
    str <- stri_replace_all_fixed(str, pattern = "_", replacement = " ")
  }
  stringi::stri_replace_all_fixed(x, 
    pattern = str,  
    replacement = paste0("<span class='highlight'>",str,"</span>"),
    vectorize_all = FALSE,
    opts_fixed = stri_opts_fixed(case_insensitive = TRUE))
}


## UI --------------------------------------------------------------------------

searchTermsUI <- function(id, corpus, settings) {
  
  ns <- NS(id)
  
  tagList(
    
    h3(gettext("Terms search")),
    
    HTML("<p>", gettext('Enter one or more terms. You can use logical operators like <code>&</code> ("and"), <code>|</code> ("or"), <code>!</code> ("not") and parentheses :'), "</p>"),
    
    fluidRow(
      column(8, textInput(ns("terms"), gettext("Terms"), width = "100%")),
      column(4, selectInput(ns("term_group"),
        gettext("Group by"),
        choices = c("none", names(vars(corpus)))))),
    
    uiOutput(ns("termsAlert")),
    uiOutput(ns("evalAlert")),
    
    h3(gettext("Selected terms frequency")),
    
    htmlOutput(ns("freqterm_query")),
    htmlOutput(ns("freqterm_total")),
    
    conditionalPanel("input.term_group != 'none'", ns = NS(id),
      tabsetPanel(type = "pills",
        tabPanel(gettext("Table"),
          DT::dataTableOutput(ns("freqtermtable"))
        ),
        tabPanel(gettext("Plot"),
          selectInput(ns("freqtermplot_y"), "Y-axis", 
            choices = freqtermplot_y_choices, selected = "prop_docs"),
          tags$p(htmlOutput(ns("freqtermplottext"))),
          plotOutput(ns("freqtermplot"))
        )
      )
    ),
    
    conditionalPanel("input.term_group == 'none'", ns = NS(id),
      div(style = "display: none;",
        numericInput(ns("start_documents"), gettext("From"), value = 1)),
      fluidRow(
        if (!is.null(settings$raw_corpus)) {
          column(4,
            selectInput(ns("doc_corpus"), 
              gettext("Display documents from"), 
              choices = doc_corpus_choices(settings)))
        },
        column(4,
          selectInput(ns("doc_display"), 
            gettext("Display"),
            choices = c("Documents", "Kwics")))
      ),
      fluidRow(
        column(4,
          texplor_switch(ns("doc_metadata"), gettext("Display metadata"), right = TRUE, status = "primary", value = TRUE))
      ),
      div(class = "inline-small form-inline",
        actionButton(ns("prev_documents"), gettext("Previous"), icon("arrow-left")),
        textOutput(ns("documents_pagination")),
        actionButton(ns("next_documents"), gettext("Next"), icon("arrow-right")),
        numericInput(ns("nb_documents_display"), gettext(" Number : "), 
          value = 10, min = 1, max = 100, step = 1, width = "auto")),
      htmlOutput(ns("documenttable"))
    )
  )

}


searchTerms <- function(input, output, session, corpus, dtm) {
  
  ## Terms input
  terms <- reactive({
    tmp <- unlist(stri_extract_all_words(input$terms))
    if (length(tmp) == 1 && is.na(tmp)) return(NULL)
    tmp <- tmp[tmp != ""]
  })
  ## Invalid terms in terms input
  invalid_terms <- reactive({
    tmp_terms <- terms()
    tmp_terms[!(tmp_terms %in% colnames(dtm))]    
  })
  
  ## Run the query on the document-feature matrix as environment,
  ## and returns the result
  terms_query <- reactive({
    
    ## Progress
    query_progress <- shiny::Progress$new()
    on.exit(query_progress$close())
    query_progress$set(message = gettext("Running query"), value = 0)
    
    error <- NULL
    if (length(terms()) == 0) return(list(res = NULL, error = NULL))
    dfm_terms <- dtm %>% 
      dfm_select(pattern = terms(), valuetype = "fixed", selection = "keep") %>% 
      convert(to = "data.frame")
    query_progress$inc(0.3)
    ## Convert count to presence / absence
    #if (ncol(dfm_terms) > 0) {
    #  dfm_terms[dfm_terms > 0] <- 1
    #}
    query_progress$inc(0.1)
    res <- try({
      code <- input$terms
      ## Add backquotes to words with quote
      code <- stringi::stri_replace_all_regex(code, pattern="(\\b)(\\w*'\\w*)(\\b)", replacement = "$1`$2`$3" )
      eval(parse(text = code), envir = dfm_terms) %>% 
        data.frame()
    }, silent = TRUE)
    query_progress$inc(0.6)
    if (inherits(res, "try-error")) {
      res <- NULL
      error <- geterrmessage()
    }
    list(res = res, error = error)
  })
  
  ## Alert if term is missing from corpus
  output$termsAlert <- renderUI({
    if (length(invalid_terms() > 0) && invalid_terms() != "") {
      tmp_terms <- paste(invalid_terms(), collapse = ", ")
      div(class = "alert alert-danger",
        HTML(paste(gettext("<strong>Warning :</strong> the following terms are missing from the corpus : <i>"), tmp_terms, "</i>")))
    }
  })
  
  ## Alert if error in search expression
  output$evalAlert <- renderUI({
    if (length(invalid_terms() > 0) && invalid_terms() != "") { return(NULL) }
    e <- terms_query()$error
    if (!is.null(e)) {
      div(class = "alert alert-danger",
        HTML(paste(gettext("<strong>Warning :</strong> Query error : <i>"), e, "</i>")))
    }
  })
  
  ## Search term frequency table
  tab_term <- reactive({
    if (input$term_group == "none") return(NULL)
    tmp_dfm <- terms_query()$res
    if (is.null(tmp_dfm)) return(NULL)
    updateNumericInput(session, "start_documents", value = 1)
    tmp_dfm <- docvars(corpus) %>% select(!!!input$term_group) %>% bind_cols(tmp_dfm)
    names(tmp_dfm) <- c("group", "n")
    res <- tmp_dfm %>% 
      group_by(group) %>% 
      summarise(freq = sum(n), nb_docs = sum(n > 0), prop_docs = round(nb_docs / n() * 100, 1))
    res
  })
  
  
  ## Search term total frequency
  tab_term_tot <- reactive({
    tmp_dfm <- terms_query()$res
    if (is.null(tmp_dfm)) return(NULL)
    names(tmp_dfm) <- "n"
    res <- tmp_dfm %>% 
      summarise(nb_docs = sum(n > 0), prop_docs = round(nb_docs / n() * 100, 1)) %>%
      mutate(nom = gettext("Total")) %>% select(nom, nb_docs, prop_docs)
    res
  })
  
  nb_docs_term <- reactive({
    if (is.null(tab_term())) return(0)
    as.numeric(tab_term() %>% summarise(n = sum(nb_docs)))
  })
  
  ## Searched terms query text
  output$freqterm_query <- renderText({
    if (input$terms == "") {
      return("")
    }
    res <- paste0(gettext("<p><strong>Query :</strong> <code>"), input$terms, "</code>.</p>")
    return(HTML(res))
  })
  
  ## Total searched terms frequency text
  output$freqterm_total <- renderText({
    if (is.null(tab_term_tot())) {
      return("")
    }
    tab <- tab_term_tot()
    res <- paste0(gettext("<p><strong>Frequency in corpus :</strong> "), tab$nb_docs, gettext(" documents ("), tab$prop_docs, "%).</p>")
    return(HTML(res))
  })
  
  ## Searched terms frequency table
  output$freqtermtable <- DT::renderDataTable({
    if (is.null(tab_term()) || nb_docs_term() == 0) {
      return(DT::datatable(data.frame(table = character())))
    }
    tab <- tab_term()
    names(tab) <- c(input$term_group,
      gettext("Terms frequency"),
      gettext("Number of documents"),
      gettext("Percentage of documents"))
    tab <- DT::datatable(tab, 
      options = c(tableOptions, order_option(tab, gettext("Percentage of documents"))), rownames = FALSE)
    tab
  })
  
  output$freqtermplottext <- renderText({
    if (is.null(tab_term()) || nb_docs_term() == 0) {
      return(gettext("No document found"))
    } else {
      text <- paste0(nb_docs_term(), gettext(" documents found. "))
    }
    return(HTML(text))
  })
  
  ## Searched terms frequency plot
  output$freqtermplot <- renderPlot({
    if (is.null(tab_term()) || nb_docs_term() == 0) {
      return()
    }
    tab <- tab_term()
    group <- quo(input$term_group)
    var <- docvars(corpus) %>% pull(!!group)
    g <- NULL
    y_label <- names(freqtermplot_y_choices)[which(freqtermplot_y_choices == input$freqtermplot_y)]
    if (is.character(var) || is.factor(var)) {
      tab <- tab %>% filter(prop_docs > 0) 
      tab$group <- stats::reorder(tab$group, tab %>% pull(!!input$freqtermplot_y))
      g <- ggplot(tab) + 
        geom_bar(aes_string(x = "group", y = input$freqtermplot_y), stat = "identity") +
        xlab(input$term_group) +
        ylab(y_label) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        expand_limits(y = 0)
    } 
    if (is.numeric(var) || inherits(var, "Date")) {
      g <- ggplot(tab, aes_string(x = "group", y = input$freqtermplot_y)) + 
        geom_line() +
        geom_smooth() +
        xlab(input$term_group) +
        ylab(y_label) +
        expand_limits(y = 0)
    }
    g
  })
  
  ## Observers on pagination events
  observeEvent(input$prev_documents, {
    updateNumericInput(session, "start_documents", value = max(1, isolate(input$start_documents) - isolate(input$nb_documents_display)), min = 1)
  })
  observeEvent(input$next_documents, {
    val <- isolate(input$start_documents) + isolate(input$nb_documents_display)
    nb_docs <- tab_term_tot()$nb_docs
    if (val <= nb_docs) {
      updateNumericInput(session, "start_documents", value = val)
    }
  })
  
  ## Documents list
  output$documenttable <- renderText({
    ## Acquire dependencies
    nb_docs <- tab_term_tot()$nb_docs
    start <- input$start_documents
    nb_documents <- input$nb_documents_display
    if (is.null(terms_query()$res)) return(gettext("<div class='document-content'>No document found.</p>"))
    indexes <- which(as.vector(terms_query()$res) > 0)
    end <- min(start + nb_documents - 1, nb_docs)
    indexes <- indexes[start:end]
    out <- ""
    for (i in indexes) {
      out <- paste(out, "<div class='document-content'>")
      out <- paste(out, "<p><strong>", rownames(docvars(corpus))[i] ,"</strong></p>")
      if (is.null(input$doc_corpus) || req(input$doc_corpus) == "clean") {
        tmp_corp <- corpus
      } else { 
        tmp_corp <- raw_corpus
      }
      tmp_terms <- terms()
      if (!identical(input$ngrams, 1)) {
        tmp_terms <- stri_replace_all_fixed(tmp_terms, pattern = "_", replacement = " ")
      }
      if (input$doc_display == "Documents") {
        out <- paste(out, highlight(tmp_corp[i], tmp_terms, input$ngrams))                       
      }
      if (input$doc_display == "Kwics") {
        tmp_terms <- phrase(tmp_terms)
        kwics <- kwic(tmp_corp[i], pattern = tmp_terms, window = 7, valuetype = "fixed",
          what = "word", 
          remove_punct = input$treat_remove_punct, 
          remove_symbols = input$treat_remove_symbols,
          remove_twitter = input$treat_remove_twitter,
          remove_hyphens = input$treat_remove_hyphens,
          remove_url = input$treat_remove_url,
          remove_numbers = input$treat_remove_numbers)
        kwics$text <- paste0("...", kwics$pre, " <strong>", kwics$keyword, "</strong> ", kwics$post, "...")
        kwics <- paste(kwics$text, collapse = "<br />")
        out <- paste(out, kwics)
      }
      if (input$doc_metadata) {
        meta <- docvars(corpus)[i,]
        metadata <- character(0)
        for (m in colnames(meta)) {
          metadata <- append(metadata, paste0("<span>", m, "</span>: <code>", meta[, m], "</code>"))
        }
        metadata <- paste(metadata, collapse = "<br />")
        out <- paste(out, HTML("<p class='metadata'>", metadata, "</p>"))
      }
      out <- paste(out, "</div>")
    }
    HTML(out)
  })
  
  ## documents list pagination text
  output$documents_pagination <- renderText({
    start <- input$start_documents
    nb_documents_display <- input$nb_documents_display
    nb_docs <- tab_term_tot()$nb_docs
    end <- min(start + nb_documents_display - 1, nb_docs)
    if (end == 0) start <- 0
    sprintf(gettext("%s to %s of %s"), start, end, nb_docs)
  })
  
  
  
}


