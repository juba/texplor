texplor_corpus_server <- function(qco, settings) {
  
  ## Document level variables
  vars <- lapply(docvars(qco), unique)
  nvalues <- lapply(vars, length)
  classes <- lapply(vars, class)
  vars <- vars[(nvalues > 1 & nvalues < 100) | classes %in% c("numeric", "Date")]
  
  ## Return the server function
  function(input, output, session) {
    

    
    ## Variable inputs
    output$filters <- renderUI({
      
      ## Document filters
      filter_inputs <- lapply(names(vars), function(name) {
        v <- vars[[name]]
        input_name <- paste0("meta_", name)
        if (class(v) == "numeric") {
          return(sliderInput(input_name, name, min(v), max(v), value = c(min(v), max(v))))
        }
        if (class(v) == "factor") {
          v <- as.character(v)
        }
        if (class(v) == "character") {
          levels <- unique(v)
          if (length(levels) == 1) return(NULL)
          levels[is.na(levels)] <- "NA"
          return(checkboxGroupInput(input_name, name, choices = levels, selected = levels))
        }
        if (class(v) == "Date") {
          return(dateRangeInput(input_name, name, start = min(v), end = max(v))) 
        }
      }) 
      return(filter_inputs)
    })
    
    ## Convert dictionary textarea value back to dictionary list
    txt2dict <- function(txt) {
      if (trimws(txt) == "") return("")
      entries <- strsplit(txt, "\n")[[1]]
      entries <- entries[entries != ""]
      entries <- trimws(entries, "both")
      dict <- list()
      for (entry in entries) {
        elements <- strsplit(entry, "=")[[1]]
        ## If several '=' in string, repaste them
        if (length(elements) > 2) elements[2] <- paste(elements[-1], collapse = "=")
        ## Ignore if no '='
        if (length(elements) <= 1) next
        key <- trimws(elements[1], "both")
        ## Ignore if no key
        if (key == "") next
        values <- trimws(strsplit(elements[2], ",")[[1]], "both")
        dict[[key]] <- values
      }
      dict
    }
    
    ## Corpus filtering R Code
    filtering_code <- reactive({
      code <- ""
      metas <- grep("^meta_", names(input), value = TRUE)
      for (meta in metas) {
        if (is.null(input[[meta]])) next()
        varname <- gsub("^meta_", "", meta)
        var <- docvars(qco)[[varname]]
        selected_values <- input[[meta]]
        test <- ""
        ## Date variables (date range)
        if (inherits(var, "Date")) {
          if (selected_values[1] != min(var) || selected_values[2] != max(var)) {
            test <- sprintf("%s >= %s & %s <= %s", varname, selected_values[1], varname, selected_values[2])
          }
        }
        ## Character variables (checkboxes)
        if (is.character(selected_values) && length(selected_values) != length(unique(var)) ) {
          selected_values[selected_values == "NA"] <- NA
          selected_values <- paste0(utils::capture.output(dput(selected_values)), collapse="")
          test <- paste0(varname, " %in% ", selected_values)
        }
        ## Numeric variables (range)
        if (is.numeric(selected_values)) {
          if (selected_values[1] != min(var) || selected_values[2] != max(var)) {
            test <- sprintf("%s >= %s & %s <= %s", varname, selected_values[1], varname, selected_values[2])
          }
        } 
        if (test != "") {
          code <- paste0(code, sprintf("tmp_corpus <- corpus_subset(tmp_corpus, %s)\n", test))
        }
      }
      code
    })
    corpus_filtering_code <- function(corpus_name) {
      code <- filtering_code()
      if (code != "") {
        code <- paste0("tmp_corpus <- ", corpus_name, "\n", code)
      }
      return(code)
    }   
    
    ## Create logical vector from selected filter elements
    selected_elements <- function(corpus, meta) {
      varname <- gsub("^meta_", "", meta)
      var <- docvars(corpus)[[varname]]
      selected_values <- input[[meta]]
      ## Date variables (date range)
      if (inherits(var, "Date")) {
        return(var >= selected_values[1] &
            var <= selected_values[2])
      }
      ## Character variables (checkboxes)
      if (is.character(selected_values)) {
        selected_values[selected_values == "NA"] <- NA
        return(var %in% selected_values)
      }
      ## Numeric variables (range)
      if (is.numeric(selected_values)) {
        return(var >= selected_values[1] &
            var <= selected_values[2])
      }
    }
    
    ## Clean corpus filtered
    co <- reactive({
      code <- corpus_filtering_code("qco")
      if (code != "") {
        eval(parse(text = code))
        return(tmp_corpus)
      } else {
        return(qco)
      }
    })
    
    ## Raw corpus filtered
    raw_co <- reactive({
      if (is.null(settings$raw_corpus)) return(co())
      code <- corpus_filtering_code(settings$raw_corpus)
      if (code != "") {
        eval(parse(text = code))
        return(tmp_corpus)
      } else {
        return(settings$raw_corpus)
      }
    })
    
    ## Tokenization code
    # corpus_dictionary_code <- function(dictionary_name) {
    #   paste0("corpus_tokens <- tokens_lookup(corpus_tokens, dictionary(", dictionary_name,"), exclusive = FALSE)")
    # }
    # corpus_tolower_code <- function() {
    #   "corpus_tokens <- tokens_tolower(corpus_tokens)"
    # }
    # corpus_stopwords_code <- function(stopwords_name) {
    #   paste0("corpus_tokens <- tokens_remove(corpus_tokens, ", stopwords_name,")")
    # }
    # corpus_ngrams_code <- function(ngrams) {
    #   paste0("corpus_tokens <- tokens_ngrams(corpus_tokens, n = ", ngrams, ")")
    # }
    
    stopwords_changed <- reactive({
      !identical(sort(unlist(strsplit(input$stopwords, ", *"))), sort(settings$stopwords))
    })
    dict_changed <- reactive({
      !identical(txt2dict(input$dictionary), settings$dictionary)
    })
    
    tokens_code <- function(corpus_name, dictionary_name, stopwords_name) {
      code <- paste0("corpus_tokens <- tokens(", corpus_name, ", what='word'",
        ", remove_punct = ", input$treat_remove_punct, 
        ", remove_symbols = ", input$treat_remove_symbols, 
        ", remove_twitter = ", input$treat_remove_twitter, 
        ", remove_hyphens = ", input$treat_remove_hyphens, 
        ", remove_url = ", input$treat_remove_url, 
        ", remove_numbers = ", input$treat_remove_numbers, ")")
      if (input$treat_tolower) {
        code <- paste0(code, "\n", 
          "corpus_tokens <- tokens_tolower(corpus_tokens)")
      }
      dict <- txt2dict(input$dictionary)
      if (!is.null(input$treat_dictionary) && input$treat_dictionary && dict != "") {
        if (dict_changed()) {
          dictionary_name <- "dict"
          dict <- paste0(utils::capture.output(dput(txt2dict(input$dictionary))), collapse = "")
          code <- paste0(code, "\n",
             dictionary_name, " <- dictionary(", dict, ")")
        }          
        code <- paste0(code, "\n", 
          "corpus_tokens <- tokens_lookup(corpus_tokens, dictionary(", dictionary_name, "), valuetype = '", input$dictionary_type, "', exclusive = FALSE)")
      }
      if (!is.null(input$treat_stopwords) && input$treat_stopwords && input$stopwords != "") {
        if (stopwords_changed()) {
          stopwords_name <- "stops"
          stops <- paste0(utils::capture.output(dput(unlist(strsplit(input$stopwords, " *, *")))), collapse="")
          code <- paste0(code, "\n",
            stopwords_name, " <- ", stops)
          }
        code <- paste0(code, "\n", 
          "corpus_tokens <- tokens_remove(corpus_tokens, ", stopwords_name, ")")
      }
      ngrams <- utils::capture.output(dput(as.numeric(input$ngrams)))
      if (ngrams != 1) {
        code <- paste0(code, "\n", 
          "corpus_tokens <- tokens_ngrams(corpus_tokens, n = ", ngrams, ")")
      }
      code
    }
    ## dfm computation code
    dfm_code <- reactive({
      ngrams <- utils::capture.output(dput(as.numeric(input$ngrams)))
      code <- paste0("dfm <- dfm(corpus_tokens, tolower = FALSE)\n")
      if (input$treat_stem) {
        code <- paste0(code, "dfm <- dfm_wordstem(dfm, language = '", input$treat_stem_lang, "')\n")
      }
      if (!is.na(input$term_min_occurrences) && input$term_min_occurrences > 0) {
        code <- paste0(code, "dfm <- dfm_trim(dfm, min_termfreq = ", input$term_min_occurrences, ")")
      }
      code
    })
    corpus_dfm_code <- function(corpus_name, stopwords_name, dictionary_name) {
      out <- tokens_code(corpus_name, dictionary_name, stopwords_name)
      out <- paste(out, dfm_code(), sep = "\n")
      out
    }
    
    ## Clean corpus dfm
    dtm <- reactive({
      if (length(input$ngrams) == 0 || is.null(co()) || ndoc(co()) == 0) { 
        return(NULL)
      }
      code <- corpus_dfm_code("co()", "settings$stopwords", "settings$dictionary")
      withProgress(message = gettext("Recomputing dfm"), value = 0.3, {
        eval(parse(text = code))
        incProgress(0.7)
      })
      dfm
    })  
    
    
    ## Global table options
    tableOptions <- list(lengthMenu =  c(10,30,50,100), 
      pageLength = 30, orderClasses = TRUE, 
      autoWidth = TRUE, searching = TRUE)
    ## Generate correct datatable order option from a column name
    order_option <- function(table, name, order="desc") {
      index <- which(names(table) == name) - 1
      list(order = list(list(index, order)))
    }
    
    
    ### FREQUENT TERMS --------------------------------------------------------------
    
    callModule(frequentTerms, "freqterms", co(), dtm())
    
    ### TERMS SEARCH ---------------------------------------------------------------------
    
    callModule(searchTerms, "search", co(), dtm())  
  
    ## Disable Twitter remove if punctuation not removed
    observeEvent(input$treat_remove_punct, {
      if (!input$treat_remove_punct) {
        shinyWidgets::updateMaterialSwitch(session, "treat_remove_twitter", value = FALSE)
      }
    })
    

    ### TERMS LOCATION ---------------------------------------------------------------------
    
    ## Location terms
    loc_terms <- reactive({
      tmp <- unlist(stri_extract_all_words(input$location_terms))
      if (length(tmp) == 1 && is.na(tmp)) return(NULL)
      tmp <- tmp[tmp != ""]
    })
    
    ## Location terms kwic object
    kwic_loc_terms <- reactive({
      withProgress(message = gettext("Computing Kwics"), value = 0.3, {
        if (is.null(loc_terms())) {
          return(NULL)
        }
        if (input$location_terms_type == "sentence") {
          terms <- phrase(input$location_terms)
        } else {
          terms <- loc_terms()
        }
        kw <- kwic(co(), terms, window = 7)
        incProgress(0.7)
        return(kw)
      })
    })
    
    ## Terms location table
    output$loctermtable <- DT::renderDataTable({
      if (is.null(kwic_loc_terms()) || nrow(kwic_loc_terms()) == 0) {
        return(DT::datatable(data.frame(table = character())))
      }
      tab <- data.frame(kwic_loc_terms())
      tab$text <- paste0(tab$pre, " <strong>", tab$keyword, "</strong> ", tab$post)
      tab <- tab %>% select("docname", "from", "to", "text") 
      tab <- DT::datatable(tab, options = c(tableOptions), rownames = FALSE, escape = FALSE)
      tab
    })
    
    ## Maximum number of documents to display location plot
    kwic_loc_nb_max <- 80
    ## Terms location plot text
    output$loctermplottext <- renderText({
      text <- ""
      kw <- kwic_loc_terms()
      if (is.null(kw) || nrow(kw) == 0) {
        return(gettext("No document found"))
      } 
      nb_kw_docs <- length(unique(kw$docname))
      text <- paste0(nb_kw_docs, gettext(" documents found. "))
      if (nb_kw_docs > kwic_loc_nb_max) {
        text <- paste0(text,
          sprintf(gettext("<strong>Only the first %s ones are displayed.</strong>"), kwic_loc_nb_max))
      }
      return(HTML(text))
    })
    ## Terms location plot
    output$loctermplot <- renderPlot({
      kw <- kwic_loc_terms()
      if (is.null(kw) || nrow(kw) == 0) return(NULL)
      nb_kw_docs <- length(unique(kw$docname))
      if (nb_kw_docs > kwic_loc_nb_max) kw <- head(kw, kwic_loc_nb_max)
      withProgress(message = gettext("Creating x-ray plot"), value = 0.3, {
        g <- textplot_xray(kw)
        incProgress(0.7)
        return(g)
      })
    }, height = 650)
    
    
    ### CODE EXPORT ---------------------------------------------------------------------
    
    ## Code export modal dialog
    observeEvent(input$get_r_code, {
      code <- ""
      corpus_name <- settings$corpus_name
      filter_code <- corpus_filtering_code(corpus_name)
      if (filter_code != "") {
        code <- paste0("## ", gettext("Corpus filtering"), "\n")
        code <- paste0(code, filter_code, "\n")
        corpus_name <- "tmp_corpus"
      }
      code <- paste0(code, "## ", gettext("Document-feature matrix computation"), "\n")
      code <- paste0(code, corpus_dfm_code(corpus_name, settings$stopwords_name, settings$dictionary_name))
      code <- formatR::tidy_source(text = code, 
        width.cutoff = 75, 
        output = FALSE)$text.tidy
      showModal(modalDialog(
        title = gettext("Export R code"), size = "l", 
        HTML(paste0(gettext("Copy, paste and run the following code in your script to compute the corresponding document-feature matrix (dfm) :"),
          "<pre><code>",
          paste(highr::hi_html(code), collapse = "\n"),
          "</code></pre>")),
        easyClose = TRUE))
    })
    
  }
}