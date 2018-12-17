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
    
    ### FREQUENT TERMS -----------------------------------------------------------
    
    ## Number of documents
    output$nbdocs <- renderText({
      ndoc(co())
    })
    
    ## Most frequent terms table
    output$freqtable <- DT::renderDataTable({
      if (is.null(dtm())) return(NULL)
      frq <- data.frame(topfeatures(dtm(), n = 10000))
      names(frq) <- "nb_terms"
      frq$term <- rownames(frq)
      docf <- data.frame(docfreq(dtm(), scheme = "count"))
      names(docf) <- "nb_docs"
      docf$term <- rownames(docf)
      docf$prop_docs <- (round(docf$nb_docs / ndoc(dtm()) * 100, 2))
      # names(docf) <- c(gettext("Term frequency"),
      #                  gettext("Documents frequency"),
      #                  gettext("Documents proportion"))
      tab <- frq %>% left_join(docf, by = "term") %>% select(term, nb_terms, nb_docs, prop_docs)
      names(tab) <- c(gettext("Term"),
        gettext("Term frequency"),
        gettext("Number of documents"),
        gettext("Percentage of documents"))
      DT::datatable(tab, 
        options = c(tableOptions, order_option(tab, gettext("Term frequency"))), rownames = FALSE)
    })
    
    ### TERMS SEARCH ---------------------------------------------------------------------
    
    ## Terms input
    terms <- reactive({
      tmp <- unlist(stri_extract_all_words(input$terms))
      if (length(tmp) == 1 && is.na(tmp)) return(NULL)
      tmp <- tmp[tmp != ""]
    })
    ## Invalid terms in terms input
    invalid_terms <- reactive({
      tmp_terms <- terms()
      tmp_terms[!(tmp_terms %in% colnames(dtm()))]    
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
      dfm_terms <- dtm() %>% 
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
      tmp_dfm <- docvars(co()) %>% select(!!!input$term_group) %>% bind_cols(tmp_dfm)
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
      var <- docvars(co()) %>% pull(!!group)
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
    
    ## Disable Twitter remove if punctuation not removed
    observeEvent(input$treat_remove_punct, {
      if (!input$treat_remove_punct) {
        shinyWidgets::updateMaterialSwitch(session, "treat_remove_twitter", value = FALSE)
      }
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
        out <- paste(out, "<p><strong>", rownames(docvars(co()))[i] ,"</strong></p>")
        if (is.null(input$doc_corpus) || req(input$doc_corpus) == "clean") {
          tmp_corp <- co()
        } else { 
          tmp_corp <- raw_co()
        }
        tmp_terms <- terms()
        if (!identical(input$ngrams, 1)) {
          tmp_terms <- stri_replace_all_fixed(tmp_terms, pattern = "_", replacement = " ")
        }
        if (input$doc_display == "Documents") {
          out <- paste(out, texplor_corpus_highlight(tmp_corp[i], tmp_terms, input$ngrams))                       
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
          meta <- docvars(co())[i,]
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