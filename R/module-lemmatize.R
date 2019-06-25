#' @importFrom shinythemes shinytheme
#' @importFrom miniUI miniPage gadgetTitleBar miniContentPanel
#' @importFrom DT dataTableOutput

lemmatizeUI <- function(id, settings) {
  
  ns <- shiny::NS(id)

  miniPage(
    tags$style(texplor_css()),
    miniContentPanel(
      fluidRow(
        column(2, class="well",
          h4(gettext("POS selection")),
          shinyWidgets::awesomeCheckboxGroup(
            inputId = ns("pos"),
            label = "Check POS to remove from corpus",
            choices = settings$pos_choices,
            selected = NULL
          ),
          getRcodeUI(ns("get_r_code"))
        ),
        column(7,
          h4(gettext("Lemmatized terms")),
          awesomeCheckbox(ns("show_context"), gettext("Show token context"), value = FALSE),
          DT::DTOutput(ns("parsed"))
        ),
        column(3,
          h4(gettext("Most frequent terms in resulting corpus")),
          frequentTermsUI(ns("freq_terms"))
        )
      )
    ),
    tags$script(texplor_js())
  )

}


#' @importFrom shinyWidgets checkboxGroupButtons
#' @importFrom DT datatable JS
#' @importFrom glue glue
#' @import rlang 

lemmatizeServer <- function(input, output, session, corpus, parsed, settings) {
  
  ns <- session$ns

  corpus_code <- reactive({
    
    pos_codes <- parse_expr(utils::capture.output(dput(input$pos)))
    pos_var <- sym(settings$pos_var)
    parsed_obj <- sym(settings$parsed_name)
    corpus_obj <- sym(settings$corpus_name)
    
    code <- expr(!!parsed_obj)
    if (!is.null(pos_codes)) {
      code <- expr(!!code %>% filter(!(!!pos_var %in% !!pos_codes)))
    }
    code <- expr(!!code %>% group_by(doc_id))
    code <- expr(!!code %>% summarise(text = paste(token, collapse = ' ')))
    code <- expr(!!code %>% tidyr::complete(doc_id = `!!`(parsed_obj)$doc_id, fill = list(text = '')))
    code_texts <- expr(.texts <- !!code)
  
    code_corpus <- expr(texts(!!corpus_obj) <- .texts)
  
    list(code_texts, code_corpus)
        
  })
  
  parsed_df <- reactive({
    parsed %>% 
      filter(!(pos %in% input$pos))
  })
  

  output$parsed <- DT::renderDT({
    tab_id <- paste0("tab", session$token)

    options <- list(lengthMenu =  c(10,30,50,100), autoWidth = TRUE,
      pageLength = 30, searching = TRUE, dom = "lrtip", 
      stateSave = TRUE, 
      stateSaveCallback = DT::JS(paste0("function(settings, data) {
        sessionStorage.setItem('", tab_id, "', JSON.stringify(data));
      }")),
      stateLoadCallback = DT::JS(paste0("function(settings, callback) {
        return JSON.parse(sessionStorage.getItem('", tab_id, "'));
      }")),
      stateLoaded = DT::JS(paste0("function(settings, data) {
        var columns = data.columns;
        for (var i = 0; i < columns.length; i++) {
          if (columns[i].search !== undefined && columns[i].search.search != '') {
            var search_value = columns[i].search.search;
            $('input', this).eq(i-1).val(search_value);
          }
        }
      }"))
    )
    
    DT::datatable(parsed_df(),
      options = options,
      filter = "top"
    )
    
  })
    
  dtm <- reactive({

    if (is.null(parsed_df()) || nrow(parsed_df()) == 0) return(NULL)

    withProgress(message = gettext("Recomputing dfm"), value = 0.3, {
      dfm <- quanteda::dfm(parsed_df()$lemma)
      incProgress(0.7)
    })
    
    dfm
  })  
  
  callModule(frequentTermsServer, "freq_terms", reactive(dtm()))
  callModule(getRcodeServer, "get_r_code", reactive(corpus_code()))
  
}
