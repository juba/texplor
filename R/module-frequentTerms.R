

frequentTermsUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    DT::dataTableOutput(ns("freqtable"))
  )
  
}

frequentTermsServer <- function(input, output, session, dtm) {
  
  tab_id <- paste0("freqtermtab_", session$token)
  
  ## Most frequent terms table
  output$freqtable <- DT::renderDataTable({
    
    if (is.null(dtm())) return(NULL)

    frq <- data.frame(topfeatures(dtm(), n = 10000))
    names(frq) <- "nb_terms"
    frq$term <- rownames(frq)
    docf <- data.frame(docfreq(dtm(), scheme = "count"))
    names(docf) <- "nb_docs"
    docf$term <- rownames(docf)
    docf$prop_docs <- round(docf$nb_docs / ndoc(dtm()) * 100, 2)
    tab <- frq %>% left_join(docf, by = "term") %>% select(term, nb_terms, nb_docs, prop_docs)
    names(tab) <- c(gettext("Term"),
      gettext("Term frequency"),
      gettext("Number of documents"),
      gettext("Percentage of documents"))
    
    options <- list(lengthMenu =  c(10,30,50,100), 
      pageLength = 30, orderClasses = TRUE, 
      autoWidth = TRUE, searching = TRUE)
    
    DT::datatable(tab,
      options = c(
        options, 
        order_option(tab, gettext("Term frequency")),
        list(
          stateSave = TRUE, 
          stateSaveCallback = DT::JS(paste0("function(settings, data) {
            sessionStorage.setItem('", tab_id, "', JSON.stringify(data));
          }")),
          stateLoadCallback = DT::JS(paste0("function(settings, callback) {
            return JSON.parse(sessionStorage.getItem('", tab_id, "'));
          }"))
        )
      ),
      rownames = FALSE)
    
  })

}