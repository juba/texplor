

getRcodeUI <- function(id) {
  
  ns <- NS(id)
  
  actionButton(ns("get_r_code"),
    class = "btn-success",
    icon = icon("code"),
    label = gettext("Get R code"))

}


#' @importFrom formatR tidy_source
#' @importFrom highr hi_html
#' @importFrom rlang expr_deparse

getRcodeServer <- function(input, output, session, code) {
  
  ## Code export modal dialog
  observeEvent(input$get_r_code, {
    code <- purrr::map_chr(code(), ~{
      out <- expr_deparse(.x, width=1e4)
      out <- gsub("%>%", "%>%\n ", out)
      out <- styler::style_text(out, strict = FALSE)
      paste(out, collapse = "\n")    
    })

    showModal(modalDialog(
      title = gettext("Export R code"), size = "l", 
      HTML(paste0(gettext("Copy, paste and run the following code in your script :"),
        "<pre><code>",
        paste(highr::hi_html(code), collapse = "\n"),
        "</code></pre>")),
      easyClose = TRUE))
  })
  
  
}