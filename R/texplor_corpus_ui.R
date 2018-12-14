texplor_corpus_ui <- function(qco, settings) {
  
  ## Custom CSS
  texplor_corpus_css <- function() {
    shiny::HTML("

#ngrams .btn-group {
    margin-bottom: 3px;
}

#ngrams button {
    padding: 6px 8px;
}

#filters .shiny-input-container {
    border-bottom: 1px solid #BBB;
    margin: 0px;
    padding: 10px 15px;
    background-color: #FAFAFA;
    width: 95%;
}

#filters .shiny-input-container:first-child {
    border-top: 1px solid #BBB;
}

#filters {
    margin-bottom: 30px;
}

#stopwords .shiny-input-container {
    width: 95%;
}

.material-switch label {
    width: 45px;
}
")
  }

# Custom JavaScript
texplor_corpus_js <- function() {
  shiny::HTML("
(function($) {
})(jQuery);
  ")
}


## Choices -----------------------------

## n-grams
m_ngrams <- 1:5
names(m_ngrams) <- paste0(1:5, "-gram")

## Document corpus choices
doc_corpus_choices <- "clean"
tmp_names <- gettext("Clean corpus")
if (!is.null(settings$raw_corpus)) {
  doc_corpus_choices <- c(doc_corpus_choices, "raw")
  tmp_names <- c(tmp_names, gettext("Raw corpus"))
  names(doc_corpus_choices) <- tmp_names
}

freqtermplot_y_choices <- c("freq", "nb_docs", "prop_docs")
names(freqtermplot_y_choices) <- c(gettext("Terms frequency"),
  gettext("Number of documents"),
  gettext("Percentage of documents"))

## Location terms type choices
loc_type_choices <- c("words", "sentence")
names(loc_type_choices) <- c(gettext("Words"), gettext("Sentence"))

navbarPage(theme = shinythemes::shinytheme("cosmo"),
  title = actionButton("get_r_code",
    class = "btn-success",
    icon = icon("code"),
    label = gettext("Get R code")),
  windowTitle = "texplor corpus",
  header = tags$head(                        
    tags$style(texplor_text_css()),
    tags$style(texplor_corpus_css())),
  
  ## "Corpus" tab -------------------------------------------------
  
  tabPanel(gettext("Corpus"),
    fluidRow(
      column(2,
        h3(gettext("Corpus treatment")),
        texplor_switch("treat_tolower", gettext("Convert to lowercase")),
        texplor_switch("treat_remove_numbers", gettext("Remove numbers")),
        texplor_switch("treat_remove_punct", gettext("Remove punctuation")),
        texplor_switch("treat_remove_symbols", gettext("Remove symbols")),
        texplor_switch("treat_remove_hyphens", gettext("Remove hyphens")),
        texplor_switch("treat_remove_url", gettext("Remove URLs")),
        #conditionalPanel("input.treat_remove_punct",
        texplor_switch("treat_remove_twitter", gettext("Remove Twitter")),
        texplor_switch("treat_stem", gettext("Stem words", value = FALSE)),
        conditionalPanel("input.treat_stem",
          selectInput("treat_stem_lang", gettext("Stemming language"),
            choices = SnowballC::getStemLanguages(), selected = "english"), width = "50%"),
        
        h3(gettext("Terms computation")),
        shinyWidgets::checkboxGroupButtons("ngrams",
          choices = m_ngrams,
          individual = TRUE,
          selected = 1,
          status = "primary",
          checkIcon = list(yes = icon("ok", 
            lib = "glyphicon"), no = icon("remove", 
              lib = "glyphicon")))
      ),
      column(3,
        h3(gettext("Corpus filtering")),
        #p(gettext("If nothing is selected, no filter is applied.")),
        h4(gettext("Filter terms")),
        numericInput("term_min_occurrences", label = gettext("Minimum frequency"), 0, 0, 1000, 1),
        h4(gettext("Filter documents")),
        uiOutput("filters")
      ),
      column(3,
        h3(gettext("Stop words")),
        div(id="stopwords",
          texplor_switch("treat_stopwords", gettext("Remove stopwords"), value = !is.null(settings$stopwords)),
          textAreaInput("stopwords", gettext("Stop words"), value = paste(settings$stopwords, collapse = ", "),
            width = "100%", rows = 6)
        ),
        h3(gettext("Dictionary")),
        texplor_switch("treat_dictionary", gettext("Apply dictionary"), value = !is.null(settings$dictionary))
      ),
      column(4,
        h3(gettext("Most frequent terms")),
        p(HTML("<strong>", gettext("Number of documents"), "&nbsp;:</strong>"), textOutput("nbdocs", inline = TRUE)),
        DT::dataTableOutput("freqtable")
      )
    )
  ),
  
  ## "Documents" tab --------------------------------------------
  
  tabPanel(gettext("Documents"),
    fluidRow(
      column(6,
        h3(gettext("Terms search")),
        HTML("<p>", gettext('Enter one or more terms. You can use logical operators like <code>&</code> ("and"), <code>|</code> ("or"), <code>!</code> ("not") and parentheses :'), "</p>"),
        fluidRow(
          column(8, textInput("terms", gettext("Terms"), width = "100%")),
          column(4, selectInput("term_group",
            gettext("Group by"),
            choices = c("none", names(vars))))),
        uiOutput("termsAlert"),
        uiOutput("evalAlert"),
        h3(gettext("Selected terms frequency")),
        htmlOutput("freqterm_query"),
        htmlOutput("freqterm_total"),
        conditionalPanel("input.term_group != 'none'",
          tabsetPanel(type = "pills",
            tabPanel(gettext("Table"),
              DT::dataTableOutput("freqtermtable")
            ),
            tabPanel(gettext("Plot"),
              selectInput("freqtermplot_y", "Y-axis", 
                choices = freqtermplot_y_choices, selected = "prop_docs"),
              tags$p(htmlOutput("freqtermplottext")),
              plotOutput("freqtermplot")
            )
          )
        ),
        conditionalPanel("input.term_group == 'none'",
          div(style = "display: none;",
            numericInput("start_documents", gettext("From"), value = 1)),
          fluidRow(
            if (!is.null(settings$raw_corpus)) {
              column(4,
                selectInput("doc_corpus", 
                  gettext("Display documents from"), 
                  choices = doc_corpus_choices))
            },
            column(4,
              selectInput("doc_display", 
                gettext("Display"),
                choices = c("Documents", "Kwics")))
          ),
          fluidRow(
            column(4,
              texplor_switch("doc_metadata", gettext("Display metadata"), right = TRUE, status = "primary", value = TRUE))
          ),
          div(class = "inline-small form-inline",
            actionButton("prev_documents", gettext("Previous"), icon("arrow-left")),
            textOutput("documents_pagination"),
            actionButton("next_documents", gettext("Next"), icon("arrow-right")),
            numericInput("nb_documents_display", gettext(" Number : "), 
              value = 10, min = 1, max = 100, step = 1, width = "auto")),
          htmlOutput("documenttable")
        )
      ),
      column(1),
      column(5, 
        h3(gettext("Terms location")),
        HTML("<p>", gettext('Enter one or more terms :'), "</p>"),
        fluidRow(
          column(8, textInput("location_terms", gettext("Terms"), width = "100%")),
          column(4, radioButtons("location_terms_type", label = NULL,
            choices = loc_type_choices))),
        uiOutput("loctermsAlert"),
        tabsetPanel(type = "pills",
          tabPanel(gettext("Kwics"),
            DT::dataTableOutput("loctermtable")
            
          ),
          tabPanel(gettext("Position plot"),
            tags$p(htmlOutput("loctermplottext")),
            plotOutput("loctermplot")
          )            
        )
      )
    )
  ),
  
  
  ## "Help" tab -----------------------------------------------
  
  tabPanel(gettext("Help"),
    h2(gettext("Help")),
    
    h3(gettext("Most frequent terms")),
    p(HTML(gettext("How to read the table :"))),
    tags$ul(
      tags$li(HTML(gettext("<code>Term frequency</code> : number of times this term is found in the selected corpus"))),
      tags$li(HTML(gettext("<code>Number of documents</code> : number of documents in the selected corpus in which this term appears at least once"))),
      tags$li(HTML(gettext("<code>Percentage of documents</code> : percentage of documents in the selected corpus in which this term appears at least once")))
    ),
    
    h3(gettext("Terms search")),
    p(HTML(gettext("Allows to search for terms or terms combinations in the selected corpus, and to display both frequencies and the corresponding documents. Note that the search is made on the cleaned corpus (after filtering, stemming, removing of stopwords, etc.). Also note that highlighting is not perfect : for example, if searching for <code>I</code>, every \"i\" in the documents will be highlighted, but the search has been made only on the word \"I\"."))),
    p("Query examples :"),
    tags$ul(
      tags$li(HTML(gettext("<code>I</code> : search for documents with the term \"I\" (or \"i\")"))),
      tags$li(HTML(gettext("<code>!i</code> : search for documents without the term \"I\""))),
      tags$li(HTML(gettext("<code>i | me | we</code> : search for documents with \"i\", \"me\" or \"we\" (or any combination)"))),
      tags$li(HTML(gettext("<code>i & we</code> : search for documents with both \"i\" and \"we\""))),
      tags$li(HTML(gettext("<code>sky & (sea | ocean)</code> : search for documents with \"sky\" and the terms \"sea\" or \"ocean\" (or both)")))
    )
  )
)
}