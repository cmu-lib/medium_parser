library(shinydashboard)
library(sparkline)

# Define UI for application that draws a histogram
dash_header <- dashboardHeader(
  title = "Medium Corpus"
)

stem_picker <- radioButtons("stem_choices", choices = c("original", "stemmed"), selected = "stemmed", label = "Use original tokens, or stemmed tokens")

core_corpus_selector <- selectizeInput("available_corpora", choices = NULL, selected = NULL, multiple = TRUE, label = "Original Medium.com search terms")

corpus_tf_idf_tab <- tabItem(
  tabName = "corpus_tf_idf",
  h2("TF-IDF of the corpus documents"),
  p("This view shows all the documents in the corpus, and shows the top 10 tokens for each document according to TF-IDF (terms that appear relatively frequently in that individual document, but rarely across the larger corpus, suggesting that they are terms distinctive to that document.)"),
  box(
    DT::dataTableOutput("document_metadata"),
    title = "Document TF-IDF",
    width = 12)
)

termsovertime_tab <- tabItem(
  tabName = "termsovertime",
  h2("Terms over time"),
  p("Add tokens to the box below to chart what percentage of documents in the corpus contain them at different points in time. Currently, the documents are clustered into halfyear buckets."),
  list(
    box(
      selectizeInput("wordchart_tokens", choices = NULL, selected = NULL, multiple = TRUE, label = "Pick tokens to plot over time"),
      p("Begin typing to generate token suggestions. Click on a token and press \"Delete\" to remove it from the list.")
    ),
    box(
      radioButtons("termsovertime_count_select", "Measurement type", choices = c("ratio", "absolute"), selected = "ratio"),
      plotOutput("termsovertime_chart"),
      width = 12,
      height = 750
    ),
    box(
      DT::dataTableOutput("termsovertime_metadata"),
      title = "Documents with these tokens",
      width = 12
    )
  )
)

kwic_tab <- tabItem(
  tabName = "kwic",
  h2("Keywords in context"),
  p("Select a token to view the keywords in context for the selected corpus"),
  selectizeInput("kwic_tokens", choices = NULL, selected = NULL, multiple = FALSE, label = "Pick token to see in context"),
  tabBox(
    tabPanel("Target KWIC", DT::dataTableOutput("kwic_table")),
    tabPanel("Reference KWIC", DT::dataTableOutput("reference_kwic_table")),
    width = 12
  )
)

keyness_tab <- tabItem(
  tabName = "keyness",
  h2("Subcorpus Keyness"),
  p("How does this selected subcorpus compare to the full Medium corpus?"),
  fluidRow(
    box(
      width = 6,
      title = "Define reference corpus",
      selectizeInput("reference_corpora", choices = NULL, selected = NULL, multiple = TRUE, label = "Select base comparison corpus"),
      selectizeInput("keyness_include", choices = NULL, selected = "", multiple = TRUE, label = "Must include ALL terms"),
      selectizeInput("keyness_or", choices = NULL, selected = "", multiple = TRUE, label = "Must include AT LEAST ONE term"),
      selectizeInput("keyness_exclude", choices = NULL, selected = "", multiple = TRUE, label = "Must exclude terms"),
      p("Number of docs: ", textOutput("reference_corpus_size", inline = TRUE)),
      actionButton("keynessButton", "Compute"),
      downloadButton("keyness_report", "Download report"),
    ),
    box(
      width= 6,
      title="Current keyword selections",
      tableOutput("keyword_summary")
    )
  ),
  tabBox(
    width = 12,
    tabPanel("Keyness",
             box(title = "Definitions", collapsible = TRUE, width = 12, collapsed = TRUE, 
                 tags$ul(
                   tags$li(strong("Feature"), "a token from the corpus"),
                   tags$li(strong("G2"), "G-squared, a measure of the likelihood that this feature is found in the target corpus and not in the reference corpus. This captures how disporportionaltely more (or less, if G2 is negative) the given feature appears in the target corpus versus the reference corpus. G-squared attempts to normalize scores based on the actual frequency of features in the comparison, so features terms that show up disproportionately more in the target corpus, but which still only have a small absolute number of appearances, are ranked lower than terms that show up more frequently."),
                   tags$li(strong("es"), "Effect size, an alternate measure of how disproportionately a given feature shows up in the target corpus compared to the reference corpus. Unlike g-squared, this measure does not attempt to adjust its ranking based on the absolute number of times a feature appears in either corpus. Therefore, a very rarely used term (e.g. \"trolley\") might have a high effect size in certain comparisons, but a lower g-squared. It is useful to look at both measures."),
                   tags$li(strong("target_termfreq"), "How many times is this token used across all the documents in the target corpus?"),
                   tags$li(strong("target_docfreq"), "How many documents in the target corpus use this term at least once?"),
                   tags$li(strong("reference_termfreq"), "How many times is this token used across all the documents in the refrence corpus?"),
                   tags$li(strong("reference_docfreq"), "How many documents in the reference corpus use this term at least once?"),
                   tags$li(strong("p"), "The p-value of the keyness measure; how likely is it that the same measure could be found by chance in a corpus of randomly-distributed terms?")
                 )),
             DT::dataTableOutput("keyness_table")),
    tabPanel("Target Authors", p("A count of the number of articles written by each author in the target corpus"), DT::dataTableOutput("target_authors_table")),
    tabPanel("Reference Authors", p("A count of the number of articles written by each author in the reference corpus"), DT::dataTableOutput("reference_authors_table"))
  )
)

term_comparison_tab <- tabItem(
  tabName = "term_comparison",
  h2("Term co-occurrence"),
  fluidRow(
    box(
      width = 6,
      selectizeInput("co_tokens", choices = NULL, selected = NULL, multiple = TRUE, label = "Choose terms to compare co-occurrence across target and reference corpora"),
      p("You must choose at least 2 terms in order to render the comparison plots. ", strong("Strongly recommend using the stemmed corpus in order to do useful comparsions."))
    ),
    box(
      width= 6,
      title="Current keyword selections",
      tableOutput("coocurrence_keyword_summary")
    )
  ),
  box(
    width = 12,
    title = "Target Corpus",
    plotOutput("target_coocurrence_plot")
  ),
  box(
    width = 12,
    title = "Reference Corpus",
    plotOutput("reference_coocurrence_plot")
  )
)

corpus_inclusive <- selectizeInput("corpus_include", choices = NULL, selected = "", multiple = TRUE, label = "Must include ALL terms")

corpus_or <- selectizeInput("corpus_or", choices = NULL, selected = "", multiple = TRUE, label = "Must include AT LEAST ONE term")

corpus_exclusive <- selectizeInput("corpus_exclude", choices = NULL, selected = "", multiple = TRUE, label = "Must not include terms")

corpus_data <- div(
  p("Number of docs: ", textOutput("corpus_size", inline = TRUE))
)

dash_sidebar <- dashboardSidebar(
  h3("Define target corpus"),
  stem_picker,
  core_corpus_selector,
  corpus_inclusive,
  corpus_or,
  corpus_exclusive,
  corpus_data,
  sidebarMenu(
    menuItem("Historical Term Frequency", tabName = "termsovertime", icon = icon("chart-line")),
    menuItem("TF-IDF", tabName = "corpus_tf_idf", icon = icon("sort-amount-down")),
    menuItem("Keyness", tabName = "keyness", icon = icon("chart-pie")),
    menuItem("Co-occurrence", tabName = "term_comparison", icon = icon("braille")),
    menuItem("KWIC", tabName = "kwic", icon = icon("underline"))
  )
)

dash_body <- dashboardBody(
  tabItems(
    termsovertime_tab,
    corpus_tf_idf_tab,
    keyness_tab,
    term_comparison_tab,
    kwic_tab
  )
)

dashboardPage(
  dash_header,
  dash_sidebar,
  dash_body,
  title = "AI and Ethics Corpus Explorer"
)
