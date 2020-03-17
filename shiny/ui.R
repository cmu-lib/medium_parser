library(shinydashboard)

# Define UI for application that draws a histogram
dash_header <- dashboardHeader(
  title = "Medium Corpus"
)

corpus_tf_idf_tab <- tabItem(
  tabName = "corpus_tf_idf",
  h2("TF-IDF of the corpus documents"),
  p("This view shows all the documents in the corpus, and shows the top 10 tokens for each document according to TF-IDF (terms that appear relatively frequently in that individual document, but rarely across the larger corpus, suggesting that they are terms distinctive to that document.)"),
  box(
    dataTableOutput("document_metadata"),
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
      plotOutput("termsovertime_chart"),
      width = 12,
      height = 650
    ),
    box(
      dataTableOutput("termsovertime_metadata"),
      title = "Documents with these tokens",
      width = 12
    )
  )
)

yearly_tfidf_tab <- tabItem(
  tabName = "yearly_tfidf",
  h2("TF-IDF by year"),
  p("This experiment runs TF-IDF again, but tries it on entire years, to find what words are distinctive to particular years.",
    strong("I am very dubious about this as an approach, but it was a cheap thing to try before diving in to topic modeling...")),
  list(
    box(
      dataTableOutput("yearly_tf_idf_table")
    )
  )
)

corpus_inclusive <- selectizeInput("corpus_include", choices = NULL, selected = "", multiple = TRUE, label = "Must include terms")

corpus_exclusive <- selectizeInput("corpus_exclude", choices = NULL, selected = "", multiple = TRUE, label = "Must not include terms")

corpus_data <- div(
  p("Number of docs: ", textOutput("corpus_size", inline = TRUE))
)

dash_sidebar <- dashboardSidebar(
  corpus_inclusive,
  corpus_exclusive,
  corpus_data,
  sidebarMenu(
    menuItem("Historical Term Frequency", tabName = "termsovertime", icon = icon("chart-line")),
    menuItem("TF-IDF", tabName = "corpus_tf_idf", icon = icon("sort-amount-down")),
    menuItem("Annual TF-IDF", tabName = "yearly_tfidf", icon = icon("calendar-check")),
    menuItem("Topic Models", tabName = "topic_models", icon = icon("object-group"))
  )
)

dash_body <- dashboardBody(
  tabItems(
    termsovertime_tab,
    corpus_tf_idf_tab,
    yearly_tfidf_tab
  )
)

dashboardPage(
  dash_header,
  dash_sidebar,
  dash_body,
  title = "AI and Ethics Corpus Explorer"
)
