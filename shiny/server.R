library(shiny)
library(quanteda)
library(glue)
library(lubridate)
library(tidyverse)
library(tidytext)

function(input, output, session) {
  
  base_dfm <- reactive({
    stopped_dfm
  })
  
  # Document IDs that must be kept in
  inclusive_filtered_corpus <- reactive({
    if (!is.null(input$corpus_include)) {
      reduced_dfm <- dfm_match(base_dfm(), input$corpus_include)
      inclusive_filtered_corpus <- rownames(reduced_dfm)[rowSums(reduced_dfm) > 0]
      return(inclusive_filtered_corpus)
    }
    rownames(base_dfm())
  })
  
  # Document IDs that must be excluded
  exclusive_filtered_corpus <- reactive({
    if (!is.null(input$corpus_exclude)) {
      reduced_dfm <- dfm_match(base_dfm(), input$corpus_exclude)
      filtered_corpus <- rownames(reduced_dfm)[rowSums(reduced_dfm) > 0]
      return(setdiff(inclusive_filtered_corpus(), filtered_corpus))
    }
    inclusive_filtered_corpus()
  })
  
  filtered_corpus_ids <- reactive({
    exclusive_filtered_corpus()
  })
  
  filtered_dfm <- reactive({
    base_dfm()[filtered_corpus_ids(),]
  })
  
  filtered_corpus <- reactive({
    base_dfm()[filtered_corpus_ids()]
  })
  
  corpus_metadata <- reactive({
    core_table %>%
      filter(date_published >= ymd(20150101)) %>% 
      filter(url %in% filtered_corpus_ids())
  })
  
  output$corpus_size <- renderText({
    format(nrow(corpus_metadata()), big.mark = ",")
  })
  
  token_choices <- reactive({
    sort(colnames(base_dfm()))
  })
  
  # Update token selectize menus based on selected corpora tokens
  observe({
    x <- token_choices()
    updateSelectizeInput(session, "corpus_include",
                         choices = x,
                         selected = NULL,
                         server = TRUE)
    updateSelectizeInput(session, "corpus_exclude",
                         choices = x,
                         selected = NULL,
                         server = TRUE)
    updateSelectizeInput(session, "wordchart_tokens",
                         choices = x,
                         selected = c("regulation", "big_data"),
                         server = TRUE)
  })
  
  # TF-IDF ----
  
  corpus_tfidf <- reactive({
    withProgress({
      filtered_dfm() %>% 
        dfm_tfidf(scheme_tf = "prop") %>% 
        tidy() %>% 
        group_by(document) %>%
        filter(row_number(desc(count)) <= 10) %>%
        arrange(desc(count)) %>%
        summarize(top_terms = str_c(term, collapse = ", "))
    }, message = "Calculating TF-IDF")
  })
  
  output$document_metadata <- renderDataTable({
    corpus_tfidf() %>%
      inner_join(corpus_metadata(), by = c("document" = "url")) %>%
      mutate(
        date = ymd_hms(date_published),
        title = glue("<a href='{document}' target='_blank' rel='noopener noreferrer'>{title}</a>"),
        publisher = glue("<a href='{publisher_url}' target='_blank' rel='noopener noreferrer'>{publisher_name}</a>")
      ) %>% 
      select(author_name, title, date_published, publisher, top_terms)
  }, escape = FALSE)
  
  # termsovertime ----
  
  timegrouped_corpus <- reactive({
    withProgress({
      corpus_metadata() %>%
        select(url, date_published) %>% 
        mutate(approx_date = round_date(date_published, "halfyear")) %>%
        group_by(approx_date) %>%
        mutate(total_docs = n_distinct(url)) %>%
        ungroup()
    }, message = "Grouping docs into half-years")
  })
  
  single_termsovertime <- reactive({
    filtered_dfm() %>% 
      dfm_match(input$wordchart_tokens) %>% 
      tidy() %>% 
      left_join(timegrouped_corpus(), by = c("document" = "url")) %>% 
      filter(count > 0)
  })
  
  termsovertime_data <- reactive({
    single_termsovertime() %>% 
      group_by(approx_date, term) %>%
      summarize(
        percent_total = n() / first(total_docs)
      )
  })
  
  output$termsovertime_chart <- renderPlot({
    req(input$wordchart_tokens)
    termsovertime_data() %>%
      ggplot(aes(x = approx_date, y = percent_total, color = term)) +
      geom_line() +
      theme_minimal()
  }, height = 600)
  
  termsovertime_metadata <- reactive({
    single_termsovertime() %>%
      distinct(document) %>% 
      left_join(core_table, by = c("document" = "url")) %>% 
      mutate(
        title = glue("<a href='{document}' target='_blank' rel='noopener noreferrer'>{title}</a>")
      ) %>% 
      select(author_name, title, date_published, publisher_name)
  })
  
  output$termsovertime_metadata <- renderDataTable({
    termsovertime_metadata()
  }, escape = FALSE)
  
  # # Annual TF-IDF ----
  # 
  # yearly_tokens <- reactive({
  #   filtered_corpus() %>%
  #     left_join(corpus_metadata(), by = "document_id") %>%
  #     filter(year > 2013) %>%
  #     group_by(year, gram) %>%
  #     summarize(nn = sum(n))
  # })
  # 
  # yearly_tf_idf <- reactive({
  #   yearly_tokens() %>%
  #     bind_tf_idf(gram, year, nn)
  # })
  # 
  # output$yearly_tf_idf_table <- renderDataTable({
  #   withProgress({
  #     # yearly_tf_idf()
  #     yearly_tf_idf() %>%
  #       group_by(year) %>%
  #       arrange(desc(tf_idf)) %>%
  #       filter(row_number(desc(tf_idf)) <= 40) %>%
  #       summarize(top_terms = str_c(gram, collapse = ", "))
  #   }, message = "Calculating annual TF-IDF")
  # }, escape = FALSE)
  # 
  # # Topic models ----
  # 
  # corpus_tm <- reactive({
  #   # withProgress({
  #   #   req(input$corpus_menu)
  #   #   tm_key <- corpus_rdata %>%
  #   #     filter(corpus_id == local(input$corpus_menu),  object_type == "topic-model") %>%
  #   #     collect() %>%
  #   #     pull(storr_key)
  #   #   st$get(corpus_key)
  #   # }, message = "Loading topic model from disk")
  #   corpus_id <- input$corpus_menu
  #   n_topics <- input$n_topics
  #   st$get(glue("analysis_dfm{corpus_id}_{n_topics}"))
  # })
  # 
  # tm_terms <- reactive({
  #   as.list((as.data.frame(terms(corpus_tm(), 20)))) %>% unname()
  # })
  # 
  # tm_docs <- reactive({
  #   posterior(corpus_tm())[["topics"]]
  # })
  # 
  # output$tm_html <- renderUI({
  #   token_summaries <- shinydashboard::box(
  #     title = "Topic terms",
  #     purrr::imap(tm_terms(), function(x, i) {
  #       p(glue("Topic {i}: "), str_c(x, collapse = ", "))
  #     })
  #   )
  #   
  #   full_tables <- doc_tables <- purrr::imap(tm_terms(), function(x, i) {
  #     docs_ranking <- sort(min_rank(desc(tm_docs()[,i])))
  #     
  #     top_docs <- tibble(document_id = as.integer(names(docs_ranking))) %>%
  #       inner_join(corpus_metadata(), by = "document_id") %>%
  #       select(item_title, doc_type, authors, parent_title, date, url) %>%
  #       slice(1:20) %>%
  #       mutate(
  #         date = ymd(date),
  #         url = glue("<a href='{url}' target='_blank' rel='noopener noreferrer'>{url}</a>")
  #       )
  #     
  #     docstring <- str_c(x, collapse = ", ")
  #     
  #     shinydashboard::box(
  #       title = glue("Topic {i}: {docstring}"),
  #       DT::datatable(top_docs, escape = FALSE),
  #       width = 12,
  #       collapsible = TRUE,
  #       collapsed = FALSE
  #     )
  #   })
  #   
  #   list(
  #     token_summaries,
  #     full_tables
  #   )
  # })
  
  
}
