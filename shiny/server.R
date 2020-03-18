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
  
  output$include_string <- renderText({
    str_c(input$corpus_include, collapse = "; ")
  })
  
  output$exclude_string <- renderText({
    str_c(input$corpus_exclude, collapse = "; ")
  })
  
  # Document IDs that must be kept in
  inclusive_filtered_corpus <- reactive({
    if (!is.null(input$corpus_include)) {
      reduced_dfm <- dfm_match(base_dfm(), input$corpus_include)
      filtered_corpus <- rownames(reduced_dfm)[rowSums(reduced_dfm > 0) == ncol(reduced_dfm)]
      return(filtered_corpus)
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
    medium_corpus[filtered_corpus_ids()]
  })
  
  corpus_metadata <- reactive({
    core_table %>%
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
    updateSelectizeInput(session, "kwic_tokens",
                         choices = x,
                         selected = NULL,
                         server = TRUE)
    updateSelectizeInput(session, "keyness_include",
                         choices = x,
                         selected = NULL,
                         server = TRUE)
    updateSelectizeInput(session, "keyness_exclude",
                         choices = x,
                         selected = NULL,
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
      left_join(timegrouped_corpus(), by = c("document" = "url"))
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
  
  # KWIC ----
  
  kwic_table <- reactive({
    withProgress({
      req(input$kwic_tokens)
      kwic_pattern <- input$kwic_tokens
      
      kwic(filtered_corpus(), kwic_pattern, window = 15) %>% 
        select(docname, pre, keyword, post) %>% 
        group_by(docname) %>% 
        summarize(phrases = str_c(pre, "<strong>", keyword, "</strong>", post, sep = " ", collapse = "<br/><br/>")) %>% 
        mutate(docname = glue("<a href='{docname}'>{docname}</a>"))
    }, message = "Finding keyword in context")
  })
  
  output$kwic_table <- renderDataTable({
    kwic_table()
  }, escape = FALSE)
  
  # Keyness ----
  
  effect_size <- function (n_target, n_reference) {
    total_a <- sum(n_target)
    total_b <- sum(n_reference)
    percent_a <- ifelse(n_target == 0, 0.5 / total_a, n_target/total_a)
    percent_b <- ifelse(n_reference == 0, 0.5 / total_b, n_reference/total_b)
    log2(percent_a / percent_b)
  }
  
  # Document IDs that must be kept in
  inclusive_reference_corpus <- reactive({
    if (!is.null(input$keyness_include)) {
      reduced_dfm <- dfm_match(base_dfm(), input$keyness_include)
      filtered_corpus <- rownames(reduced_dfm)[rowSums(reduced_dfm > 0) == ncol(reduced_dfm)]
      return(filtered_corpus)
    }
    rownames(base_dfm())
  })
  
  # Document IDs that must be excluded
  exclusive_reference_corpus <- reactive({
    if (!is.null(input$keyness_exclude)) {
      reduced_dfm <- dfm_match(base_dfm(), input$keyness_exclude)
      filtered_corpus <- rownames(reduced_dfm)[rowSums(reduced_dfm) > 0]
      return(setdiff(inclusive_filtered_corpus(), filtered_corpus))
    }
    inclusive_reference_corpus()
  })
  
  keyness_reference_ids <- reactive({
    exclusive_reference_corpus()
  })
  
  reference_dfm <- reactive({
    base_dfm()[keyness_reference_ids(),]
  })
  
  censored_dfm <- reactive({
    trimmed_dfm %>% 
      dfm_remove(input$corpus_include) %>% 
      dfm_remove(input$corpus_exclude)
  })
  
  combined_dfm <- reactive({
    union_names <- union(rownames(reference_dfm()), rownames(censored_dfm()))
    rbind(reference_dfm(), censored_dfm())[union_names,]
  })
  
  plot_keyness <- reactive({
    nrow(combined_dfm() != length(filtered_corpus_ids()))
  })
  
  keyness_stats <- reactive({
    req(plot_keyness())
    textstat_keyness(combined_dfm(), target = filtered_corpus_ids(), measure = "lr") %>%
      mutate(effect_size = effect_size(n_target, n_reference)) %>%
      filter(p < 0.05) %>%
      arrange(desc(effect_size))
  })
  
  output$keyness_table <- renderDataTable({
      keyness_stats()
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
