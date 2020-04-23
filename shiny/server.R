library(shiny)
library(quanteda)
library(glue)
library(lubridate)
library(tidyverse)
library(tidytext)
library(htmlwidgets)
library(sparkline)
library(DT)
library(openxlsx)
library(cowplot)

function(input, output, session) {
  
  original_dfm <- reactive({
    req(input$stem_choices)
    if (input$stem_choices == "original") {
      stopped_dfm
    } else if (input$stem_choices == "stemmed") {
      stemmed_dfm
    }
  })
  
  observe({
    corpus_type <- input$stem_choices
    if (corpus_type == "original") {
      corpus_selectors <- c(
        "artifical intelligence" = "artificial_intelligence", 
        "ethics" = "ethics", 
        "machine learning" = "machine_learning", 
        "algorithm" = "algorithm", 
        "goveranance" = "governance", 
        "regulation" = "regulation", 
        "big data" = "big_data",
        "design" = "design",
        "ux" = "ux",
        "user experience" = "user_experience",
        "ixd" = "ixd",
        "ixda" = "ixda")
    } else if (corpus_type == "stemmed") {
      corpus_selectors <- c(
        "artifici_intellig" = "artifici_intellig", 
        "ethic" = "ethic", 
        "machin_learn" = "machin_learn", 
        "algorithm" = "algorithm", 
        "govern" = "govern", 
        "regul" = "regul", 
        "big_data" = "big_data",
        "design" = "design",
        "ux" = "ux",
        "user_experi" = "user_experi",
        "ixd" = "ixd",
        "ixda" = "ixda")
    }
    
    # add counts to corpus selectors
    reduced_dfm <- original_dfm() %>% 
      dfm_match(corpus_selectors)
    
    selector_binary <- reduced_dfm > 0
    
    selector_counts <- colSums(selector_binary)
    
    names(corpus_selectors) <- str_c(names(corpus_selectors), " (", selector_counts, ")", sep = "")
    
    updateSelectizeInput(session, "available_corpora",
                         choices = corpus_selectors,
                         selected = "algorithm",
                         server = TRUE)
    updateSelectizeInput(session, "reference_corpora",
                         choices = corpus_selectors,
                         selected = "algorithm",
                         server = TRUE)
  })
  
  base_dfm <- reactive({
    req(input$available_corpora)
    selected_corpora_terms <- dfm_match(original_dfm(), input$available_corpora)
    keep_docs <- rownames(selected_corpora_terms)[rowSums(selected_corpora_terms) > 0]
    original_dfm()[keep_docs,]
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
  
  or_filtered_corpus <- reactive({
    if (!is.null(input$corpus_or)) {
      reduced_dfm <- dfm_match(base_dfm(), input$corpus_or)
      filtered_corpus <- rownames(reduced_dfm)[rowSums(reduced_dfm) > 0]
      return(intersect(inclusive_filtered_corpus(), filtered_corpus))
    }
    inclusive_filtered_corpus()
  })
  
  # Document IDs that must be excluded
  exclusive_filtered_corpus <- reactive({
    if (!is.null(input$corpus_exclude)) {
      reduced_dfm <- dfm_match(base_dfm(), input$corpus_exclude)
      filtered_corpus <- rownames(reduced_dfm)[rowSums(reduced_dfm) > 0]
      return(setdiff(or_filtered_corpus(), filtered_corpus))
    }
    or_filtered_corpus()
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
    sort(colnames(original_dfm()))
  })
  
  # Update token selectize menus based on selected corpora tokens
  observe({
    x <- token_choices()
    updateSelectizeInput(session, "corpus_include",
                         choices = x,
                         selected = NULL,
                         server = TRUE)
    updateSelectizeInput(session, "corpus_or",
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
    updateSelectizeInput(session, "keyness_or",
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
  
  output$document_metadata <- DT::renderDataTable({
    print(corpus_metadata())
    corpus_tfidf() %>%
      inner_join(corpus_metadata(), by = c("document" = "url")) %>%
      mutate(
        date = ymd_hms(date_published),
        title = glue("<a href='{document}' target='_blank' rel='noopener noreferrer'>{title}</a>"),
        publisher = glue("<a href='{publisher_url}' target='_blank' rel='noopener noreferrer'>{publisher_name}</a>")
      ) %>% 
      select(author_name, title, date_published, publisher, top_terms)
  }, escape = FALSE, options = list(searching = FALSE, pageLength = 100))
  
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
    req(input$wordchart_tokens)
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
    docfreq_data <- corpus_metadata() %>% 
      ggplot(aes(x = date_published)) +
      geom_histogram(bins = 100) +
      theme_minimal()
    termfreq_data <- termsovertime_data() %>%
      ggplot(aes(x = approx_date, y = percent_total, color = term)) +
      geom_line(size = 2) +
      scale_color_brewer(palette = "Dark2") +
      theme_minimal() +
      theme(legend.position = "bottom")
    plot_grid(docfreq_data, termfreq_data, nrow = 2, rel_heights = c(1, 2), axis = "lbrt", align = "hv")
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
  
  output$termsovertime_metadata <- DT::renderDataTable({
    termsovertime_metadata()
  }, escape = FALSE, options = list(pageLength = 100, searching = FALSE))
  
  # KWIC ----
  
  kwic_table <- reactive({
    withProgress({
      req(input$kwic_tokens)
      kwic_pattern <- str_c(input$kwic_tokens, "*", sep = "")
      
      kwic(filtered_corpus(), kwic_pattern, window = 15) %>% 
        select(docname, pre, keyword, post) %>% 
        group_by(docname) %>% 
        summarize(phrases = str_c(pre, "<strong>", keyword, "</strong>", post, sep = " ", collapse = "<br/><br/>")) %>% 
        mutate(docname = glue("<a href='{docname}' target='_blank' rel='noopener noreferrer'>{docname}</a>"))
    }, message = "Finding keyword in context")
  })
  
  output$kwic_table <- DT::renderDataTable({
    kwic_table()
  }, escape = FALSE, options = list(pageLength = 100, searching = FALSE))
  
  # Keyness ----
  
  effect_size <- function (n_target, n_reference) {
    total_a <- sum(n_target)
    total_b <- sum(n_reference)
    percent_a <- ifelse(n_target == 0, 0.5 / total_a, n_target/total_a)
    percent_b <- ifelse(n_reference == 0, 0.5 / total_b, n_reference/total_b)
    log2(percent_a / percent_b)
  }
  
  # Define a reference corpus
  
  reference_base_dfm <- reactive({
    req(input$reference_corpora)
    selected_corpora_terms <- dfm_match(original_dfm(), input$reference_corpora)
    keep_docs <- rownames(selected_corpora_terms)[rowSums(selected_corpora_terms) > 0]
    original_dfm()[keep_docs,]
  })
  
  # Document IDs that must be kept in
  inclusive_reference_corpus <- reactive({
    if (!is.null(input$keyness_include)) {
      reduced_dfm <- dfm_match(reference_base_dfm(), input$keyness_include)
      filtered_corpus <- rownames(reduced_dfm)[rowSums(reduced_dfm > 0) == ncol(reduced_dfm)]
      return(filtered_corpus)
    }
    rownames(reference_base_dfm())
  })
  
  or_reference_corpus <- reactive({
    if (!is.null(input$keyness_or)) {
      reduced_dfm <- dfm_match(reference_base_dfm(), input$keyness_or)
      filtered_corpus <- rownames(reduced_dfm)[rowSums(reduced_dfm) > 0]
      return(intersect(inclusive_reference_corpus(), filtered_corpus))
    }
    inclusive_reference_corpus()
  })
  
  # Document IDs that must be excluded
  exclusive_reference_corpus <- reactive({
    if (!is.null(input$keyness_exclude)) {
      reduced_dfm <- dfm_match(reference_base_dfm(), input$keyness_exclude)
      filtered_corpus <- rownames(reduced_dfm)[rowSums(reduced_dfm) > 0]
      return(setdiff(or_reference_corpus(), filtered_corpus))
    }
    or_reference_corpus()
  })
  
  keyness_reference_ids <- reactive({
    exclusive_reference_corpus()
  })
  
  reference_dfm <- reactive({
    reference_base_dfm()[keyness_reference_ids(),]
  })
  
  output$reference_corpus_size <- renderText({
    format(nrow(reference_dfm()), big.mark = ",")
  })
  
  combined_dfm <- reactive({
    union_names <- union(rownames(reference_dfm()), rownames(filtered_dfm()))
    rbind(reference_dfm(), filtered_dfm())[union_names,] %>% 
      dfm_remove(c(input$keyness_include, input$keyness_exclude, input$reference_corpora, input$available_corpora))
  })
  
  combined_metadata <- reactive({
    core_table %>% 
      filter(url %in% rownames(combined_dfm())) %>% 
      mutate(approx_date = round_date(date_published, "halfyear"))
  })
  
  split_corpus <- reactive({
    rownames(combined_dfm()) %>% 
      split(combined_metadata()$approx_date)
  })
  
  keyness_stats_central <- reactive({
    textstat_keyness(combined_dfm(), target = filtered_corpus_ids(), measure = "lr") %>% 
      mutate(es = effect_size(n_target, n_reference)) %>% 
      filter(p <= 0.5) %>% 
      arrange(desc(G2))
  })
  
  keyness_stats_time <- reactive({
    withProgress({
      split_corpus() %>% 
        imap_dfr(function(sc, datestring) {
          incProgress(1/length(split_corpus))
          combined_split <- combined_dfm()[sc,]
          target_split <- intersect(filtered_corpus_ids(), sc)
          
          tryCatch({
            textstat_keyness(combined_split, target = target_split, measure = "lr") %>%
              mutate(es = effect_size(n_target, n_reference))
          }, error = function(e) {
            # If an invalid comparison comes up for a given time period, produce an empty table
            tibble(feature = colnames(combined_split), 
                   G2 = NA_real_, 
                   p = NA_real_, 
                   n_target = NA_integer_, 
                   n_reference = NA_integer_, 
                   es = NA_real_)
          })
        }, .id = "date") %>% 
        mutate(date = ymd(date)) %>% 
        # Expand to include all combos, even when feature had null effect size. This is critical for calculating the sparkline
        tidyr::complete(feature, date) %>%
        mutate(
          es = dplyr::coalesce(es, 0),
          G2 = dplyr::coalesce(G2, 0)) %>% 
        group_by(feature) %>% 
        arrange(date) %>% 
        summarize(
          effect_timeline = spk_chr(es, type = "bar"),
          g2_timeline = spk_chr(G2, type = "bar")
        )
    }, message = "Calculating keyness...", value = 0)
  })
  
  keyness_stats <- reactive({
    keyness_stats_central()
  })
  
  
  output$keyness_table <- DT::renderDataTable({
    if (input$keynessButton == 0)
      return()
    
    isolate({
      if (length(setdiff(rownames(combined_dfm()), filtered_corpus_ids())) > 0) {
        datatable(keyness_stats(), escape = FALSE, options = list(
          searching = FALSE,
          pageLength = 100,
          fnDrawCallback = htmlwidgets::JS(
            'function(){
  HTMLWidgets.staticRender();
}'
          ))) %>% spk_add_deps()
      } else {
        stop(safeError("The reference corpus must contain some documents not in the target corpus. Right now, one the target corpus completely overlaps the reference corpus. Adjust filters on the sidebar or in the reference corpus definition to change the subset of documents you are comparing."))
      }
    })
  })
  
  keyword_summary <- reactive({
    collapsed_available_corpora <- str_c(input$available_corpora, collapse = '; ')
    collapsed_corpus_include <- str_c(input$corpus_include, collapse = '; ')
    collapsed_corpus_or <- str_c(input$corpus_or, collapse = '; ')
    collapsed_corpus_exclude <- str_c(input$corpus_exclude, collapse = '; ')
    collapsed_reference_corpora <- str_c(input$reference_corpora, collapse = '; ')
    collapsed_keyness_include <- str_c(input$keyness_include, collapse = '; ')
    collapsed_keyness_or <- str_c(input$keyness_or, collapse = '; ')
    collapsed_keyness_exclude <- str_c(input$keyness_exclude, collapse = '; ')
    
    tibble(
      option = c("Tokens",
                 "Target corpus:",
                 "- Original medium search terms:",
                 "- Includes all:",
                 "- Includes any:",
                 "- Excludes any:",
                 "Reference corpus:",
                 "- Original medium search terms:",
                 "- Includes all:",
                 "- Includes any:",
                 "- Excludes any:"),
      values = c(
        input$stem_choices,
        format(nrow(corpus_metadata()), big.mark = ','),
        collapsed_available_corpora[1],
        collapsed_corpus_include[1],
        collapsed_corpus_or[1],
        collapsed_corpus_exclude[1],
        format(nrow(combined_dfm()), big.mark = ','),
        collapsed_reference_corpora[1],
        collapsed_keyness_include[1],
        collapsed_keyness_or[1],
        collapsed_keyness_exclude[1]
      )
    )
  })
  
  output$keyword_summary <- renderTable({
    keyword_summary()
  })
  
  target_authors <- reactive({
    target_authors <- corpus_metadata() %>% 
      count(author_name, author_url, sort = TRUE)
  })
  
  reference_authors <- reactive({
    core_table %>% 
      filter(url %in% keyness_reference_ids()) %>% 
      count(author_name, author_url, sort = TRUE)
  })
  
  format_authors_table <- function(dt) {
    dt %>% 
      mutate(author_name = glue("<a href='{author_url}' target='_blank' rel='noopener noreferrer'>{author_name}</a>")) %>% 
      select(posts = n, author = author_name)
  }
  
  output$target_authors_table <- DT::renderDataTable({
    format_authors_table(target_authors()) 
  }, escape = FALSE, options = list(pageLength = 100, searching = FALSE))
  
  output$reference_authors_table <- DT::renderDataTable({
    format_authors_table(reference_authors()) 
  }, escape = FALSE, options = list(pageLength = 100, searching = FALSE))
  
  download_filename <- reactive({
    datetime <- str_replace_all(Sys.time(), " ", "-")
    glue("keyness_{datetime}.xlsx")
  })
  
  output$keyness_report <- downloadHandler(
    filename = download_filename(),
    content = function(file) {
      l <- list("keyness" = keyness_stats(), 
                "target_authors" = target_authors(), 
                "reference_authors" = reference_authors(), 
                "settings" = keyword_summary())
      write.xlsx(l, file = file)
    }
  )
}
