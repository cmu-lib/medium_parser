# Drake build plan to analyze medium posts

library(tidyverse)
library(drake)
library(quanteda)
library(rmarkdown)

source("scraping/parse_medium.R")

exclude_articles <- c(
  # This "article" is an aggregate of a lot of medium posts so skews us like crazy
  "https://medium.com/hackernoon/top-150-medium-articles-related-with-big-data-data-science-and-data-visualization-803773728ff7"
)

medium_plan <- drake_plan(
  collected_articles = readRDS(file = file_in("scraping/medium_html.rds")),
  named_entities = read_lines(file = file_in("scraping/hand_cleaned_names.txt")) %>% 
    str_extract("[a-zA-Z ]+") %>% 
    str_trim() %>% 
    str_to_lower(),
  core_table = pmap_dfr(collected_articles, possibly(article_core_table, otherwise = NULL)) %>% 
    filter(date_published >= ymd(20150101)),
  article_tags = pmap_dfr(collected_articles, possibly(article_tag_table, otherwise = NULL)),
  article_links = pmap_dfr(collected_articles, article_links_table),
  article_images = pmap_dfr(collected_articles, article_images_table),
  compound_tokens = union(c("artificial intelligence", "big data", "machine learning", "user experience"), named_entities),
  medium_corpus = core_table %>% 
    filter(!(url %in% exclude_articles)) %>% 
    corpus(docid_field = "url", text_field = "text"),
  medium_tokens = medium_corpus %>% 
    tokens(what = "word", remove_symbols = TRUE, remove_punct = TRUE, remove_numbers = TRUE, split_hyphens = FALSE) %>% 
    tokens_compound(pattern = phrase(compound_tokens)),
  medium_dfm = dfm(medium_tokens),
  stopped_dfm = medium_dfm %>% 
    dfm_remove(stopwords("en")) %>% 
    dfm_remove(stopwords("es")) %>% 
    dfm_trim(min_docfreq = 4, termfreq_type = "count"),
  trolley_try = stopped_dfm[,"trolley"],
  stemmed_dfm = stopped_dfm %>% 
    dfm_wordstem(language = "en"),
  shiny_data = save(stopped_dfm, stemmed_dfm, medium_corpus, core_table, file = file_out("shiny/data.rda"))
)

make(medium_plan)
