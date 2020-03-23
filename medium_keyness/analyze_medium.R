# Drake build plan to analyze medium posts

library(tidyverse)
library(drake)
library(quanteda)
library(rmarkdown)

source("scraping/parse_medium.R")

effect_size <- function (n_target, n_reference) {
  total_a <- sum(n_target)
  total_b <- sum(n_reference)
  percent_a <- ifelse(n_target == 0, 0.5 / total_a, n_target/total_a)
  percent_b <- ifelse(n_reference == 0, 0.5 / total_b, n_reference/total_b)
  log2(percent_a / percent_b)
}

exclude_articles <- c(
  # This "article" is an aggregate of a lot of medium posts so skews us like crazy
  "https://medium.com/hackernoon/top-150-medium-articles-related-with-big-data-data-science-and-data-visualization-803773728ff7"
)

medium_plan <- drake_plan(
  collected_articles = readRDS(file = file_in("scraping/medium_html.rds")),
  core_table = pmap_dfr(collected_articles, possibly(article_core_table, otherwise = NULL)) %>% 
    filter(date_published >= ymd(20150101)),
  article_tags = pmap_dfr(collected_articles, possibly(article_tag_table, otherwise = NULL)),
  article_links = pmap_dfr(collected_articles, article_links_table),
  article_images = pmap_dfr(collected_articles, article_images_table),
  medium_corpus = core_table %>% 
    filter(!(url %in% exclude_articles)) %>% 
    corpus(docid_field = "url", text_field = "text"),
  medium_tokens = medium_corpus %>% 
    tokens(what = "word", remove_symbols = TRUE, remove_punct = TRUE, remove_numbers = TRUE, split_hyphens = FALSE) %>% 
    tokens_compound(pattern = phrase(c("artificial intelligence", "big data", "machine learning"))) %>% 
    tokens_replace(pattern = "ethical", replacement = "ethics"),
  medium_dfm = dfm(medium_tokens),
  regulation_docs = rownames(medium_dfm)[as.logical(medium_dfm[,"regulation"])],
  big_data_docs = rownames(medium_dfm)[as.logical(medium_dfm[,"big_data"])],
  governance_docs = rownames(medium_dfm)[as.logical(medium_dfm[,"governance"])],
  ai_docs = rownames(medium_dfm)[(as.logical(medium_dfm[,"artificial_intelligence"]) | as.logical(medium_dfm[,"a.i"]) | as.logical(medium_dfm[,"ai"]))],
  ethics_docs = rownames(medium_dfm)[(as.logical(medium_dfm[,"ethics"]))],
  ml_docs = rownames(medium_dfm)[as.logical(medium_dfm[,"machine_learning"])],
  algorithm_docs = rownames(medium_dfm)[(as.logical(medium_dfm[,"algorithm"]) | as.logical(medium_dfm[,"algorithms"]))],
  ai_ethics_docs = intersect(ai_docs, ethics_docs),
  ai_no_ethics_docs = setdiff(ai_docs, ethics_docs),
  ethics_only_docs = setdiff(ethics_docs, c(ai_docs, ml_docs)),
  ml_ethics_docs = intersect(ml_docs, ethics_docs),
  ml_no_ethics_docs = setdiff(ml_docs, ethics_docs),
  ai_governance_docs = intersect(ai_docs, governance_docs),
  ai_no_governance_docs = setdiff(ai_docs, governance_docs),
  stopped_dfm = medium_dfm %>% 
    dfm_remove(stopwords("en")) %>% 
    dfm_remove(stopwords("es")) %>% 
    dfm_trim(min_docfreq = 0.01, max_docfreq = 0.9, docfreq_type = "prop"),
  stemmed_dfm = stopped_dfm %>% 
    dfm_wordstem(language = "en"),
  shiny_data = save(stopped_dfm, stemmed_dfm, medium_corpus, core_table, file = file_out("shiny/data.rda"))
)

make(medium_plan)
