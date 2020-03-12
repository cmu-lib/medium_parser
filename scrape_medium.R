library(tidyverse)
library(DBI)
library(rvest)
library(RSQLite)
library(progress)

# Download and store HTML ----
# Writes to SQLITE so we can download once and then have full HTML available for future parsing

url_list <- read_lines("links.txt") %>% 
  str_match("^(.+)\\?source") %>% .[,2] %>% 
  unique()

db <- dbConnect(SQLite(), "medium.sqlite3")
dbExecute(db, "PRAGMA foreign_keys = on")
dbExecute(db, "CREATE TABLE IF NOT EXISTS article(url TEXT UNIQUE NOT NULL, html TEXT NOT NULL)")

urls_to_fetch <- setdiff(url_list, dbGetQuery(db, "SELECT url FROM article")$url)

upb <- progress_bar$new(format = "  downloading :current [:bar]:percent eta: :eta",
                        total = length(urls_to_fetch), width = 60)
walk(urls_to_fetch, possibly(function(url) {
  htmlstring <- str_c(as.character(read_html(url)), collapse = "")
  dbAppendTable(db, "article", tibble(url = url, html = htmlstring))
  Sys.sleep(sample.int(4, size = 1)) # be polite
  upb$tick()
}, otherwise = NULL))

saveRDS(dbReadTable(db, "article"), file = "medium_html.rds")

# Parse HTML ----

source("parse_medium.R")

library(drake)
library(quanteda)

effect_size <- function (n_target, n_reference) {
  total_a <- sum(n_target)
  total_b <- sum(n_reference)
  percent_a <- ifelse(n_target == 0, 0.5 / total_a, n_target/total_a)
  percent_b <- ifelse(n_reference == 0, 0.5 / total_b, n_reference/total_b)
  log2(percent_a / percent_b)
}

medium_plan <- drake_plan(
  collected_articles = readRDS(file = file_in("medium_html.rds")),
  core_table = pmap_dfr(collected_articles, possibly(article_core_table, otherwise = NULL)),
  article_tags = pmap_dfr(collected_articles, possibly(article_tag_table, otherwise = NULL)),
  article_links = pmap_dfr(collected_articles, article_links_table),
  article_images = pmap_dfr(collected_articles, article_images_table),
  medium_corpus = core_table %>% 
    corpus(docid_field = "url", text_field = "text"),
  medium_tokens = medium_corpus %>% 
    tokens(what = "word", remove_puct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE, split_hyphens = FALSE) %>% 
    tokens_compound(pattern = phrase(c("artificial intelligence", "big data", "machine learning"))),
  medium_dfm = dfm(medium_tokens),
  ai_ethics_docs =  rownames(medium_dfm)[as.logical(medium_dfm[,"artificial_intelligence"]) & as.logical(medium_dfm[,"ethics"])],
  ai_no_ethics_docs = rownames(medium_dfm)[as.logical(medium_dfm[,"artificial_intelligence"]) & !(as.logical(medium_dfm[,"ethics"]))],
  ml_no_ethics_docs = rownames(medium_dfm)[as.logical(medium_dfm[,"machine_learning"]) & !(as.logical(medium_dfm[,"ethics"]))],
  ml_ethics_docs = rownames(medium_dfm)[as.logical(medium_dfm[,"machine_learning"]) & as.logical(medium_dfm[,"ethics"])],
  ethics_only_docs = rownames(medium_dfm)[as.logical(medium_dfm[,"ethics"]) & !(as.logical(medium_dfm[,"artificial_intelligence"])) & !(as.logical(medium_dfm[,"machine_learning"]))],
  trimmed_dfm = medium_dfm %>% 
    dfm_remove(stopwords("en")) %>% 
    dfm_trim(min_docfreq = 0.01, max_docfreq = 0.9, docfreq_type = "prop"),
  stemmed_dfm = trimmed_dfm %>% 
    dfm_wordstem(language = "en"),
  keyness = target(
    textstat_keyness(x, target = fac) %>%
      mutate(effect_size = effect_size(n_target, n_reference)) %>%
      filter(p < 0.05) %>%
      arrange(desc(effect_size)),
    transform = cross(
      x = list("unstemmed" = trimmed_dfm, "stemmed" = stemmed_dfm),
      fac = list(ai_ethics = ai_ethics_docs, 
                 ai_no_ethics = ai_no_ethics_docs, 
                 ml_ethics = ml_ethics_docs, 
                 ml_no_ethics = ml_no_ethics_docs, 
                 ethics_only = ethics_only_docs))
  )
)

make(medium_plan)
