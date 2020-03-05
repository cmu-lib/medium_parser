library(tidyverse)
library(DBI)
library(rvest)
library(RSQLite)
library(progress)

# Download and store HTML ----
# Writes to SQLITE so we can download once and then have full HTML available for future parsing

url_list <- read_lines("links.txt")

db <- dbConnect(SQLite(), "medium.sqlite3")
dbExecute(db, "CREATE TABLE IF NOT EXISTS article(url TEXT UNIQUE NOT NULL, html TEXT NOT NULL)")
urls_to_fetch <- setdiff(url_list, dbGetQuery(db, "SELECT url FROM article")$url)

upb <- progress_bar$new(format = "  downloading [:bar] :percent eta: :eta",
                        total = length(urls_to_fetch), width = 60)
walk(urls_to_fetch, function(url) {
  htmlstring <- str_c(as.character(read_html(url)), collapse = "")
  dbAppendTable(db, "article", tibble(url = url, html = htmlstring))
  Sys.sleep(sample.int(4, size = 1)) # be polite
  upb$tick()
})

collected_articles <- dbReadTable(db, "article")

dbDisconnect(db)

# Parse HTML ----

source("parse_medium.R")

core_table <- pmap_dfr(collected_articles, article_core_table)
article_tags <- pmap_dfr(collected_articles, article_tag_table)
article_links <- pmap_dfr(collected_articles, article_links_table)
article_images <- pmap_dfr(collected_articles, article_images_table)

library(tidytext)

medium_tokens <- core_table %>% 
  unnest_tokens(output = "word", input = "text") %>% 
  anti_join(get_stopwords(language = "en"), by = "word") %>% 
  count(url, word, name = "word_count") %>% 
  bind_tf_idf(term = "word", document = "url", n = "word_count")

medium_tf_idf <- medium_tokens %>% 
  arrange(url, desc(tf_idf)) %>% 
  group_by(url) %>% 
  filter(row_number() <= 15) %>% 
  summarize(terms = str_c(word, collapse = ", "))
