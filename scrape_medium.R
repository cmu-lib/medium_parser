library(tidyverse)
library(DBI)
library(rvest)
library(RSQLite)
library(progress)

source("parse_medium.R")

url_list <- read_lines("links.txt")

db <- dbConnect(SQLite(), "medium.sqlite3")
dbExecute(db, "CREATE TABLE IF NOT EXISTS article(url TEXT UNIQUE NOT NULL, html TEXT NOT NULL)")
urls_to_fetch <- setdiff(url_list, dbGetQuery(db, "SELECT url FROM article")$url)

upb <- progress_bar$new(format = "  downloading [:bar] :percent eta: :eta",
                        total = length(urls_to_fetch), width = 60)
walk(urls_to_fetch, function(url) {
  htmlstring <- str_c(as.character(read_html(url)), collapse = "")
  dbAppendTable(db, "article", tibble(url = url, html = htmlstring))
  Sys.sleep(1) # be polite
  upb$tick()
})

collected_articles <- dbReadTable(db, "article")

dbDisconnect(db)

core_table <- pmap_dfr(collected_articles, function(url, html) article_core_table(url, html))

article_tags <- pmap_dfr(collected_articles, function(url, html) article_tag_table(url, html))

article_links <- pmap_dfr(collected_articles, function(url, html) article_links_table(url, html))

