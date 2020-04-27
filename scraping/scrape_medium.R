library(tidyverse)
library(DBI)
library(rvest)
library(RSQLite)
library(progress)

# Download and store HTML ----
# Writes to SQLITE so we can download once and then have full HTML available for future parsing

url_list <- read_lines("scraping/links.txt") %>% 
  str_match("^(.+)\\?source") %>% .[,2] %>% 
  unique() %>% 
  na.omit()

db <- dbConnect(SQLite(), "scraping/medium.sqlite3")
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

saveRDS(dbReadTable(db, "article"), file = "scraping/medium_html.rds")
write_lines(dbGetQuery(db, "SELECT url FROM article ORDER BY url")$url, path = "scraping/links.txt")
