library(tidyverse)
library(DBI)
library(RSQLite)
library(progress)
library(glue)

url_list <- read_lines("links.txt")

db <- dbConnect(SQLite(), "medium.sqlite3")
dbExecute(db, "CREATE TABLE IF NOT EXISTS article(url TEXT UNIQUE NOT NULL, html TEXT NOT NULL)")

# load links first time
urls_to_fetch <- setdiff(url_list, dbGetQuery(db, "SELECT url FROM article")$url)

upb <- progress_bar$new(format = "  downloading [:bar] :percent eta: :eta",
                        total = length(urls_to_fetch), clear = FALSE, width= 60)
walk(urls_to_fetch, function(url) {
  upb$tick()
  htmlstring <- str_c(as.character(read_html(url)), collapse = "")
  dbAppendTable(db, "article", tibble(url = url, html = htmlstring))
  })

collected_articles <- dbReadTable(db, "article")

dbDisconnect(db)

