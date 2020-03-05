library(rvest)
library(jsonlite)



article_html <- read_html(collected_articles[1, "html"])
article_html <- read_html(collected_articles[2, "html"])

body_text <- article_html %>% 
  xml_nodes("p") %>% 
  xml_text() %>% 
  str_c(collapse = " ")

body_links <- article_html %>% 
  xml_nodes("p > a") %>% 
  xml_attr("href")

article_data <- article_html %>% 
  xml_nodes("script") %>% 
  .[10] %>% 
  xml_text() %>% 
  str_sub(26) %>% 
  fromJSON

article_author <- article_data

jsonedit(article_data)
