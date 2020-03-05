library(rvest)
library(jsonlite)
library(lubridate)

get_body_text <- function(article_html, collapse = "\n") {
  article_html %>% 
    xml_nodes(xpath = "//article//p|//article//h1|//article//h2|//article//h3|//article//figcaption") %>% 
    xml_text() %>% 
    str_c(collapse = collapse)
}

get_body_links <- function(article_html) {
  article_html %>% 
    xml_nodes(css = "p > a") %>% 
    xml_attr("href")
}

get_article_metadata <- function(article_html) {
  article_html %>% 
    xml_node(xpath = "//script[@type='application/ld+json']") %>% 
    xml_text() %>% 
    fromJSON(flatten = FALSE)
}

get_article_date <- function(article_metadata) {
  ymd_hms(article_metadata[["datePublished"]])
}

get_article_author <- function(article_metadata) {
  tibble(
    author_type = article_metadata[["author"]][["@type"]],
    author_name = article_metadata[["author"]][["name"]],
    author_url = article_metadata[["author"]][["url"]]
  )
}

get_article_publisher <- function(article_metadata) {
  tibble(
    publisher_type = article_metadata[["publisher"]][["@type"]],
    publisher_name = article_metadata[["publisher"]][["name"]],
    publisher_url = article_metadata[["publisher"]][["url"]]
  )
}

get_article_tags <- function(article_metadata) {
  tibble(keywords = article_metadata[["keywords"]]) %>% 
    separate(col = keywords, into = c("type", "keyword"), sep = ":")
}

get_article_title <- function(article_metadata) {
  article_metadata[["name"]]
}

article_core_table <- function(url, html) {
  article_html <- read_html(html)
  md <- get_article_metadata(article_html)
  authorship <- get_article_author(md)
  publisher <- get_article_publisher(md)
  one_row <- tibble(
    url = url,
    text = get_body_text(article_html),
    date_published = get_article_date(md),
    title = get_article_title(md),
  ) %>% 
    bind_cols(authorship, publisher)
}

article_tag_table <- function(url, html) {
  article_html <- read_html(html)
  md <- get_article_metadata(article_html)
  tags <- get_article_tags(md)
  tags %>% add_column(url = url, .before = TRUE)
}

article_links_table <- function(url, html) {
  article_html <- read_html(html)
  links <- get_body_links(article_html)
  tibble(url = url, link = links) %>% 
    mutate(external = str_detect(link, "^http"))
}
