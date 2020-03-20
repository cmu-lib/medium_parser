# Write out articles and metadata to share on box

library(tidyverse)
library(glue)

drake::loadd(core_table)

pwalk(select(core_table, slug, text), function(slug, text) {
  write_lines(text, path = glue("scraping/medium_articles/{slug}.txt"))
})

core_table %>% 
  select(-text) %>% 
  write_tsv(path = "scraping/medium_metadata.tsv")

write_tsv(article_links, path = "article_links.tsv")
