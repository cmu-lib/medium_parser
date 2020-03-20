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
    tokens_compound(pattern = phrase(c("artificial intelligence", "big data", "machine learning"))),
  medium_dfm = dfm(medium_tokens),
  regulation_docs = rownames(medium_dfm)[as.logical(medium_dfm[,"regulation"])],
  big_data_docs = rownames(medium_dfm)[as.logical(medium_dfm[,"big_data"])],
  governance_docs = rownames(medium_dfm)[as.logical(medium_dfm[,"governance"])],
  ai_docs = rownames(medium_dfm)[(as.logical(medium_dfm[,"artificial_intelligence"]) | as.logical(medium_dfm[,"a.i"]) | as.logical(medium_dfm[,"ai"]))],
  ethics_docs = rownames(medium_dfm)[(as.logical(medium_dfm[,"ethics"]) | as.logical(medium_dfm[,"ethical"]))],
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
    dfm_trim(min_docfreq = 0.01, max_docfreq = 0.9, docfreq_type = "prop"),
  trimmed_dfm = stopped_dfm %>% 
    dfm_remove(c("ethics", "ethical", "artificial_intelligence", "medium.com", "machine_learning", "big_data", "tags", "ai", "a.i", "ai's")),
  stemmed_dfm = trimmed_dfm %>% 
    dfm_wordstem(language = "en") %>% 
    dfm_wordstem(language = "es"),
  keyness = target(
    textstat_keyness(x, target = fac, measure = "lr") %>%
      mutate(effect_size = effect_size(n_target, n_reference)) %>%
      filter(p < 0.05) %>%
      arrange(desc(effect_size)),
    transform = cross(
      x = list("unstemmed" = trimmed_dfm, "stemmed" = stemmed_dfm),
      fac = list(ai_ethics = ai_ethics_docs, 
                 ai_no_ethics = ai_no_ethics_docs, 
                 ai_governance = ai_governance_docs,
                 ai_no_governance = ai_no_governance_docs,
                 ml_ethics = ml_ethics_docs, 
                 ml_no_ethics = ml_no_ethics_docs, 
                 ethics_only = ethics_only_docs,
                 governance = governance_docs))
  ),
  keyness_df = bind_rows(
    "trimmed_dfm-ml_no_ethics_docs" = keyness_trimmed_dfm_ml_no_ethics_docs,
    "trimmed_dfm-ethics_only_docs" = keyness_trimmed_dfm_ethics_only_docs,
    "trimmed_dfm-ai_ethics_docs" = keyness_trimmed_dfm_ai_ethics_docs,
    "trimmed_dfm-ml_ethics_docs" = keyness_trimmed_dfm_ml_ethics_docs,
    "trimmed_dfm-ai_no_ethics_docs" = keyness_trimmed_dfm_ai_no_ethics_docs,
    "trimmed_dfm-ai_governance_docs" = keyness_trimmed_dfm_ai_governance_docs,
    "trimmed_dfm-ai_no_governance_docs" = keyness_trimmed_dfm_ai_no_governance_docs,
    "stemmed_dfm-ethics_only_docs" = keyness_stemmed_dfm_ethics_only_docs,
    "stemmed_dfm-ai_ethics_docs" = keyness_stemmed_dfm_ai_ethics_docs,
    "stemmed_dfm-ml_no_ethics_docs" = keyness_stemmed_dfm_ml_no_ethics_docs,
    "stemmed_dfm-ml_ethics_docs" = keyness_stemmed_dfm_ml_ethics_docs,
    "stemmed_dfm-ai_no_ethics_docs" = keyness_stemmed_dfm_ai_no_ethics_docs,
    "stemmed_dfm-ai_governance_docs" = keyness_stemmed_dfm_ai_governance_docs,
    "stemmed_dfm-ai_no_governance_docs" = keyness_stemmed_dfm_ai_no_governance_docs,
    .id = "type") %>% 
    separate(col = type, into = c("stemming", "target_corpus"), sep = "-") %>% 
    mutate(stemming = stemming == "stemmed_dfm"),
  shiny_data = save(keyness_df, stopped_dfm, trimmed_dfm, stemmed_dfm, medium_corpus, medium_dfm, core_table, file = file_out("shiny/data.rda"))
)

make(medium_plan)
