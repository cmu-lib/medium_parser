medium_parser
=============

Scripts to scrape a set of URLs from Medium and parse out html, body text, links, and metadata, as well as a [Shiny](https://shiny.rstudio.com/) app for exploring the results of the corpus.

## Setup

To run all of this code, you will need [R](https://cran.r-project.org/) >= 3.6.

Package dependencies are tracked using the [renv](https://rstudio.github.io/renv/articles/renv.html) package. When you first load this project, renv will be automatically installed. Next, run `renv::restore()` to read the required package list form `renv.lock`, which will install any required package versions.

## Scraping and pre-processing

`scraping/scrape_medium.R` reads the list of links from `scraping/links.txt` and dowloads the full HTML from each page, storing it in an sqlite3 database (which makes it easier to interrupt the script and restart it without losing progress). Once the scrape has been finished, this script outputs the full HTML data as `medium_html.rds`.

Once the raw HTML has been scraped, `scraping/preprocess_medium.R` contains an incremental data processing pipeline (based on the extremely useful [drake](https://github.com/ropensci/drake) package that consumes `medium_html.rds` and using helper methods from `parse_medium.R` to extract metadata, and then uses functions from quanteda to construct a full Corpus object with the Medium texts, the core_data metadata data frame, and two document-feature matrices - one stemmed and one unstemmed. These four objects are saved to `shiny/data.rda` to be referenced by the exploratory shiny app.

## Shiny app

The `shiny/` directory contains the Shiny app for creating an interactive web interface for data exploration and visualization. This is a conventional Shiny app, taking advantage of [reactivity](https://shiny.rstudio.com/tutorial/) to dynamically update menus, tables, and graph outputs based on input selections across the site.

The UI uses [ShinyDashboard](https://rstudio.github.io/shinydashboard/) elements.


## Corpus definition

Searches on Medium used to assemble the list of links in `scraping/links.txt`:

- "artificial intelligence"
- "ethics"
- "ethical"
- "big data"
- "machine learning"
- "algorithm"
- "regulation"
- "governance"
- "robotics"

These were scraped using the Firefox plugin [webscraper.io](https://www.webscraper.io/), under the following configuration:

```json
{
  "_id": "mediumlinks",
  "startUrl": [
    "https://medium.com/search?q=ethics"
  ],
  "selectors": [
    {
      "id": "postlinks",
      "type": "SelectorElementScroll",
      "parentSelectors": [
        "_root"
      ],
      "selector": "div.postArticle",
      "multiple": true,
      "delay": "3500"
    },
    {
      "id": "link",
      "type": "SelectorElementAttribute",
      "parentSelectors": [
        "postlinks"
      ],
      "selector": "a[data-action=open-post]",
      "multiple": true,
      "extractAttribute": "href",
      "delay": 0
    }
  ]
}
```

---
[Matthew Lincoln](https://matthewlincoln.net)
