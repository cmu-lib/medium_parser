medium_parser
=============

Scripts to scrape a set of URLs from Medium and parse out html, body text, links, and metadata.

## Corpus definition

Searches on Medium used to assemble the list of links in `links.txt`:

- "artificial intelligence"
- "ethics"
- "ethical"
- "big data"
- "machine learning"
- "algorithm"
- "regulation"
- "governance"

These were scraped using the Firefox plugin webscraper.io, under the following configuration:

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
