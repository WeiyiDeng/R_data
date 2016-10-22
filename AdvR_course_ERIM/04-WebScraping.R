# --------------------------------------------
# Authors: Pieter Schoonees and Andreas Alfons
#          Erasmus Universiteit Rotterdam
# --------------------------------------------

# ------------------
# Web Scraping
# ------------------


## Required packages
library("RCurl")
library("XML")
library("jsonlite")

## Simple GET request
u <- "http://www.r-datacollection.com/materials/http/helloworld.html"
getURL(u)
getURL(u,verbose = TRUE)
htmlParse(getURL(u))

# -------------------
# Working with text
# -------------------
## Paste examples
name <- "Pieter"
paste("Hi", name)
start <- c(0, 10, 20)
paste("https://www.google.nl/#q=R&start", start, sep = "=")

keyword <- "R"
start <- c(0, 10, 20)
paste0("https://www.google.nl/#q=", keyword, "&start=", start)

## sprintf() example
url <- "https://www.google.nl/#q=%s&start=%d"
keyword <- "R"
start <- c(0, 10, 20)
sprintf(url, keyword, start)

## Parsing JSON
txt <- readLines("examples/indy.json")
fromJSON(txt)

## Parsing HTML
doc <- readLines("examples/fortunes.html")
doc <- htmlParse(doc)
doc

## XPath examples
getNodeSet(doc, "/html/body/div/p/i")

getNodeSet(doc, "//body//p/i")

getNodeSet(doc, "//p/i")

getNodeSet(doc, "/html/body/div/*/i")

getNodeSet(doc, "//p/..")

getNodeSet(doc, "//p/i | //head/title")

getNodeSet(doc, "//a/ancestor::div")

## XpathSApply examples
getNodeSet(doc, "//p//i")
xpathSApply(doc, "//p//i", xmlValue)

getNodeSet(doc, "//p//a")
xpathSApply(doc, "//p//a", xmlAttrs)

## Getting Feyenoord data
u <- "https://en.wikipedia.org/wiki/Feyenoord"
feyen <- getURL(u)
feyen_parse <- htmlParse(feyen)
tabs <- readHTMLTable(feyen_parse)
tabs[[13]]
tabs[[14]]
squad <- rbind(tabs[[13]], tabs[[14]])
View(squad)
