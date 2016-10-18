## read from mySQL into R (notes skipped)

## read from hdf5 into R (notes skipped)

## regular expressions I II see week 4


## webscrapping
# getting data from the web using readLInes
myconnection = url("https://scholar.google.com/citations?user=Z1G9Lk4AAAAJ&hl=en")
myhtmlCode = readLines(myconnection)
close(myconnection)
myhtmlCode                                  # hard to read

# parsing with XML pkg
library(XML)
myurl <- "http://scholar.google.com/citations?user=Z1G9Lk4AAAAJ&hl=en"           # remove s from https !!
myhtml <- htmlTreeParse(myurl, useInternalNodes=T)

xpathSApply(myhtml, "//title", xmlValue)
xpathSApply(myhtml, "//div[@class='gs_gray']", xmlValue)                       # get authors and publishers
xpathSApply(myhtml, "//a[@class='gsc_a_ac']", xmlValue)                        # get number of citations (under tag a)

# GET from the httr package
library(httr) 
html2 = GET(myurl)
content2 = content(html2,as="text")
parsedHtml = htmlParse(content2,asText=TRUE)
xpathSApply(parsedHtml, "//title", xmlValue)

# access websites with passwords
pg2 = GET("http://httpbin.org/basic-auth/user/passwd",
          authenticate("user","passwd"))
pg2
names(pg2)

# use handles
google_site = handle("http://google.com")
pg1 = GET(handle=google_site,path="/")
pg2 = GET(handle=google_site,path="search")


### mytrial
# w: read source code of websites to figure out which content under which tags is useful 
myurl3 <- "http://www.thuisbezorgd.nl/en/order-takeaway-de-3063"           # remove s from https !!
html3 = GET(myurl3)
content3 = content(html3,as="text")
parsedHtml = htmlParse(content3,asText=TRUE)
xpathSApply(parsedHtml, "//title", xmlValue)

xpathSApply(parsedHtml, "//div[@class='details']", xmlValue)
xpathSApply(parsedHtml, "//h2[@class='restaurantname']", xmlValue)
xpathSApply(parsedHtml, "//a[@class='restaurantname']", xmlValue)

getNodeSet(parsedHtml, "//h2//a")
xpathSApply(parsedHtml, "//h2//a", xmlValue)
xpathSApply(parsedHtml, "//h2//a", xmlAttrs)                              # get attributes of tag a (e.g. href)

myobj <- xpathSApply(parsedHtml, "//h2//a", xmlAttrs)
class(myobj)                                                              # view what type of object myobj is
str(myobj)

# myobj turns out to be a matrix with char elements
link_extend <- myobj[2,]
link_for_scrap <- paste("http://www.thuisbezorgd.nl", link_extend, sep = "")

# 
web_scrapped <- vector("list", length = length(link_for_scrap))                   # create a list       check list [][]?
str(web_scrapped)
for (k in 1:length(link_for_scrap)) {
  food_url <- link_for_scrap[k]
  html_food = GET(food_url)
  food_content = content(html_food,as="text")
  web_scrapped[[k]] <- htmlParse(food_content,asText=TRUE)
}

## w: check how to write trycatch !!
# http://stackoverflow.com/questions/12193779/how-to-write-trycatch-in-r

xpathSApply(web_scrapped[[3]], "//label[@class='pulldown_transparent']", xmlValue)
xpathSApply(web_scrapped[[3]], "//div[@class='button_add']", xmlValue)

## check on getting reviews
# https://www.thuisbezorgd.nl/en/reviews-johnnys-burger-company-rotterdam/3

save(web_scrapped, file = "food_scrap.RData")
rm(list=setdiff(ls(), "web_scrapped"))



## Twitter API
library(httr)
myapp = oauth_app("twitter",
                  key="xn1xWsDrDPBDwTDckdTNMTHtt",secret="L5BMk4Agdoj86CJE29sq9mdnVaOlUpl0faamsqL2vdctc1veHC")
sig = sign_oauth1.0(myapp,
                    token = "2201560691-wqSaWaOXFPXgH20i5tZDBRelIOozBS3g2LJOmCP",
                    token_secret = "UD8dcl0re4ozofoV6DWtSAcjuwduOPu7lddVy47ImIuyI")
homeTL = GET("https://api.twitter.com/1.1/friends/list.json?count=120&screen_name=KimKardashian", sig)
# see https://dev.twitter.com/rest/reference/get/friends/list

json1 = content(homeTL)

library(jsonlite)
json2 = jsonlite::fromJSON(toJSON(json1))
json2[[1]][[3]]                                       # get a list of people KimKardashian is following

export_json <- jsonlite::toJSON(json2)
write(export_json, file="KimKardashian_twitter_friends.JSON")                     # export as json file

# see https://dev.twitter.com/rest/reference/get/search/tweets
# URL encode the query before making the request ! (eg. search query #haiku is encoded into %23haiku in URL)
# see https://dev.twitter.com/rest/public/search
homeTL = GET("https://api.twitter.com/1.1/search/tweets.json?q=%23BlackLivesMatter&count=50", sig)     # get 50 recent tweets with hashtag #BlackLivesMatter
json1 = content(homeTL)
json2 = jsonlite::fromJSON(toJSON(json1))
export_json <- jsonlite::toJSON(json2)
write(export_json, file="BLM_hashtag.JSON")                     # export as json file

names(json2)
names(json2$statuses)
json2$statuses$text

json2$statuses$id                                               # view ids of the tweets

# need to write loops with max_id or since_id to page through results?
# see https://dev.twitter.com/rest/public/timelines

