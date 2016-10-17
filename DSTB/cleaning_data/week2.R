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
link_for_scrap <- paste("https://www.thuisbezorgd.nl", link_extend, sep = "")



