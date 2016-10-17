## download data from URL

getwd()

setwd("D:/Users/R_data/DSTB/cleaning_data")

if (!file.exists("data")){
  dir.create("data")
}

# url from webpage: https://data.baltimorecity.gov/Transportation/Baltimore-Fixed-Speed-Cameras/dz54-2aru
# click on "export" and choose csv; right click and choose "copy link address"
fileURL <- "https://data.baltimorecity.gov/api/views/dz54-2aru/rows.csv?accessType=DOWNLOAD"

download.file(fileURL, destfile = "./data/camera_data.csv", method = "curl")

list.files("./data")

data_downloads_time <- date()
data_downloads_time


## read local csv file
camera_dt <- read.table("./data/camera_data.csv", sep = ",", header = TRUE)
head(camera_dt)

# or
camera_dt <- read.csv("./data/camera_data.csv")                
# w: use quote = "" to avoid " or ' s in data? Gives error here, why ?


## download and read excel files
if (!file.exists("data")){dir.create("data")}                 # check if dir exists
fileURL2 <- "https://data.baltimorecity.gov/api/views/dz54-2aru/rows.csv?accessType=DOWNLOAD&bom=true"
download.file(fileURL2, destfile = "./data/camera_data.xlsx", method = "curl")
dateDownloaded <- date()

# w: cannot use xlsx pkg; check error ?
# error: unable to load shared object 'C:/Users/Weiyi Deng/Documents/R/win-library/3.3/rJava/libs/x64/rJava.dll':
# LoadLibrary failure:  The specified module could not be found.
# Error: package or namespace load failed for 'rJava'
install.packages("xlsx")
library(xlsx)

camera_dt <- read.xlsx("./data/camera_data.xlsx", sheetIndex = 1, header = TRUE)
head(camera_dt)


## read XML files into R
library(XML)
?? XML
fileUrl <- "http://www.w3schools.com/xml/simple.xml"
doc <- xmlTreeParse(fileUrl,useInternal=TRUE)                       # parse the xml file
rootNode <- xmlRoot(doc)                                            # xmlRoot to create a wrapper for the xml file
xmlName(rootNode)                                                   # get name of the top node
names(rootNode)                                                     # get names for each subnodes

rootNode[[1]]                                                       # access elements of the xml file like lists
rootNode[[1]][[2]]

# extract all contents of the xml file (get rid of tags)
xmlSApply(rootNode,xmlValue)

# extract all contents under the tags of name and price
xpathSApply(rootNode,"//name",xmlValue)
xpathSApply(rootNode,"//price",xmlValue)

# another example
fileUrl <- "http://espn.go.com/nfl/team/_/name/bal/baltimore-ravens"
doc <- htmlTreeParse(fileUrl,useInternal=TRUE)
scores <- xpathSApply(doc,"//li[@class='score']",xmlValue)          # select all content under tag list with class="score"
teams <- xpathSApply(doc,"//li[@class='team-name']",xmlValue)
ranks <- xpathSApply(doc,"//li[@class='ranking']",xmlValue)
scores
teams
ranks
game-info <- xpathSApply(doc,"//div[@class='game-info']",xmlValue)
game-info


## read Json file into R
library(jsonlite)
jsonData <- fromJSON("https://api.github.com/users/weiyideng/repos")
names(jsonData)                                                     # get top level variable names in Json data 
jsonData$name                                                       # access content of the "name" variable

# variable "owner" is an array containing multiple variables
names(jsonData$owner)                                            # get sub-level variable names from the variable "owner"
jsonData$owner$login                                             # access content of the "login" variable under the array of the variable "name"

jsonData$clone_url                                                  # get urls of all my repos

# write data frame to JSON
myjson <- toJSON(iris, pretty=TRUE)
cat(myjson)                           # similar as print

# convert JSON back to data frame 
iris2 <- fromJSON(myjson)
head(iris2)



### data table            (w: incomplete notes)
install.packages("data.table")
library(data.table)

DF = data.frame(x=rnorm(9),y=rep(c("a","b","c"),each=3),z=rnorm(9))
head(DF,3)

DT = data.table(x=rnorm(9),y=rep(c("a","b","c"),each=3),z=rnorm(9))
head(DT,3)



### exercise
rm(list=ls())                                           # clear workspace

install.packages("swirl")
packageVersion("swirl")
library(swirl)
install_from_swirl("Getting and Cleaning Data")
swirl()

