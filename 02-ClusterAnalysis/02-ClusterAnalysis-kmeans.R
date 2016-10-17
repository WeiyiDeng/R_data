# --------------------------------------------
# Authors: Pieter Schoonees and Andreas Alfons
#          Erasmus Universiteit Rotterdam
# --------------------------------------------

## Install packages if not done yet
install.packages("NbClust")
install.packages(c("ggplot2", "colorspace", "GGally"))

## Load all required packages
library("euR")
library("NbClust")
library("ggplot2")
library("colorspace")
library("GGally")
library("cluster")

## Load the crime2008 data
library("euR")
data("crime2008")
dim(crime2008)
summary(crime2008)
head(crime2008)

## Divide all rows by the Population
crime.std <- sweep(crime2008, MARGIN = 1, STATS =                       # sweep for each row/col
                     crime2008$Population, FUN = "/")
crime.std <- 1000 * crime.std
head(crime.std, 4)
crime.std <- crime.std[, -9]                                            # delete col 9
head(crime.std) 

## Scatterplot matrix
library("GGally")
ggpairs(crime.std)

## Parallel coordinates plot
ggparcoord(crime.std, scale = "uniminmax")

## Check number of observations with missing valeus
sum(!complete.cases(crime.std))  

## Remove incomplete observations
crime.std.c <- na.omit(crime.std) 
summary(crime.std.c)
any(is.na(crime.std.c))

## Run kmeans algorithm 5000 times
set.seed(4131)
km <- kmeans(crime.std.c, centers = 5, nstart = 5000)

## kmeans output
km$centers
km$size
km$cluster    # or fitted(km, "classes")

## Inspect first two clusters
crime.std.c[km$cluster == 1, ]
crime.std.c[km$cluster == 2, ]

## Parallel coordinates plot of the means
df5 <- as.data.frame(km$centers)
df5$cluster <- factor(1:5)
ggparcoord(df5, groupColumn = "cluster", columns = 1:8, scale = "uniminmax")

## SAme for all the data
df5 <- crime.std.c
df5$cluster <- factor(km$cluster)
ggparcoord(df5, groupColumn = "cluster", columns = 1:8, scale = "uniminmax")

## Scree plot
library("NbClust")
?NbClust
set.seed(1711)
nc <- NbClust(crime.std.c, min.nc = 2, max.nc = 15, 
              method = "kmeans", index = "tracew")
nc
df_scree <- data.frame(TraceW = nc$All.index, Cluster = 2:15)
ggplot(df_scree, aes(x = Cluster, y = TraceW)) + geom_line() + geom_point()            # aes denote axis

## Using multiple clustering criteria
set.seed(1966)
nc <- NbClust(crime.std.c, min.nc = 2, max.nc = 15, 
              method = "kmeans", index = "all")
df_crit <- data.frame(Clusters = nc$Best.nc[1, ])
ggplot(df_crit, aes(x = Clusters)) + geom_bar()

## Using pam() instead of kmeans()
library("cluster")
pm <- pam(crime.std.c[, -6], k = 5)
pm$medoids
table(pm$cluster)
df5 <- as.data.frame(pm$medoids)
df5$cluster <- factor(1:5)
ggparcoord(df5, groupColumn = "cluster", columns = 1:7, scale = "uniminmax")

## Comparing pam and kmeans
pm$clustering
table(pam = pm$clustering, kmeans = km$cluster)                                       # cluster names/labels do not matter

## Parallel coordinates plot for pam result
df5 <- crime.std.c[, -6]
df5$cluster <- factor(pm$clustering)
ggparcoord(df5, groupColumn = "cluster", columns = 1:7, scale = "uniminmax")

