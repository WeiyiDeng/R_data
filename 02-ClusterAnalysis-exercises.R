# --------------------------------------------
# Authors: Pieter Schoonees and Andreas Alfons
#          Erasmus Universiteit Rotterdam
# --------------------------------------------
library("euR")

#---------------------
# cityweather data
#---------------------

data("cityweather")
?cityweather
summary(cityweather)
dim(cityweather)

#---------------------
# Exercise 1.1
#---------------------
## a
library("GGally")
ggpairs(cityweather)

## b
nrow(cityweather) == nrow(na.omit(cityweather))
any(is.na(cityweather))

## c
cityweather[which.max(cityweather$SunHours), ]

## d
cityweather[which.min(cityweather$Rainfall), ]

#---------------------
# Exercise 1.2
#---------------------

## a
colmins <- apply(cityweather, 2, min)
colmins

## b 
cityweather.std <- sweep(cityweather, 2, STATS = colmins, FUN = "-")

## c
apply(cityweather.std, 2, min)

## d
colmaxs <- apply(cityweather.std, 2, max)
colmaxs
cityweather.std <- sweep(cityweather.std, 2, STATS = colmaxs, 
                         FUN = "/")
apply(cityweather.std, 2, max)
apply(cityweather.std, 2, min)

scale(cityweather)
#---------------------
# Exercise 1.3
#---------------------

## a
load("cityweather.std.RData")
set.seed(1)
km <- kmeans(cityweather.std, centers = 6, nstart = 1000)

## b
km$centers
km$cluster
km$size

## c
df <- as.data.frame(km$centers)
df$cluster <- factor(1:6)
ggparcoord(df, columns = 1:5, groupColumn = 6, scale = "uniminmax")
cityweather.std[km$cluster == 1, ]
cityweather.std[km$cluster == 4, ]

## d
df <- cityweather.std
df$cluster <- factor(km$cluster)
ggparcoord(df, columns = 1:5, groupColumn = 6, scale = "uniminmax")

#---------------------
# Exercise 1.4
#---------------------

## a
library("NbClust")
set.seed(1)
nc <- NbClust(cityweather.std, min.nc = 2, max.nc = 12, 
              method = "kmeans", index = "all")

## b
df_crit <- data.frame(Clusters = nc$Best.nc[1, ])
ggplot(df_crit, aes(x = Clusters)) + geom_bar()

## c
set.seed(1)
km <- kmeans(cityweather.std, centers = 3, nstart = 1000)
df <- as.data.frame(km$centers)
df$cluster <- factor(1:3)
ggparcoord(df, columns = 1:5, groupColumn = 6, 
           scale = "uniminmax")
km$size
df <- cityweather.std
df$cluster <- factor(km$cluster)
ggparcoord(df, columns = 1:5, groupColumn = 6, 
           scale = "uniminmax")
km$size
km$centers

## d
pm <- pam(cityweather.std, k = 3)
pm$medoids
table(pm$cluster)
df <- as.data.frame(pm$medoids)
df$cluster <- factor(1:3)
ggparcoord(df, groupColumn = "cluster", columns = 1:5, 
           scale = "uniminmax")
df <- cityweather
df$cluster <- factor(pm$clustering)
ggpairs(df, columns= 1:5, mapping = aes(colour = cluster))
table(kmeans = km$cluster, pam = pm$clustering)


#---------------------
# Exercise 1.5
#---------------------

## a
load("cityweather.std.RData")
hc.std <- hclust(dist(cityweather.std), method = "ward.D2")
plot(hc.std)
par(cex = 0.6)
plot(hc.std)
par(cex = 1)
abline(h = 2.5, col = "red")

## b
grps.hc.std <- cutree(hc.std, h = 2.5)
unique(grps.hc.std)
library("GGally")
df <- cityweather.std
df$cluster <- factor(grps.hc.std)
ggpairs(df, columns = 1:5, mapping = aes(colour = cluster))
df_orig <- cityweather
df_orig$cluster <- factor(grps.hc.std)
ggpairs(df_orig, columns = 1:5, mapping = aes(colour = cluster))
ggparcoord(df, columns = 1:5, scale = "uniminmax", groupColumn = 6)
cityweather[grps.hc.std == 1, ]
cityweather[grps.hc.std == 2, ] # etc
cityweather[grps.hc.std == 3, ]

#---------------------
# Exercise 2
#---------------------
hc.paint <- hclust(dist(painters[,-5], method = "manhattan"))
plot(hc.paint)
grps.hc.paint <- cutree(hc.paint, k = 8)
table(grps.hc.paint, painters$School)
