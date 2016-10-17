# --------------------------------------------
# Authors: Andreas Alfons and Pieter Schoonees
#          Erasmus University Rotterdam
# --------------------------------------------

## Load standardized data
load("crime.std.RData")
dist(crime.std[1:5, ])

## Euclidean distqnce with NAs
crime.std[1:2, ]
dist(crime.std[1:2, ])
sqrt(sum((crime.std[1, ] - crime.std[2, ])^2, na.rm = TRUE)*8/2)

## Canberra distance
dist(crime.std[1:5, ], method = "canberra")

## Manhattan distance
dist(crime.std[1:5, ], method = "manhattan")

## Effect of scale on distance
dist(crime.std[1:3, ])         # all variables
dist(crime.std[1:3, -5])       # without Drug
dist(crime.std[1:3, -6])       # without Total

## Plot actual clusters (this used base graphics)
library("colorspace")
data("testdata")
plot(testdata[,-3], pch = 21, asp = 1, main = "Actual Clusters",
          bg = rainbow_hcl(3)[testdata[,3]])

## Euclidean hclust with dendrogram
hc <- hclust(dist(crime.std))
plot(hc)

## Cut dendrogram into 5 groups
grps <- cutree(hc, k = 5)
table(grps)     # calculate cluster sizes
grps

## Cut dendrogram at height 40
grps <- cutree(hc, h = 40)
table(grps)     # calculate cluster sizes
grps

## Partial scatterplot matrix
library("GGally")
df <- crime.std
df$cluster <- factor(grps)
ggpairs(df, columns = 3:6, mapping = aes(colour = cluster))

## Parcoord plot
df <- crime.std
df$cluster <- factor(grps)
ggparcoord(df, columns = 1:8, groupColumn = "cluster", scale = "uniminmax")

## Euclidean distances without Total
hc.nototal <- hclust(dist(crime.std[, -6]))
plot(hc.nototal)

## Cut dendrogram into 5 groups
grps <- cutree(hc.nototal, k = 5)
parcoord(crime.std, col = rainbow_hcl(5)[grps], lwd = 1.5)

## Parcoord plot
grps <- cutree(hc.nototal, k = 5)
df$cluster <- factor(grps)
ggparcoord(df, columns = 1:8, groupColumn = "cluster", scale = "uniminmax")

## Partial scatterplot matrix
ggpairs(df, columns = 3:6, mapping = aes(colour = cluster))
