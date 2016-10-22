data("cityweather")
?cityweather
summary(cityweather)

ggpairs(cityweather)

which.max(cityweather$SunHours)
cityweather[which.max(cityweather$SunHours),]
cityweather[which.min(cityweather$RainDays),]

min_col <- apply(cityweather,2,min)

cityweather.std <- sweep(cityweather, MARGIN = 2, STATS =                       # sweep for each row/col
                     min_col, FUN = "-")

apply(cityweather.std,2,min)

colmax <- apply(cityweather,2,max)
cityweather.std <- sweep(cityweather, MARGIN = 2, STATS =                       # sweep for each row/col
                           colmax, FUN = "/")

apply(cityweather.std,2,max)

# or use variance standardization
scale(cityweather)

sum(!complete.cases(cityweather.std))
set.seed(10086)
km <- kmeans(cityweather.std, centers = 6, nstart = 1000)

km$centers
km$size
km$cluster    # or fitted(km, "classes")

df <- as.data.frame(km$centers)
df$cluster <- factor(1:6)
ggparcoord(df, groupColumn = "cluster", columns = 1:5, scale = "uniminmax")
df

df6 <- cityweather.std
df6$cluster <- factor(km$cluster)
ggparcoord(df6, groupColumn = "cluster", columns = 1:5, scale = "uniminmax")

library("NbClust")
?NbClust
set.seed(10086)
nc <- NbClust(cityweather.std, min.nc = 2, max.nc = 12, 
              method = "kmeans", index = "tracew")
nc
df_scree <- data.frame(TraceW = nc$All.index, Cluster = 2:12)
ggplot(df_scree, aes(x = Cluster, y = TraceW)) + geom_line() + geom_point()            # aes denote axis

set.seed(10086)
nc <- NbClust(cityweather.std, min.nc = 2, max.nc = 12, 
              method = "kmeans", index = "all")
df_crit <- data.frame(Clusters = nc$Best.nc[1, ])
ggplot(df_crit, aes(x = Clusters)) + geom_bar()

km <- kmeans(cityweather.std, centers = 3, nstart = 1000)
km$centers
km$size
km$cluster    # or fitted(km, "classes")

library("cluster")
pm <- pam(cityweather.std, k = 3)
pm$medoids
table(pm$cluster)
pm$clustering
table(pam = pm$clustering, kmeans = km$cluster)

# use ggpairs mapping = aes(colour = cluster) ?
