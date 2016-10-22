n <- 10
fac <- 1
for (i in 1:n){
  fac <- fac*i
}
fac  
factorial(10)

#
data("cityweather")
??cityweather
cityweather.std <- cityweather
for (c in 1:ncol(cityweather)){
  cityweather.std[,c] <- (cityweather[,c]-min(cityweather[,c]))/(max(cityweather[,c])-min(cityweather[,c]))
}
exame.cw <- data.frame(mincol = rep(NA,5),maxcol = rep(NA,5))
for (k in 1:ncol(cityweather.std)){
  exame.cw[k, 1] <- min(cityweather.std[,k])                        # can use range instead
  exame.cw[k, 2] <- max(cityweather.std[,k])
}
exame.cw

# 
cityweather.med <- data.frame(median = rep(NA,ncol(cityweather)),mad = rep(NA,ncol(cityweather)))
for (c in 1:ncol(cityweather)){
  cityweather.med[c,1] <- median(cityweather.std[,c])
  cityweather.med[c,2] <- mad(cityweather.std[,c])
}


#
ptm <- proc.time()                                           # starts to count run time

cur.max <- -Inf
set.seed(1)
x <- rnorm(10000000)
for (i in 1:length(x)){
  if (x[i]>cur.max){cur.max = x[i]}
}
cur.max==max(x)

proc.time() - ptm                                            # ends run time

# can also use system.time() for timing single expression
cur.max <- -Inf
set.seed(1)
x <- rnorm(10000)
system.time({                                                # for multiple expressions use system.time({})
  for (i in 1:length(x)){s
    if (x[i]>cur.max){cur.max = x[i]}
  }
  cur.max==max(x)
})

# use cat to combine str and numerica values
# cat("This is one and two:", 1:2, "\n", "sth")

data("Affairs")
head(Affairs)
varnames <- colnames(Affairs)
for (k in 1:ncol(Affairs)){
  if (class(Affairs[,k])=="numeric" ||class(Affairs[,k])=="integer"){            # can use is.numeric or is.factor
    cat("The mean of variable", varnames[k], "is:",mean((Affairs[,k])),"\n")
  } else {
    if(is.factor(Affairs[,k])){
    cat("The levels of variable", varnames[k], "is:",levels((Affairs[,k])),"\n")
    }
  }
}  


