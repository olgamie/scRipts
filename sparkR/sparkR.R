library(SparkR)
#library(geosphere) 

sc <- sparkR.init(master="local")

data <- textFile(sc, "Desktop/R/logs.txt")

# pre-processing of text file, split by \t, return list of columns
logs <- flatMap(data,
                 function(line) {
                   list(strsplit(line, split = "\t")[[1]])
                 })

path <- lapply(logs, function(log) { list( log[1], c(log[4], log[3]) ) })

# group by key
pathGrouped <- groupByKey(path, 1L)

# map over paths to calculate minimum distance
mapRes <- map(pathGrouped, function(x) {
  lonlat <- do.call(rbind, x[[2]])
  min(dist(apply(lonlat, 2, as.numeric)))
})

# collect results, download them to R
collection <- collect(mapRes)

# stop sparkR
sparkR.stop()
