getwd()
install.packages("kohonen")
library(kohonen)

# Data
# file <- read.csv(file.choose(), header = T)
file <- read.csv("us-states.csv")
data <- subset(file, select = -State )
str(data)
X <- scale(data)
# X <- as.numeric(X)
summary(X)

# SOM
set.seed(222)
g <- somgrid(xdim = 5, ydim = 6, topo = "hexagon" )
map <- som(X,
           grid = g,
           alpha = c(0.1, 0.01),
           radius = 1,
           keep.data = TRUE)

som <- som(X, grid = somgrid(5, 5, "hexagonal"), alpha = c(0.05, 0.01),
           radius = 1,
           keep.data = TRUE)
summary(som)
## xyf
#xyf <- xyf(X,file$State, grid = somgrid(5, 5, "hexagonal"))
#summary(xyf)

supersom<-supersom(X, grid=g,alpha = c(0.01, 0.05),
                   radius = 1,whatmap = NULL, maxNA.fraction = 0.50,
                   keep.data = TRUE, dist.fcts = NULL,
                   mode = c("online", "batch", "pbatch"), cores = -1,
                   normalizeDataLayers = TRUE)

plot(supersom, type = "changes")

#obj.classes <- as.integer(yeast$class)
#colors <- c("yellow", "green", "blue", "red", "orange")
#plot(yeast.supersom, type = "mapping", col = colors[obj.classes],
#     pch = obj.classes, main = "yeast da


plot(supersom,
     type = 'mapping',
     bgcol = c("blue","red","green"),
     keepMargins = F,
     col = NA,
     main = '')

plot(map, type = "changes")
plot(map)
plot(map, type="count", main="Node Counts")
plot(map, type="dist.neighbours", main = "SOM neighbour distances")


m = map$codes
m =dist(as.numeric(unlist(map$codes)))
som.events <- map$codes[[1]]
som.dist <- as.matrix(dist(som.events))
library(readr)
set.seed(222)
# Compute and plot wss for k = 2 to k = 15.
k.max <- 15
d<-as.matrix(dist(som.events))

wss <- sapply(1:k.max, 
              function(k){kmeans(d, k, nstart=50,iter.max =70 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
# Visualising cluster results
## use hierarchical clustering to cluster the codebook vectors
som_cluster
som_cluster <- cutree(hclust(m), 3)
# plot these results:
plot(map, type="mapping",  main = "Clusters") 
add.cluster.boundaries(map, som_cluster)
# get vector with cluster value for each original data sample
cluster_assignment <- som_cluster[map$unit.classif]
# for each of analysis, add the assignment as a column in the original data:
file$cluster <- cluster_assignment
write.table(file, "us-states-clusters.csv", row.names=FALSE, sep=",")

clus <- read.csv("us-states-clusters.csv")
clus

table(file$cluster)
