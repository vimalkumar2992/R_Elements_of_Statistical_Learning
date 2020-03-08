###############################################
##
## Rachael Blair
## Created: 3/7/19
## Editer
##
###############################################
source("http://bioconductor.org/biocLite.R")
# biocLite()
# biocLite("multtest")
# biocLite("cluster")
# install.packages("fpc")
# install.packages("bootcluster")
BiocManager::install(version = "3.9")
BiocManager::install("multtest")

install.packages("multtest")
library("multtest")
library("fpc")
library("cluster")
library("bootcluster")
library("fossil")

# Look at the iris
?iris
head(iris)
dats <- iris[ ,1:4]

# apply k-means
km <- kmeans(dats, centers = 3, nstart = 10)

# how well does the clustering match the labels
table(km$cluster, iris$Species)

# visualize the groups
quartz()
x11()
plot(dats[ ,c("Sepal.Length", "Sepal.Width")], col = km$cluster, main = "Example: k-means")
points(km$centers[, c("Sepal.Length", "Sepal.Width")], col = 1:3, pch = 8, cex = 2)

# calculate rand index and adjusted rand index
rand.index(km$cluster, as.numeric(iris$Species))
adj.rand.index(km$cluster, as.numeric(iris$Species))

#########################################
## apply k-means to PC1 and PC2
#########################################
fit <- prcomp(dats, center=TRUE, scale=TRUE)

quartz()
x11()
plot(fit)

quartz()
x11()
biplot(fit)

PC1 <- fit$x[,1]
PC2 <- fit$x[,2] 
PC_dats <- cbind(PC1, PC2)

# run k-means on the PC values
km2 <- kmeans(PC_dats, centers = 3, nstart = 10)

rand.index(km2$cluster, as.numeric(iris$Species))
adj.rand.index(km2$cluster, as.numeric(iris$Species))

# plot the groups
quartz()
plot(PC_dats, col = km2$cluster, main = "Example k-means w/PC")
points(km2$centers, col = 1:3, pch = 8, cex= 2)

##############################
## k-mediods
##############################
kmed <- pamk(dats)

# let the program decide optimal k
kmed$nc

# tabulate the results as before
table(kmed$pamobject$clustering, iris$Species)

# plot the results for k= 2
layout(matrix(c(1,2), 1, 2))
plot(kmed$pamobject)

# lets try k= 3
kmed3 <- pamk(dats, 3)
table(kmed3$pamobject$clustering, iris$Species)

# plot the results for k= 2
layout(matrix(c(1,2), 1, 2))
plot(kmed3$pamobject)

###################################
## Some other cluster selection

# bootstrapping cluster stability
k.select(dats, range = 2:6, B=50, r= 5, scheme_2=TRUE)

k.select(dats, range = 2:6, B=50, r= 5, scheme_2=FALSE)

# gap statistics - kmeans
gap_kmeans <- clusGap(dats, kmeans, nstart = 20, K.max = 10, B = 100)

quartz()
plot(gap_kmeans, main = "Gap Statistic: kmeans")

# gap statistics - kmedoid
gap_kmed <- clusGap(dats, pam, K.max = 10, B = 100)

quartz()
plot(gap_kmed, main = "Gap Statistic: kmedoids")

####################################
##
## Hierchical clustering
######################################
idx <- sample(c(1:length(dats[,1])), 60)
dat_red <- dats[idx, ]

# calculate the distance for HC
d <- dist(dat_red)
dim(as.matrix(d))
hc <- hclust(d, method = "ave")

quartz()
plot(hc, hang = -1, labels = iris$Species[idx])

?cutree
?silhouette

ct <- cutree(hc, k = 5)
si <- silhouette(ct, dist = d)

quartz()
plot(si)

##
ct2 <- cutree(hc, h = 1.95)
si2 <- silhouette(ct2, dist = d)

quartz()
plot(si2)

##
ct3 <- cutree(hc, h = 3)
si3 <- silhouette(ct3, dist = d)

quartz()
plot(si3)

# cut the tree over a series of values k = 2,3,4,5,6
store <- c()
for (i in 2:6){
	ct <- cutree(hc, k=i)
	si <- silhouette(ct, dist = d)
	avg_width <- summary(si)$avg.width
	store <- c(store, avg_width)
}

############################################
###### Cluster high dimensional data
############################################

data(golub)
rownames(golub) <- golub.gnames[,3]
colnames(golub) <- c(paste("ALL", 1:27, sep = ":"), paste("AML", 28:38, sep = ":"))


myttest <- function(x) t.test(x[1:27], x[28:38])$p.value
p.val <- apply(golub, 1, myttest)
sorted_p <- order(p.val)

golub.order <- golub[sorted_p, ]
DEgenes <- golub.order[1:250, ]

write.table(DEgenes, file = "DEgenes.txt", sep = "\t")

DEgenes[1:5,1:5]

?heatmap

quartz()
heatmap(DEgenes, col = cm.colors(20), xlab = "conditions", ylab = "genes")


















































