set.seed(2)
x <- matrix(rnorm(50 * 2), ncol = 2)
x[1:25, 1] <- x[1:25, 1] + 3
x[1:25, 2] <- x[1:25, 2] - 4
km.out <- kmeans(x, 2, nstart = 20)
km.out$cluster
plot(x, col = (km.out$cluster + 1), main = "K-Means Clustering Results with K=2", 
     xlab = "", ylab = "", pch = 20, cex = 2)
km.out$centers
points(km.out$centers[1, 1], km.out$centers[1, 2], pch = 10, col = "red", cex = 2)
points(km.out$centers[2, 1], km.out$centers[2, 2], pch = 10, col = "blue", cex = 2)
