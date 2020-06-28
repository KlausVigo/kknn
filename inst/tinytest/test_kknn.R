x <- data.frame(a = rnorm(5), b = rnorm(5), 
                c = factor(c("X0", "X0", "X0", "X1", "X1")))
y <- as.matrix(x[, 1:2])

knn <- kknn(c~., train = x, test = x, k = 5, kernel = "rectangular", 
            scale=FALSE)
knn_dist <- kknn.dist(y, y, k=5)

D <- as.matrix(dist(y)) 
D <- t(apply(D, 1, sort))

D1 <- as.matrix(dist(y, "minkowski", p=1))
D1 <- t(apply(D1, 1, sort))

knn_dist1 <- kknn.dist(y, y, k=5, distance=1)

## check Euclidean distances
## check distances are equvalent to dist
expect_equivalent(knn$D,  D)
expect_equivalent(knn_dist[[2]],  D)


## check Minkowski distances
## check distances are equvalent to dist
expect_equivalent(knn_dist1[[2]],  D1)


