library(UsingR)
data("father.son")
x <- father.son$sheight
n <- length(x)
B <- 10000
resamples <- matrix(sample(x, n * B, replace = TRUE), B, n)
resampledMedians <- apply(resamples, 1, median)
hist(resampledMedians)
median(resampledMedians)

x1 <- sample(1:6, 1000, replace = TRUE, prob = c(1/8, 1/8, 1/4, 1/4, 1/8, 1/8))
n1 <- length(x1)
B1 <- 10000
resamples1 <- matrix(sample(x1, n1 * B1, replace = TRUE), B1, n1)
resampledMeans1 <- apply(resamples1, 1, mean)
hist(resampledMeans1)
median(resampledMedians1)