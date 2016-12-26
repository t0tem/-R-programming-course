options(digits = 4, scipen = 1)
set.seed(1)

n <- 100000
means <- cumsum(rnorm(n))/(1:n)
 # head(means)
plot(1 : n, means, type = "l", lwd = 1, 
     frame = FALSE, ylab = "cumulative means", xlab = "sample size")
abline(h = 0)

n <- 500
means <- cumsum(sample(0:1, n, replace = TRUE))/(1:n)
plot(1 : n, means, type = "l", lwd = 1, 
     frame = FALSE, ylab = "cumulative means", xlab = "sample size")
abline(h = 0.5)


library(UsingR)
data("father.son")
x <- father.son$sheight
