options(scipen=1)
nosim <- 10000
n <- 100

sd(apply(matrix(rnorm(nosim * n), nosim), 1, mean))
1/sqrt(n)

sd(apply(matrix(runif(nosim * n), nosim), 1, mean))
1/sqrt(12*n)

sd(apply(matrix(rpois(nosim * n, 4), nosim), 1, mean))
2/sqrt(n)

sd(apply(matrix(sample(0:1, nosim * n, replace = TRUE), nosim), 1, mean))
1/(2*sqrt(n))


hist(runif(10000))
hist(rnorm(10000))
hist(rpois(10000,10))
hist(sample(0:1, 10000, replace = TRUE))


plot(runif(10000))
plot(rnorm(10000))
plot(rpois(10000,10))
plot(sample(0:1, 10000, replace = TRUE))
