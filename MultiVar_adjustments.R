#3d simulation

p <- 1
n <- 100
x2 <- runif(n)
x1 <- p * runif(n) - (1 - p) * x2
beta0 <- 0; beta1 <- 1; 
tau <- 4 ; sigma <- .01

y <- beta0 + x1 * beta1 + tau * x2 + rnorm(n, sd = sigma)

plot(x1, y, type = "n", frame = FALSE)

abline(lm(y ~ x1), lwd = 2)
co.pal <- heat.colors(n)
points(x1, y, pch = 21, col = "black", 
       bg = co.pal[round((n - 1) * x2 + 1)], cex = 2)

library(rgl)
plot3d(x1, x2, y)


