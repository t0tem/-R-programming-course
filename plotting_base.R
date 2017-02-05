## 1

set.seed(45678)
p1 <- hist(rnorm(150, mean = 1), plot=FALSE)
p2 <- hist(rnorm(150, mean = 8), plot=FALSE)
yrange <- c(0, max(p1$counts, p2$counts))
plot(p1, col=rgb(0,0,1,1/4), xlim=range(-2,12), ylim=yrange, xlab="Estimated coefficient of x1",
     main="Bias Effect of Omitted Regressor")
plot(p2, col=rgb(1,0,0,1/4), xlim=range(-2,12), ylim=yrange, add=TRUE)
legend(1.1, 40, c("Uncorrelated regressor, x3, omitted", "Correlated regressor, x2, omitted"),
       fill=c(rgb(0,0,1,1/4), rgb(1,0,0,1/4)))
