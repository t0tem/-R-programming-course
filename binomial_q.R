library(MASS)
data(shuttle)

######################################################################
## Make our own variables just for illustration
shuttle$auto <- 1 * (shuttle$use == "auto")
shuttle$headwind <- 1 * (shuttle$wind == "head")
fit <- glm(auto ~ headwind, data = shuttle, family = binomial)
exp(coef(fit))
#####################################################################




shuttle$use2 <- 1 * (shuttle$use == "auto")
table(shuttle$use, shuttle$use2)


fit <- glm(use2 ~ wind - 1, binomial, shuttle)
fit2 <- glm(use2 ~ wind + magn - 1, binomial, shuttle)
fit3 <- glm(1-use2 ~ wind - 1, binomial, shuttle)


plot(shuttle$wind, fit$fitted, pch = 19)
plot(shuttle$wind, fit3$fitted, pch = 19)

summary(fit)$coef
summary(fit2)$coef
summary(fit3)$coef

exp(coef(fit)[1]) / exp(coef(fit)[2])
exp(coef(fit2)[1]) / exp(coef(fit2)[2])


data(InsectSprays)
str(InsectSprays)
fit4 <- glm(count ~ factor(spray) -1, family = "poisson", data = InsectSprays)
summary(fit4)$coef
exp(coef(fit4)[1]) / exp(coef(fit4)[2])


count <- rpois(100, 4)
x <- rep(c(1,0), each = 50)
t <- log(abs(rnorm(100)))
t2 <- log(10) + t
fit5.1 <- glm(count ~ x + offset(t), family = poisson)
fit5.2 <- glm(count ~ x + offset(t2), family = poisson)

summary(fit5.1)$coef
summary(fit5.2)$coef



x <- -5:5
y <- c(5.12, 3.93, 2.67, 1.87, 0.52, 0.08, 0.93, 2.05, 2.54, 3.87, 4.97)
knot <- 0
splineTerms <- (x > knot) * (x - knot)
xMat <- cbind(1, x, splineTerms)
fit6 <- lm(y ~ xMat - 1)
yhat <- predict(fit6)
plot(x, y, frame = FALSE, pch = 21, bg = "lightblue", cex = 2)
lines(x, yhat, col = "red", lwd = 2)
summary(fit6)$coef
yhat[11]-yhat[10]
