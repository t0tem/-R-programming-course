#####
# 1 #
#####

x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)
n <- length(y)

beta1 <- cor(y, x) * sd(y) / sd(x) #slope
beta0 <- mean(y) - beta1 * mean(x) #intercept

e <- y - beta0 - beta1 * x #residuals
sigma <- sqrt(sum(e^2) / (n-2)) #estimate of residual standard deviation

ssx <- sum((x - mean(x))^2) #sums of squares of X's
seBeta1 <- sigma / sqrt(ssx) #standard error of the Slope
tBeta1 <- beta1 / seBeta1 #t-statistic to test beta1

# p-values of 2-sided test beta1 is 0
pBeta1 <- 2 * pt(abs(tBeta1), df = n - 2, lower.tail = FALSE)

pBeta1

#OR...
summary(lm(y ~ x))$coef[2,4]

#####
# 2 #
#####

sigma

#####
# 3 #
#####

library(datasets); data(mtcars)
fit <- lm(mpg ~ wt, mtcars)
sumCoef <- summary(fit)$coefficients
sumCoef

interv <- mean(mtcars$mpg) + c(-1, 1) * qt(.975, df = fit$df) * sumCoef[2, 2]
interv[1]

#####
# 5 #
#####

predict(fit, newdata = data.frame(wt = 3), interval = ("prediction"))

#####
# 6 #
#####

fit <- lm(mpg ~ wt, data = mtcars)
confint(fit)[2, ] * 2

#or...
fit_1 <- lm(mpg ~ I(wt * 0.5), data = mtcars)
confint(fit_1)[2, ]


#####
# 9 #
#####

fit2 <- lm(mpg ~ 1 , mtcars)

sum(fit$res^2) / sum(fit2$res^2)

#or...
1 - summary(fit)$r.squared

#or...
sum((predict(fit) - mtcars$mpg)^2) / sum((predict(fit2) - mtcars$mpg)^2)



