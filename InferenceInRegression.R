library(UsingR); data(diamond)
y <- diamond$price; x <- diamond$carat; n <- length(y)

beta1 <- cor(y, x) * sd(y) / sd(x) #slope
beta0 <- mean(y) - beta1 * mean(x) #intercept

e <- y - beta0 - beta1 * x #residuals

sigma <- sqrt(sum(e^2) / (n-2)) #estimate of Std around the regression line

#sums of squares of X's (numerator of Variance calculation)
ssx <- sum((x - mean(x))^2)

#standard error of Intercept
seBeta0 <- (1 / n + mean(x) ^ 2 / ssx) ^ .5 * sigma

#standard error of the Slope
seBeta1 <- sigma / sqrt(ssx)

#t-statistic to test the hypothesis that beta0 is 0 (so we don't subtract 
#the true value as it's assumed to be zero)
tBeta0 <- beta0 / seBeta0

#t-statistic to test beta1
tBeta1 <- beta1 / seBeta1

#P-values. We double it as estimates are larger than zero (? not quite clear...)
pBeta0 <- 2 * pt(abs(tBeta0), df = n - 2, lower.tail = FALSE)
pBeta1 <- 2 * pt(abs(tBeta1), df = n - 2, lower.tail = FALSE)

#manual creation of coefficients table
coefTable <- rbind(c(beta0, seBeta0, tBeta0, pBeta0), 
                   c(beta1, seBeta1, tBeta1, pBeta1))
colnames(coefTable) <- c("Estimate", "Std. Error", "t value", "P(>|t|)")
rownames(coefTable) <- c("(Intercept)", "x")

#comparing manual to computational results
coefTable
fit <- lm(y ~ x); 
summary(fit)$coefficients

########################
##Confidence intervals##
########################

sumCoef <- summary(fit)$coefficients
sumCoef

#Estimate of Intercept +- 97.5 T-quantile with degrees of freedom as those from
#linear model times the standard error of Intercept
sumCoef[1,1] + c(-1, 1) * qt(.975, df = fit$df) * sumCoef[1, 2]

#Same for the slope. But we devide by 10 here as we want the increase in price
#NOT for 1 carat BUT for 0.1 carat (so delta is 10 times smaller)
(sumCoef[2,1] + c(-1, 1) * qt(.975, df = fit$df) * sumCoef[2, 2]) / 10

########################################
## Plotting the prediction intervals ###
########################################

library(ggplot2)
newx = data.frame(x = seq(min(x), max(x), length = 100))
p1 = data.frame(predict(fit, newdata= newx, interval = ("confidence")))
p2 = data.frame(predict(fit, newdata = newx, interval = ("prediction")))
p1$interval = "confidence"
p2$interval = "prediction"
p1$x = newx$x
p2$x = newx$x
dat = rbind(p1, p2)
names(dat)[1] = "y"

g = ggplot(dat, aes(x = x, y = y))
g = g + geom_ribbon(aes(ymin = lwr, ymax = upr, fill = interval), alpha = 0.2) 
g = g + geom_line()
g = g + geom_point(data = data.frame(x = x, y=y), aes(x = x, y = y), size = 2)
g





