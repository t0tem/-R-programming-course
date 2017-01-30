n <- 100
plot(c(1, n), 0 : 1, type = "n", frame = FALSE, xlab = "p", ylab = "R^2").

r <- sapply(1 : n, function(p) {
    
    y <- rnorm(n)
    x <- matrix(rnorm(n * p), n, p)
    summary(lm(y ~ x))$r.squared
    
    }
    )
lines(1 : n, r, lwd = 2)
abline(h = 1)


y1 <- rnorm(n)
x1 <- matrix(rnorm(n * 15), n, 15)
plot(lm(y1 ~ x1))
plot(x1[,2:3])
x1[,2:3][,1]



################################################################################


# Variance inflation #

#regressors are independent => no inflation (var is approx. the same)
n <- 100; nosim <- 1000

x1 <- rnorm(n); x2 <- rnorm(n); x3 <- rnorm(n); 

betas <- sapply(1 : nosim, function(i){
    y <- x1 + rnorm(n, sd = .3)
    c(coef(lm(y ~ x1))[2], 
      coef(lm(y ~ x1 + x2))[2], 
      coef(lm(y ~ x1 + x2 + x3))[2])
})

round(apply(betas, 1, sd), 5)


#regressors are related => variance of beta coef of x1 increases
n <- 100; nosim <- 1000
x1 <- rnorm(n); x2 <- x1/sqrt(2) + rnorm(n) /sqrt(2)
x3 <- x1 * 0.95 + rnorm(n) * sqrt(1 - 0.95^2); 
betas <- sapply(1 : nosim, function(i){
    y <- x1 + rnorm(n, sd = .3)
    c(coef(lm(y ~ x1))[2], 
      coef(lm(y ~ x1 + x2))[2], 
      coef(lm(y ~ x1 + x2 + x3))[2])
})
round(apply(betas, 1, sd), 5)


# Swiss data VIFs (Variance Inflation Factors)
data(swiss)
library(car)
fit <- lm(Fertility ~ . , data = swiss)
vif(fit)
sqrt(vif(fit)) #bcaffo prefers sd :) 


#Nested model testing
fit1 <- lm(Fertility ~ Agriculture, data = swiss)
fit3 <- update(fit, Fertility ~ Agriculture + Examination + Education)
fit5 <- update(fit, Fertility ~ Agriculture + Examination + Education + Catholic + Infant.Mortality)
anova(fit1, fit3, fit5)




