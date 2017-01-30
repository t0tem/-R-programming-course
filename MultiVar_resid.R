data(swiss)
par(mfrow = c(2, 2))
fit <- lm(Fertility ~ . , data = swiss)

plot(fit)

#Patterns in your residual plots generally indicate some poor aspect of model fit. 
#These can include:
# -Heteroskedasticity (non constant variance).
# -Missing model terms.
# -Temporal patterns (plot residuals versus collection order).
#Residual QQ plots investigate normality of the errors.
#Leverage measures (hat values) can be useful for diagnosing data entry errors.
#Influence measures get to the bottom line, 
#'how does deleting or including this point impact a particular aspect of the model'.


#Case 1
n <- 100; x <- c(10, rnorm(n)); y <- c(10, c(rnorm(n)))
plot(x, y, frame = FALSE, cex = 2, pch = 21, bg = "lightblue", col = "black")
abline(lm(y ~ x))   

fit <- lm(y ~ x)
round(dfbetas(fit)[1 : 10, 2], 3)
round(hatvalues(fit)[1 : 10], 3)

#Case 2
x <- rnorm(n); y <- x + rnorm(n, sd = .3)
x <- c(5, x); y <- c(5, y)
plot(x, y, frame = FALSE, cex = 2, pch = 21, bg = "lightblue", col = "black")
fit2 <- lm(y ~ x)
abline(fit2)            

round(dfbetas(fit2)[1 : 10, 2], 3)
round(hatvalues(fit2)[1 : 10], 3)

#Stefanski example TAS 2007 Vol 61.

link <- 'http://www4.stat.ncsu.edu/~stefanski/NSF_Supported/Hidden_Images/orly_owl_files/orly_owl_Lin_4p_5_flat.txt'
dat <- read.table(link, header = FALSE)
pairs(dat)
summary(lm(V1 ~ . -1, data = dat))$coef
fit <- lm(V1 ~ . - 1, data = dat)
plot(predict(fit), resid(fit), pch = '.')









