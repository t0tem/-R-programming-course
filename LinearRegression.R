library(UsingR)
data(diamond)
library(ggplot2)
g = ggplot(diamond, aes(x = carat, y = price))
g = g + xlab("Mass (carats)")
g = g + ylab("Price (SIN $)")
g = g + geom_point(size = 7, colour = "black", alpha=0.5)
g = g + geom_point(size = 5, colour = "blue", alpha=0.2)
g = g + geom_smooth(method = "lm", colour = "black")
g

fit <- lm(price ~ carat, data = diamond)
coef(fit)
summary(fit)

fit2 <- lm(price ~ I(carat - mean(carat)), data = diamond)
coef(fit2)

fit3 <- lm(price ~ I(carat * 10), data = diamond)
coef(fit3)

#predicting
newx <- c(0.16, 0.27, 0.34)
coef(fit)[1] + coef(fit)[2] * newx
predict(fit, newdata = data.frame(carat = newx))



### Residuals

##knitr options
options(width = 100)
opts_chunk$set(message = F, error = F, warning = F, 
               comment = NA, fig.align = 'center', dpi = 100, 
               tidy = F, cache.path = '.cache/', fig.path = 'fig/')
##end of knitr options

options(scipen = 999)

library(UsingR)
data(diamond)
y <- diamond$price; x <- diamond$carat; n <- length(y)
fit <- lm(y ~ x)
e <- resid(fit) #getting residuals from linear model

#another way
yhat <- predict(fit)
max(abs(e -(y - yhat))) #diff is almost zero

max(abs(e - (y - coef(fit)[1] - coef(fit)[2] * x))) #also zero

sum(e) #sum of residuals is zero
sum(e * x)


#plotting
plot(diamond$carat, diamond$price,  
     xlab = "Mass (carats)", 
     ylab = "Price (SIN $)", 
     bg = "lightblue", 
     col = "black", cex = 1.5, pch = 21,frame = FALSE)
abline(fit, lwd = 2)
for (i in 1 : n) 
    lines(c(x[i], x[i]), c(y[i], yhat[i]), col = "red" , lwd = 2)


#plotting residuals vertically (Residuals versus X)
plot(x, e,  
     xlab = "Mass (carats)", 
     ylab = "Residuals (SIN $)", 
     bg = "lightblue", 
     col = "black", cex = 1.5, pch = 21,frame = FALSE)
abline(h = 0, lwd = 2)
for (i in 1 : n) 
    lines(c(x[i], x[i]), c(e[i], 0), col = "red" , lwd = 2)


###Heteroskedasticity
x <- runif(100, 0, 6); y <- x + rnorm(100,  mean = 0, sd = .001 * x); 
g = ggplot(data.frame(x = x, y = y), aes(x = x, y = y))
g = g + geom_smooth(method = "lm", colour = "black")
g = g + geom_point(size = 7, colour = "black", alpha = 0.4)
g = g + geom_point(size = 5, colour = "red", alpha = 0.4)
g

#+residual plot
g = ggplot(data.frame(x = x, y = resid(lm(y ~ x))), 
           aes(x = x, y = y))
g = g + geom_hline(yintercept = 0, size = 2); 
g = g + geom_point(size = 7, colour = "black", alpha = 0.4)
g = g + geom_point(size = 5, colour = "red", alpha = 0.4)
g = g + xlab("X") + ylab("Residual")
g


#Dimond data residual plot
e = c(resid(lm(price ~ 1, data = diamond)),
      resid(lm(price ~ carat, data = diamond)))
fit = factor(c(rep("Itc", nrow(diamond)),
               rep("Itc, slope", nrow(diamond))))
g = ggplot(data.frame(e = e, fit = fit), aes(y = e, x = fit, fill = fit))
g = g + geom_dotplot(binaxis = "y", size = 2, stackdir = "center", binwidth = 20)
g = g + xlab("Fitting approach")
g = g + ylab("Residual price")
g



# Residual Variance
y <- diamond$price; x <- diamond$carat; n <- length(y)
fit <- lm(y ~ x)
summary(fit)$sigma
sqrt(sum(resid(fit)^2) / (n - 2))

