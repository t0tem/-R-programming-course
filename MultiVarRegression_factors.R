require(datasets);data(InsectSprays); require(stats); require(ggplot2)
str(InsectSprays)
#plotting the data
g = ggplot(data = InsectSprays, aes(y = count, x = spray, fill  = spray))
g = g + geom_violin(colour = "black", size = 2)
g = g + xlab("Type of spray") + ylab("Insect count")
g


# checking the coefficients
summary(lm(count ~ spray, data = InsectSprays))$coef
# T-test (p-value) test whether e.g. spray B is different from spray A

# same but doing it manually - so every level of factor if a separate predictor
# A is omitted as it's redundant (we use intercept instead)
summary(lm(count ~ 
               I(1 * (spray == 'B')) + I(1 * (spray == 'C')) + 
               I(1 * (spray == 'D')) + I(1 * (spray == 'E')) +
               I(1 * (spray == 'F'))
           , data = InsectSprays))$coef

# even if we add A - we get NA (it's redundant)
summary(lm(count ~ 
               I(1 * (spray == 'B')) + I(1 * (spray == 'C')) +  
               I(1 * (spray == 'D')) + I(1 * (spray == 'E')) +
               I(1 * (spray == 'F')) + I(1 * (spray == 'A')), 
           data = InsectSprays))

# but if we remove intercept => then A is there instead
summary(lm(count ~ spray - 1, data = InsectSprays))$coef
# but now coefficients are exactly means of corresponding factors
# while with intercept they were related to mean of A (intercept estimate)
# so T-test (p-value) tests whether e.g. spray B is killing any insects by itself (mean <> 0)

library(dplyr)
summarise(group_by(InsectSprays, spray), mn = mean(count))


## Releveling

spray2 <- relevel(InsectSprays$spray, "C")
summary(lm(count ~ spray2, data = InsectSprays))$coef
# now spray C is a reference level





## Examples with Swiss data

library(datasets); data(swiss)
head(swiss)

library(dplyr); 
swiss = mutate(swiss, CatholicBin = 1 * (Catholic > 50))

g = ggplot(swiss, aes(x = Agriculture, y = Fertility, colour = factor(CatholicBin)))
g = g + geom_point(size = 3, colour = "black") + geom_point(size = 2)
g = g + xlab("% in Agriculture") + ylab("Fertility")
g


#No effect of religion
summary(lm(Fertility ~ Agriculture, data = swiss))$coef

fit = lm(Fertility ~ Agriculture, data = swiss)
g1 = g
g1 = g1 + geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2], size = 2)
g1

#Parallel lines 
summary(lm(Fertility ~ Agriculture + factor(CatholicBin), data = swiss))$coef

fit = lm(Fertility ~ Agriculture + factor(CatholicBin), data = swiss)
g1 = g
g1 = g1 + geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2], size = 2)
g1 = g1 + geom_abline(intercept = coef(fit)[1] + coef(fit)[3], slope = coef(fit)[2], size = 2)
g1




# Lines with different slopes and intercepts

summary(lm(Fertility ~ Agriculture * factor(CatholicBin), data = swiss))$coef

fit = lm(Fertility ~ Agriculture * factor(CatholicBin), data = swiss)
g1 = g
g1 = g1 + geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2], size = 2)
g1 = g1 + geom_abline(intercept = coef(fit)[1] + coef(fit)[3], 
                      slope = coef(fit)[2] + coef(fit)[4], size = 2)
g1

##############################################################################
##                          Examples with mtcars data                       ##
##############################################################################

data(mtcars); str(mtcars)
require(ggplot2)


#plotting the data
g <- ggplot(data = mtcars, aes(y = mpg, x = wt, colour  = factor(cyl)))
g <- g + geom_point(size = 3, colour = "black") + geom_point(size = 2)
g <- g + xlab("weight") + ylab("mpg")
g

#No effect of cyl
summary(lm(mpg ~ wt, data = mtcars))$coef

fit0 <- lm(mpg ~ wt, data = mtcars)
g1 <- g
g1 <- g1 + geom_abline(intercept = coef(fit0)[1], 
                       slope = coef(fit0)[2], size = 1.2)
g1

#Parallel lines 
summary(lm(mpg ~ wt + factor(cyl), mtcars))$coef
# wt coefficient interpretation:
# The estimated expected change in mpg per one unit increase in wt 
# for a specific number of cylinders (4, 6, 8)

fit1 <- lm(mpg ~ wt + factor(cyl), mtcars)
g1 <- g
g1 <- g1 + geom_abline(intercept = coef(fit1)[1], 
                       slope = coef(fit1)[2], size = 1.2, color = "red")

g1 <- g1 + geom_abline(intercept = coef(fit1)[1] + coef(fit1)[3], 
                       slope = coef(fit1)[2], size = 1.2, color = "green")

g1 <- g1 + geom_abline(intercept = coef(fit1)[1] + coef(fit1)[4], 
                       slope = coef(fit1)[2], size = 1.2, color = "blue")
g1


# Lines with different slopes and intercepts
summary(lm(mpg ~ wt * factor(cyl), mtcars))$coef

fit2 <- lm(mpg ~ wt * factor(cyl), mtcars)
g1 <- g
g1 <- g1 + geom_abline(intercept = coef(fit2)[1], 
                       slope = coef(fit2)[2], size = 1.2, color = "red")

g1 <- g1 + geom_abline(intercept = coef(fit2)[1] + coef(fit2)[3], 
                       slope = coef(fit2)[2] + coef(fit2)[5], size = 1.2, color = "green")

g1 <- g1 + geom_abline(intercept = coef(fit2)[1] + coef(fit2)[4], 
                       slope = coef(fit2)[2] + coef(fit2)[6], size = 1.2, color = "blue")
g1


# plotting just 1 'factor fitted line'

fit2_3 <- lm(mpg ~ wt, mtcars[mtcars$cyl == 4, ])
summary(fit2_3)$coef

g1 <- g
g1 <- g1 + geom_abline(intercept = coef(fit2_3)[1], 
                       slope = coef(fit2_3)[2], size = 1.2, color = "red")
g1



