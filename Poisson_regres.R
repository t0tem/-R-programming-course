#####################################
###       website data example    ###
#####################################

download.file("https://dl.dropboxusercontent.com/u/7710864/data/gaData.rda", 
              destfile="./data/gaData.rda")

load("./data/gaData.rda")

gaData$julian <- julian(gaData$date)
head(gaData)

plot(gaData$julian, gaData$visits, 
     pch=19,col="darkgrey", xlab="Julian", ylab="Visits")


#linear regression line
plot(gaData$julian, gaData$visits, 
     pch=19, col="darkgrey", xlab="Julian", ylab="Visits")

lm1 <- lm(gaData$visits ~ gaData$julian)

abline(lm1, col="red", lwd=3)

#Exponentiating coefficients
round(exp(coef(lm(I(log(gaData$visits + 1)) ~ gaData$julian))), 5)

#Poisson regression in R
plot(gaData$julian, gaData$visits, pch=19, 
     col="darkgrey", xlab="Julian", ylab="Visits")

glm1 <- glm(gaData$visits ~ gaData$julian, family="poisson")

abline(lm1, col="red", lwd=3)
lines(gaData$julian, glm1$fitted, col="blue", lwd=3)


#fitted rates in R
glm2 <- glm(gaData$simplystats ~ julian(gaData$date), offset = log(visits+1),
            family = "poisson", data = gaData)

plot(julian(gaData$date), gaData$simplystats, col="blue", pch=19, xlab="Date", ylab="SimplyStats")

plot(julian(gaData$date), glm2$fitted, col="blue", pch=19, xlab="Date", ylab="Fitted Counts")

points(julian(gaData$date),glm1$fitted,col="red",pch=19)



glm2 <- glm(gaData$simplystats ~ julian(gaData$date),offset=log(visits+1),
            family="poisson", data=gaData)

plot(julian(gaData$date), gaData$simplystats/(gaData$visits+1), col="grey", xlab="Date",
     ylab="Fitted Rates", pch=19)

lines(julian(gaData$date), glm2$fitted/(gaData$visits+1), col="blue", lwd=3)


