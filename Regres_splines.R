###########################################################
###          fitting functions using linear models      ###
###########################################################

### REGRESSION SPLINES

n <- 500; 
x <- seq(0, 4 * pi, length = n); 
y <- sin(x) + rnorm(n, sd = .3)

knots <- seq(0, 8 * pi, length = 20); 

splineTerms <- sapply(knots, function(knot) (x > knot) * (x - knot))

#matrix of: intercept (1s), slopes (x), splines
xMat <- cbind(1, x, splineTerms)


yhat <- predict(lm(y ~ xMat - 1)) #removing intercept as it's in matrix already

#plotting
plot(x, y, frame = FALSE, pch = 21, bg = "lightblue", cex = 2)

lines(x, yhat, col = "red", lwd = 2)

#adding square terms
splineTerms <- sapply(knots, function(knot) (x > knot) * (x - knot)^2) #squared

xMat <- cbind(1, x, x^2, splineTerms) #adding x^2

yhat <- predict(lm(y ~ xMat - 1))

plot(x, y, frame = FALSE, pch = 21, bg = "lightblue", cex = 2)
lines(x, yhat, col = "red", lwd = 2)

# MODELING HARMONICS USING FOURIER BASIS
#(basis is the collection of splines and regressors - 'blocks' to build the function)

##Chord finder, playing the white keys on a piano from octave c4 - c5
notes4 <- c(261.63, 293.66, 329.63, 349.23, 392.00, 440.00, 493.88, 523.25)

t <- seq(0, 2, by = .001)
n <- length(t)

c4 <- sin(2 * pi * notes4[1] * t)
e4 <- sin(2 * pi * notes4[3] * t)
g4 <- sin(2 * pi * notes4[5] * t)

chord <- c4 + e4 + g4 + rnorm(n, 0, 0.3)

x <- sapply(notes4, function(freq) sin(2 * pi * freq * t)) #basis of all notes

fit <- lm(chord ~ x - 1)

#plotting
plot(c(0, 9), c(0, 1.5), xlab = "Note", ylab = "Coef^2", axes = FALSE, frame = TRUE, type = "n")

axis(2)
axis(1, at = 1 : 8, labels = c("c4", "d4", "e4", "f4", "g4", "a4", "b4", "c5"))
for (i in 1 : 8) abline(v = i, lwd = 3, col = grey(.8))
lines(c(0, 1 : 8, 9), c(0, coef(fit)^2, 0), type = "l", lwd = 3, col = "red")

library(seewave)
savewav(c4, f = 22050)
savewav(e4, f = 22050)
savewav(g4, f = 22050)
savewav(chord, f = 22050)








