library(lattice)
xyplot(Ozone ~ Wind, data = airquality)
airquality <- transform(airquality, Month=factor(Month))
x <- xyplot(Ozone ~ Wind | Month, data = airquality, layout=c(3,2))

set.seed(10)
x <- rnorm(100)
f <- rep(0:1, each = 50)
y <- x -f*x + rnorm(100, sd = 0.5)
f <- factor(f, labels = c("Group1", "Group2"))
xyplot(y ~ x | f, panel=function (x, y, ...) {
    panel.xyplot(x, y, ...)
    panel.abline(h = median(y), lty = 2 )
})

xyplot(y ~ x | f, panel=function (x, y, ...) {
    panel.xyplot(x, y, ...)
    panel.lmline(x, y, col =2)
})

