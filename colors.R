pal <- colorRamp(c("red", "blue"))

library(RColorBrewer)

cols <- brewer.pal(3, "BuPu")

pal <- colorRampPalette(cols)

image(volcano, col = pal(100))

x <- rnorm(1000)
y <- rnorm(1000)

plot(x, y, col = rgb(0.5, 0.5, 1, 0.2), pch = 19)
