library(ggplot2)
str(mpg)

#colors by factor
qplot(displ, hwy, data = mpg, color = drv)

#geom
qplot(displ, hwy, data = mpg, geom = c("point", "smooth"))

#hist
qplot(hwy, data = mpg, fill = drv)

#facets
qplot(displ, hwy, data = mpg, facets = .~drv)
qplot(hwy, data = mpg, facets = drv~., binwidth = 2)

dev.copy(png, file="file1.png")
dev.off()
dev.cur()