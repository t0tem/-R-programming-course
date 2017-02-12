library(ggplot2)


#1
data(presidential)
data(economics)

events <- presidential[-(1:3),]
baseline = min(economics$unemploy)
delta = 0.05 * diff(range(economics$unemploy))
events$ymin = baseline
events$timelapse = c(diff(events$start),Inf)
events$bump = events$timelapse < 4*370 # ~4 years
offsets <- rle(events$bump)
events$offset <- unlist(mapply(function(l,v) {if(v){(l:1)+1}else{rep(1,l)}}, l=offsets$lengths, v=offsets$values, USE.NAMES=FALSE))
events$ymax <- events$ymin + events$offset * delta

ggplot() +
      geom_line(mapping=aes(x=date, y=unemploy), data=economics , size=3, alpha=0.5) +
      geom_segment(data = events, mapping=aes(x=start, y=ymin, xend=start, yend=ymax)) +
      geom_point(data = events, mapping=aes(x=start,y=ymax), size=3) +
      geom_text(data = events, mapping=aes(x=start, y=ymax, label=name), hjust=-0.1, vjust=0.1, size=6) +
      scale_x_date("time") +  
      scale_y_continuous(name="unemployed [1000's]")


###2 
#Credits to Vasilena Taralova from her mtcars project of regression models 
# (Coursera Data Science classmate)

require(ggplot2); require(gridExtra)

g1 <- ggplot (data = mtcars, aes (x = factor (0), y = mpg)) +
    geom_boxplot () +
    stat_summary (fun.y = mean, geom = 'point', shape = 16, col = "red", cex = 2) +
    ylab ("Mpg") +
    xlab ("") +
    scale_x_discrete (breaks = NULL) +
    ggtitle ("Box plot of mpg by transmission type") +
    facet_wrap (~am) +
    theme (plot.title = element_text (size = 10, face = "bold"))

g2 <- ggplot (data = mtcars, aes (x = mpg)) +
    geom_histogram ( aes (y = ..density..), binwidth = 3, colour = "black", alpha = 0.2) +
    stat_function (fun = dnorm,
                   args = list (mean = mean (mtcars$mpg), sd = sd (mtcars$mpg)),
                   lwd = 1, col = "red") +
    scale_x_continuous (breaks = seq (0, 50, 5)) +
    labs (x = "Mpg",
          y = "Density") +
    ggtitle ("Normalized histogram and \n the normal probability density function") +
    theme (plot.title = element_text (size = 10, face = "bold"))

grid.arrange (grobs = list (g1,g2), ncol = 2)
