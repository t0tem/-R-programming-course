library(ggplot2)
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
