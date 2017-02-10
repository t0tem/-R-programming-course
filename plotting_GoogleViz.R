install.packages("googleVis")
library(googleVis)

dates <- seq(as.Date("2011/1/1"), as.Date("2011/12/31"), "days")
happiness <- rnorm(365)^ 2
happiness[333:365] <- happiness[333:365]  * 3 + 20
Title <- NA
Annotation <- NA
df <- data.frame(dates, happiness, Title, Annotation)
df$Title[333] <- "Discovers Google Viz"
df$Annotation[333] <- "Google Viz API interface by Markus Gesmann causes acute increases in happiness."

### Everything above here is just for making up data ### 
## from here down is the actual graphics bits        ###
AnnoTimeLine  <- gvisAnnotatedTimeLine(df, datevar="dates",
                                       numvar="happiness", 
                                       titlevar="Title", annotationvar="Annotation",
                                       options=list(displayAnnotations=TRUE,
                                                    legendPosition='newRow',
                                                    width=600, height=300)
)
# Display chart
plot(AnnoTimeLine) 
# Create Google Gadget
cat(createGoogleGadget(AnnoTimeLine), file="annotimeline.xml")
