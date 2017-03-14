suppressPackageStartupMessages(library(googleVis))
M <- gvisMotionChart(Fruits, "Fruit", "Year",
                     options=list(width=600, height=400))
plot(M)
# print(M, file = "test.html")


##Plots on maps
G <- gvisGeoChart(Exports, locationvar="Country",
                  colorvar="Profit",options=list(width=600, height=400))
#print(G,"chart")
plot(G)


##Specifying a region
G2 <- gvisGeoChart(Exports, locationvar="Country",
                   colorvar="Profit",options=list(width=600, height=400,region="150"))
#print(G2,"chart")
plot(G2)

#options here
#https://developers.google.com/chart/interactive/docs/gallery/geochart


##Combining
G <- gvisGeoChart(Exports, "Country", "Profit", 
                  options=list(width=200, height=100))
T1 <- gvisTable(Exports, 
                options=list(width=200, height=270))
M <- gvisMotionChart(Fruits, "Fruit", "Year", 
                     options=list(width=400, height=370))
GT <- gvisMerge(G,T1, horizontal=FALSE)
GTM <- gvisMerge(GT, M, horizontal=TRUE, 
                 tableOptions="bgcolor=\"#CCCCCC\" cellspacing=10")

plot(GTM)




