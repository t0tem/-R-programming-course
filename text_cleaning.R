###############################1
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"

download.file(url, "data/f1.csv")
f <- read.csv("data/f1.csv")
names(f)
f_s <- strsplit(names(f), "wgtp")
f_s[[123]]

################################2

url2 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
download.file(url2, "data/f2.csv")
f2 <- read.csv("data/f2.csv", skip=4)
f2 <- f2[1:190,1:5]
head(f2)
names(f2) <- NULL
class(f2[,5])
names(f2) <- c("","",'','',"GDP")
class(f2$GDP)
f2[,5] <- as.character(f2[,5])

f2[,5] <- gsub(",","",f2[,5])
f2[,5] <- as.numeric(f2[,5])

mean(f2[,5], na.rm =T)

head(f2[,5])
f2[190:195,]

###############################3

f2 <- f2[,c(1,2,4,5)]

f2[,1] <- as.factor(as.character(f2[,1]))

head(f2)
tail(f2)
colnames(f2)[3] <- "countryNames"
countryNames <- f2$countryNames

grep("^United",countryNames)

###############################4

url4 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
download.file(url4, "data/f4.csv")
f4 <- read.csv("data/f4.csv")

colnames(f2)[1] <- "CountryCode"

f2_4 <- merge(f2,f4, by = "CountryCode")

names(f2_4)
head(f2_4)

x <- as.character(f2_4$Special.Notes)
x_fiscal <- x[grepl("Fiscal year end", x)]
length(grep("June", x_fiscal))

############################################5

install.packages("quantmod")
library(quantmod)
amzn <- getSymbols("AMZN",auto.assign=FALSE)
sampleTimes <- index(amzn)
d1 <- sampleTimes[format(sampleTimes, "%Y")=="2012"]
format(d1, "%a")
d2 <- d1[format(d1, "%a") == "Пн"]
c(length(d1), length(d2))









