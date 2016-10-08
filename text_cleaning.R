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
f2 <- f2[c(1:5)]
head(f2)
names(f2) <- NULL
class(f2[,5])
names(f2) <- c("","",'','',"GDP")
class(f2$GDP)
f2[,5] <- as.character(f2[,5])

sub(",","",f2[,5])
