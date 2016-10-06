url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv"

dir.create("data")

download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv", "data/file.csv")

acs <- read.csv("data/file.csv")

install.packages("sqldf")
library(sqldf)

x <-sqldf("select pwgtp1 from acs where AGEP < 50")


sqldf("select distinct AGEP from acs")
sqldf("select AGEP where unique from acs")


#################################

con <- "http://biostat.jhsph.edu/~jleek/contact.html"
html <- readLines(con)
close(con)
nchar(html[c(10,20,30, 100)])

################################

url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fwksst8110.for"

download.file(url, "data/file2.for")

x <- read.fwf("data/file2.for", skip=4, widths=c(10, 9,4,9,4,9,4,9,4))
colSums(x[4])
