
library(kernlab)
data(spam)
head(spam)

plot(density(spam$your[spam$type=="nonspam"]),
     col="blue",main="",xlab="Frequency of 'your'")
lines(density(spam$your[spam$type=="spam"]),col="red")

hist(spam$your[spam$type=="nonspam"])


### IN sample vs OUT OF sample errors

library(kernlab); data(spam); set.seed(333)
smallSpam <- spam[sample(dim(spam)[1],size=10),]
spamLabel <- (smallSpam$type=="spam")*1 + 1
plot(smallSpam$capitalAve,col=spamLabel)

## 'perfect' rule (but only for this sample)
#capitalAve $>$ 2.7 = "spam"
#capitalAve $<$ 2.40 = "nonspam"
#capitalAve between 2.40 and 2.45 = "spam"
#capitalAve between 2.45 and 2.7 = "nonspam"

rule1 <- function(x){
    prediction <- rep(NA,length(x))
    prediction[x > 2.7] <- "spam"
    prediction[x < 2.40] <- "nonspam"
    prediction[(x >= 2.40 & x <= 2.45)] <- "spam"
    prediction[(x > 2.45 & x <= 2.70)] <- "nonspam"
    return(prediction)
}
table(rule1(smallSpam$capitalAve),smallSpam$type)

#simplier rule
rule2 <- function(x){
    prediction <- rep(NA,length(x))
    prediction[x > 2.8] <- "spam"
    prediction[x <= 2.8] <- "nonspam"
    return(prediction)
}
table(rule2(smallSpam$capitalAve),smallSpam$type)

#applying to all data
table(rule1(spam$capitalAve),spam$type)
table(rule2(spam$capitalAve),spam$type)
mean(rule1(spam$capitalAve)==spam$type)
mean(rule2(spam$capitalAve)==spam$type)

#accuracy
sum(rule1(spam$capitalAve)==spam$type)
sum(rule2(spam$capitalAve)==spam$type)
