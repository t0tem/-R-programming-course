#1

b <- c(140,138,150,148,135)
w <- c(132,135,151,146,130)
t.test(w, b, paired = TRUE)$p.value

#2
x <- qt(0.975, 8) 

# t_stat = (mu-mu0) / (s / sqrt(n))
# -x <= (mu - mu0) / (s / sqrt(n)) <= x
# -x * s / sqrt(n) + mu <= mu0 <= x * s / sqrt(n) + mu

mu <- 1100
s <- 30
n <- 9
c(-1, 1) * x * s / sqrt(n) + mu


#3
binom.test(x=3, n=4, p=0.5, alternative="greater")$p.value

#4
x <- 1 / 100 * 1787
poisson.test(10, r = 1/100*1787, alternative = "less" )$p.value

#5

n1 <- n2 <- 9
x1 <- -3 ##treated
x2 <- 1 ##placebo
s1 <- 1.5 ##treated
s2 <- 1.8 ##placebo
s <- sqrt(((n1 - 1) * s1^2 + (n2 - 1) * s2^2)/(n1 + n2 - 2))

ts <- (x1 - x2)/(s * sqrt(1/n1 + 1/n2))

2 * pt(ts, n1 + n2 - 2)

#6
qt(.95, 8)


#######################



#7
power.t.test(n = 100, delta = 0.01, sd = 0.04, type = "one.sample", alt = "one.sided")$power
pnorm(qnorm(0.95) * (0.04/sqrt(100)), mean = 0.01, sd = 0.04/sqrt(100), lower.tail = FALSE)

#8
power.t.test(power = 0.9, delta = 0.01, sd = 0.04, type = "one.sample", alt = "one.sided")$n

#9


