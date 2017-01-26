n = 100; x = rnorm(n); x2 = rnorm(n); x3 = rnorm(n)
y = 1 + x + x2 + x3 + rnorm(n, sd = .1)
ey = resid(lm(y ~ x2 + x3))
ex = resid(lm(x ~ x2 + x3))

sum(ey * ex) / sum(ex ^ 2)

coef(lm(ey ~ ex - 1))


y <- 56 + 1.5*x + 2.5*x2 + 3.3*x3 + rnorm(n, sd = 0.1)

fit <- lm(y ~ x + x2 + x3)

summary(fit)$coefficients

conf <- confint(fit)
