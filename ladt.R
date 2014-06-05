lad <- function(y = "farm", x = "land", data="http://pages.stat.wisc.edu/~gvludwig/327-5/FarmLandArea.csv") {
  dat <- setNames(read.csv(data)[, c(x, y)], c('x', 'y'))
  sum.abs.dev <- function(beta, data) {
    with(data, sum(abs(y - beta[1] - beta[2] * x)))
  }
  fit <- lm(y ~ x, dat)
  optim(par=coef(fit), sum.abs.dev, data=dat)$par
}

pars<-lad(y = "farm", x = "land", data="http://pages.stat.wisc.edu/~gvludwig/327-5/FarmLandArea.csv");
data="http://pages.stat.wisc.edu/~gvludwig/327-5/FarmLandArea.csv"
dat <- read.csv(data);
plot(dat$farm ~ dat$land);
abline(pars[1], pars[2],col = "blue", lty = 3)
plot(lm(dat$farm~dat$land))


