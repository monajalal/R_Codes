baby.prop.test = function (x, n, p, conf.level = 0.95) {
  test<-list()
  p.hat = x / n
  alpha=1-conf.level
  z = (p.hat - p) / sqrt(p*(1-p)/n)
  p.value=2*(1-pnorm(z,0,1))
  #CI=c(p.hat + z_{alpha/2} * sqrt(p.hat*(1-p.hat)/n), p.hat - z_{alpha/2} * sqrt(p.hat*(1-p.hat)/n))
  CI=c(p.hat - (abs(qnorm(alpha/2))) * sqrt(p.hat*(1-p.hat)/n),p.hat + (abs(qnorm(alpha/2))) * sqrt(p.hat*(1-p.hat)/n))
  
  test$null.value<-p
  test$estimate<-p.hat
  test$statistics<-z
  test$p.value<-p.value
  test$conf.int<-CI
  return(test)
}
# test case
baby.prop = baby.prop.test(72, 100, .7, conf.level=.99)
stopifnot(isTRUE(all.equal(as.numeric(baby.prop$statistic), .43643578)))
stopifnot(isTRUE(all.equal(as.numeric(baby.prop$p.value), .66252058)))
stopifnot(isTRUE(all.equal(as.numeric(baby.prop$conf.int), c(.60434555, .83565445))))
stopifnot(isTRUE(all.equal(as.numeric(baby.prop$estimate), .72)))
stopifnot(isTRUE(all.equal(as.numeric(baby.prop$null.value), .7)))