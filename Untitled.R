# Description: baby.prop.test performs a z-test and computes a
#   confidence interval for an unknown proportion.
# Usage: baby.prop.test(x, n, p, conf.level = .95)
# Parameters:
#   x: the number of successes in a sample (must be in [0, n])
#   n: the sample size (must be greater than 0)
#   p: the hypothesized true proportion (must be in [0, 1]) 
#   conf.level: confidence level of the interval (must be in (0, 1))
# Details: The test is for the null hypothesis that the true proportion
#   is p against the alternative that it isn't p. The statistic is
#     z = (p.hat - p) / sqrt(p*(1-p)/n),
#   where
#     p.hat = x / n
#   Note: This z is approximately N(0,1) under the null hypothesis.
#   The interval is
#     p.hat +/- z_{alpha/2} * sqrt(p.hat*(1-p.hat)/n),
#   where alpha = 1 - conf.level.
# Value: a list containing these components:
#   $statistic: the z statistic
#   $p.value: probability of a z-statistic more extreme than the one computed
#   $conf.int: a confidence interval for the true proportion using
#     confidence level conf.level
#   $estimate: the estimated (sample) proportion, p.hat
#   $null.value: the specifited hypothesized value of the proportion, p
#
# (You may not use the real prop.test() or chisq.test() or related
# functions. Use stopifnot() to check the arguments according to the
# "must" statements above.)
#
baby.prop.test = function (x, n, p, conf.level = 0.95) {
  # ...
  p.hat = x / n
  baby.prop.test$null.value<-p.hat
  return(baby.prop.test)
}
# test case
baby.prop = baby.prop.test(72, 100, .7, conf.level=.99)
stopifnot(isTRUE(all.equal(as.numeric(baby.prop$statistic), .43643578)))
stopifnot(isTRUE(all.equal(as.numeric(baby.prop$p.value), .66252058)))
stopifnot(isTRUE(all.equal(as.numeric(baby.prop$conf.int), c(.60434555, .83565445))))
stopifnot(isTRUE(all.equal(as.numeric(baby.prop$estimate), .72)))
stopifnot(isTRUE(all.equal(as.numeric(baby.prop$null.value), .7)))