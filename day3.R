rm(list=ls())

# Write the function is.prime(), below.
#
# Description: is.prime() figures out whether a number is prime
# Usage: is.prime(n)
# Parameters:
#   n: an integer (must have 0 <= n)
# Value: TRUE or FALSE, whether or not n is prime
is.prime = function(n) {
  # ...
}
# test cases
stopifnot(!is.prime(0))
stopifnot(!is.prime(1))
stopifnot( is.prime(2))
stopifnot( is.prime(3))
stopifnot(!is.prime(4))
stopifnot( is.prime(5))
for (i in 1:100) {
  if (is.prime(i)) {
    cat(sep="", i, " ")
  }
}
cat("\n")

# Write the function moving.average(), below.
# 
# Description: moving.average() computes moving averages over a vector.
# Usage: moving.average(x, n=2)
# Parameters:
#   x: a numeric vector of data values
#   n: length of moving average window (must have 1 <= n <= length(x))
# Value: a vector v such that
#   - for i in 1:(n-1), v[i] is 0 (because no moving average exists)
#   - for i in n:length(x), v[i] is the average of n values up to x[i]
moving.average = function(x, n=2) {
  # ...
}
# test cases
stopifnot(isTRUE(all.equal(moving.average(x=c(1, 2, 3),    n=1), c(1, 2,   3))))
stopifnot(isTRUE(all.equal(moving.average(x=c(1, 2, 3),    n=2), c(0, 1.5, 2.5))))
stopifnot(isTRUE(all.equal(moving.average(x=c(1, 2, 3),    n=3), c(0, 0,   2))))
stopifnot(isTRUE(all.equal(moving.average(x=c(1, 2, 3, 4), n=2), c(0, 1.5, 2.5, 3.5))))

# Make a graph of the daily temperatures in the built-in data frame
# "airquality" with day number (since May 1) on the x-axis and
# temperature on the y-axis. Smooth it with 1-day, 7-day, and 30-day
# moving averages. (Well, a 1-day moving average isn't smoothed at
# all; it's a degenerate case of smoothing.)

# ...