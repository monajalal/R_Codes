Rprof("montecarlopi.Rout")
# ----------------------------------------------------------------------
# Part 2: Run a Monte Carlo simulation to estimate pi.

# ----------------------------------------------------------------------
# Description: magnitude returns the distance a of point (x, y) from
#   the origin (or a vector of such distances if x and y are vectors).
# Usage: magnitude(x, y)
# Arguments:
#   x: x-coordinate of one point (or x-coordinates of several points) 
#   y: y-coordinate of one point (or y-coordinates of several points)
#      (must have same length as x)
# Value: distance(s) of (x, y) from the origin.
# Hint: don't forget to check that x and y have the same length.
magnitude = function(x, y) {
  stopifnot(length(x)==length(y))
  n <- length(x)
  ret <- NULL
  for(i in 1:n){
    ret[i] <- x[i]^2 + y[i]^2
  }
  for(i in 1:n){
    ret[i] <- sqrt(ret[i])
  }
  return(ret)
}
# Here are some test cases, first passing just one (x, y) point.
stopifnot(isTRUE(all.equal(magnitude(3,  4), 5)))
stopifnot(isTRUE(all.equal(magnitude(3, -4), 5)))
stopifnot(isTRUE(all.equal(magnitude(0, 0), 0)))
stopifnot(isTRUE(all.equal(magnitude(-1, 1), sqrt(2))))
# Now test passing vectors of points (x, y).
test.x   = c(3,  3, 0,      -1,  5)
test.y   = c(4, -4, 0,       1, 12)
test.mag = c(5,  5, 0, sqrt(2), 13)
stopifnot(isTRUE(all.equal(magnitude(test.x, test.y), test.mag)))

# Description: monte.carlo.pi estimates pi by simulation.
# Usage: monte.carlo.pi(n, draw=FALSE)
# Arguments:
#   n: number of random points to use
#   draw: logical, whether or not to draw a graph of the simulation
#     consisting of the n points and the unit circle.
# Details: The simulation proceeds by getting n random points {(x, y)}
#   in the square defined by -1 < x < 1 and -1 < y < 1, and then
#   counting how many of those points are within a radius 1 of (0, 0).

#   We can use
#     (area of circle)/(area of square) = pi*r^2  / (2r)^2
#                                       = pi(1^2) / (2*1)^2, since r=1
#                                       = pi/4
#   so
#     pi = 4*(area of circle)/(area of square)
#        =~ 4*(#points in circle) / (#points in square)
# Value: an estimate of pi.
# Hints:
#   - ?runif tells how to get random coordinates in the range (-1, 1)
#   - You can see a picture like the required drawing at the top of
#     http://en.wikipedia.org/wiki/Monte_Carlo_Integration. (Don't
#     worry if you don't understand the article, which isn't easy.)
#   - Using R's base graphics, you can plot the points via plot(x, y).
#   - One way to draw the circle is to get a sequence of angles (in
#     radians) from 0 to 2*pi, and then use x coordinate cos(angle)
#     and y coordinate sin(angle).
monte.carlo.pi = function(n, draw=FALSE) {
  x <- NULL
  y <- NULL
  for(i in 1:n){
    x[i] <- runif(1, -1, 1)
    y[i] <- runif(1, -1, 1)
  }
  dist <- NULL
  for(i in 1:n){
    dist[i] <- magnitude(x[i], y[i])
  }
  inside <- NULL
  for(i in 1:n){
    inside[i] <- (dist[i] < 1)
  }
  if(draw){
    plot(x,y, col=(inside+1))
    theta <- seq(0, 2*pi, length.out=100)
    lines(cos(theta),sin(theta))
  }
  return(4*sum(inside)/n)
}
cat(sep="", "monte.carlo.pi(1000)=", monte.carlo.pi(1000), "\n") # get estimate
monte.carlo.pi(1000, TRUE) # make graph

# Finally, make a graph to show the distribution of pi estimates for
# n=100 and n=1000. Use replicate() to get 500 pi estimates using
# n=100 each time. Then use replicate() to get 500 pi estimates using
# n=1000 each time. Draw a graph comparing these two distributions.
# (You can use a reasonable graph of your choice. We suggest overlaid
# density plots.)

n.100 <- replicate(500, monte.carlo.pi(100))
Rprof(NULL)
