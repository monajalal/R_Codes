magnitude = function(x, y) {
  stopifnot(isTRUE(all.equal(length(x),length(y))))
  return (sqrt(x^2 + y^2))
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
#   - Using R's base graphics, you can plot the points via plot(x, y).
#   - One way to draw the circle is to get a sequence of angles (in
#     radians) from 0 to 2*pi, and then use x coordinate cos(angle)
#     and y coordinate sin(angle).

library("plotrix")

monte.carlo.pi<-function(n,draw=FALSE)
{
  circle.points<-0
  square.points<-0
  x<-runif(n,-1,1)
  y<-runif(n,-1,1)
  for (i in 1:n)
  {
    #if ((x[i])^2 + (y[i])^2 <=1)
    if (magnitude(x[i],y[i])<=1)
    {
      circle.points<-circle.points+1
      square.points<-square.points+1
    } else
    {
      square.points<-square.points+1
    }
  }
  if (draw==TRUE)
  {
    plot.new()
    frame()
    plot(x,y,asp=1,xlim=c(-1,1),ylim=c(-1,1))
    draw.circle(0,0,1,nv=1000,border=NULL,col=NA,lty=1,lwd=1)
    rect(-1,-1,1,1)
    
  }
  return(as.numeric(4*circle.points / square.points))
}


cat(sep="", "monte.carlo.pi(1000)=", monte.carlo.pi(1000), "\n") # get estimate
monte.carlo.pi(1000, TRUE) # make graph
Sys.sleep(2) # sleep 2 seconds to let user see graph

# Finally, make a graph to show the distribution of pi estimates for
# n=100 and n=1000. Use replicate() to get 500 pi estimates using
# n=100 each time. Then use replicate() to get 500 pi estimates using
# n=1000 each time. Draw a graph comparing these two distributions.
# (You can use a reasonable graph of your choice. We suggest overlaid
# density plots.)

# ... your code here
#x=replicate(100, monte.carlo.pi(500))
#y=replicate(1000, monte.carlo.pi(500))
#plot(density(x))
#lines(density(y))


Sys.sleep(2) # sleep 2 seconds to let user see graph
