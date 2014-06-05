# Name: Mona Jalal
# Email: jalal@wisc.edu

# We'll grade your homework by running
#   source("hw2.R")
# in a directory containing a "groceries.csv" grocery price list file
# (which will be different from the one you used), giving user input,
# checking your output, and reading your code.
#
# To make it possible partly to automate grading, your output strings
# should exactly match the specified output strings.

rm(list = ls())

# ----------------------------------------------------------------------
# Part 1: Write the floor.log2() function described below.
#
# Description: log2floor(n) computes the largest integer exponent, e,
# such that 2^e <= n. That is, it computes floor(log(n, base=2)).
#
# Usage: floor.log2(n)
# Parameter: n, a number (must be >= 1)
# Value: floor(log(n, base=2)
# Examples:
#   floor.log2(1) is 0
#   floor.log2(2) is 1
#   floor.log2(3) is 1
#   floor.log2(4) is 2
#   floor.log2(5) is 2
#   floor.log2(6) is 2
#   floor.log2(7) is 2
#   floor.log2(8) is 3
# (You may not use "^" or log() or any other function. Use stopifnot()
# to check the argument according to the "must" statement above.)
floor.log2 = function(n) {
  count=0
  if (n<2)
  {return(0)}
  while ((n %/% 2)!=0)
  {
    n=n/2
    count=count+1
  }
  return(count)
}


# Write code to confirm that floor.log2(n) matches floor(log(n, base=2))
# on integer inputs from 1 to 1000.

for (i in 1:1000)
  stopifnot(isTRUE(all.equal(floor.log2(i),floor(log2(i)))))

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
x=replicate(100, monte.carlo.pi(500))
y=replicate(1000, monte.carlo.pi(500))
plot(density(x))
lines(density(y))


Sys.sleep(2) # sleep 2 seconds to let user see graph

# ----------------------------------------------------------------------
# Part 3: Manage a grocery list.
#
# Write a function, how.many, that takes two arguments, item
# (character string) and n.max (numeric). Display the prompt,
# "How many item?", and then require the user to enter desired number
# of item, an integer between 0 and n.max. (Assume the user will enter
# a nonnegative integer.) If user enters a number larger than n.max,
# display the message, "  ERROR: too many for the budget" and start
# again. Return the user's number.
#
# e.g. the call
#   how.many("apple", 4)
# prints "How many apple?" and returns a number between 0 and 4.

# your code here ...
how.many<-function(fruit, number){
  string<-paste("How many",fruit,"?",sep=" ")
  fruit.number<-as.numeric(readline(string))
  while(fruit.number > number){
    print("ERROR: too many for the budget")
    string<-paste("How many",fruit,"?",sep=" ")
    fruit.number<-as.numeric(readline(string))
  }
  return(as.numeric(fruit.number))
}

grocery.list <- function(file,budget) {
  item.count<-0
  updated.price=budget
  final.price<-0
  purchased.items <- data.frame(count= numeric(0), item= character(0), price = numeric(0),quantity=numeric(0))
  outf<-read.csv(file,header=FALSE)
  colnames(outf)<-c("item","price")
  mat = as.matrix(outf)
  colnames(mat) <- NULL
  m.dim=dim(mat)
  for (i in 1:m.dim[1])
  {
    
    if (updated.price-as.numeric(mat[i,2])>0)
    {
      number=how.many(mat[i,1],updated.price/as.numeric(mat[i,2]))
      updated.price=updated.price- (number* (as.numeric(mat[i,2])) )
      if (number>0){
        item.count=item.count+1
        new_row=data.frame(item=mat[i,1],price=as.numeric(mat[i,2]),quantity=number)
        purchased.items<-rbind(purchased.items,new_row)
      }
      
    }
    
  }
  colnames(purchased.items)=c("item","price", "quantity" )
  print(purchased.items)
  
  print(purchased.items)
  p.mat=as.matrix(purchased.items)
  colnames(p.mat)<-NULL
  rownames(p.mat)<-NULL
  p.dim<-dim(p.mat)
  for (i in 1:p.dim[1])
    final.price<- final.price+ (as.numeric(p.mat[i,2]))*(as.numeric(p.mat[i,3]))
  
  return(final.price)
  
}






# Write a function, grocery.list, that takes two arguments, file
# (character string) and budget (numeric). From file, it reads a
# grocery price list containing items and prices, like this:
#   spinach,2.00
#   rice,3.00
#   toilet paper,4.00
#   bread,2.40
#   milk,3.10
#   apple,0.40
#
# Display the price list. Loop through it asking how many of each item
# should be purchased. (Use your how.many() function.)
#
# budget is the maximum amount that can be spent. Do not accept a
# users's number of an item if it causes the budget to be exceeded. Do
# not display an item at all if it's price is higher than the
# remaining budget.
#
# Return a data frame consisting of three columns, "item", "price",
# and "quantity", and those rows with nonzero quantities.

# your code here ...

# Finally, write a few lines of code to call your grocery.list() on a
# "groceries.csv" file with a budget of $10. Print the returned
# shopping list (data frame) along with the total bill, in a line of
# the form, "Your bill is $n", where n is a number.

gl=grocery.list("groceries.csv",10)
string<-paste("Your bill is $",gl,sep="")
print(string)
# your code here ...



# e.g. Here's a sample session:
#   > source("hw2.R")
#             item price
#   1      spinach   2.0
#   2         rice   3.0
#   3 toilet paper   4.0
#   4        bread   2.4
#   5         milk   3.1
#   6        apple   0.4
#   How many spinach?
#   1: 2
#   How many rice?
#   1: 1
#   How many bread?
#   1: 1
#   How many apple?
#   1: 2
#     ERROR: too many for the budget
#   How many apple?
#   1: 1
#        item price quantity
#   1 spinach   2.0        2
#   2    rice   3.0        1
#   3   bread   2.4        1
#   4   apple   0.4        1
#   Your bill is $9.8
#   >