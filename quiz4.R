jalalQ4 <- read.csv("jalalQ4.csv", header=TRUE)
jalal <- read.csv("jalalQ4.csv", header=TRUE,sep=",")
aggregate(cbind(age, weight) ~ sex, data=jalal, FUN=mean)
jalal$count <- 1
aggregate(count ~sex+eye.color, data=jalal, FUN=length)
aggregate(count ~ hair.color, data=jalal, FUN=length)
aggregate(count ~eye.color, data=jalal, FUN=length)

aggregate(count ~hair.color, data=jalal, FUN=length)
sorted.jalal <- jalal[with(jalal, order(weight, decreasing=TRUE)), ]
sorted.jalal <- jalal[with(jalal, order(eye.color, -weight, decreasing=TRUE)), ]

sorted.jalal <- jalal[with(jalal, order(-xtfrm(eye.color), weight)), ]

sorted.jalal[sorted.jalal$sex == "M", ][4,]



boxplot(mtcars$mpg, main="Gas mileage", ylab="miles per gallon", ylim=c(0,40),cex.main=1.2,cex.axes=2,cex.lab=1.4)

cex changes the font size
boxplot is not good for tiny data sets while strip chart is good

//I should learn the difference
> stripchart(mpg ~ am, data=mtcars, method="stack")
> stripchart(mpg ~ am, data=mtcars, method="stack",pch=16)
> stripchart(mpg ~ am, data=mtcars, method="overplot",pch=16)
> stripchart(mpg ~ am, data=mtcars, method="jitter",pch=16)
//histogram is either based on frequency or histogram
hist(mtcars$mpg)
hist(mtcars$mpg,freq=FALSE)
//what is break used for?
> hist(mtcars$mpg,freq=FALSE,breaks=10)
> hist(mtcars$mpg,freq=FALSE,breaks=seq(from=10,to=36,by=4))

//scatter plot
> x=1:5; y=2*x
> plot(x,y) //slope is not ok!!!
  plot(x,y,xlim=c(0,10), ylim=c(0,10)) //fixes the slope
> plot(x,x,pch=15) //pch is just the shape (selecting the shape for dots)


??
> lines(1:5,5:1)
> x=seq(from=0, to=10, length.out=100)
> y=x^2
> lines(x,y)


d=density(mtcars$mpg)
> str(d)
List of 7
$ x        : num [1:512] 2.97 3.05 3.12 3.2 3.27 ...
$ y        : num [1:512] 0.000114 0.000125 0.000137 0.00015 0.000164 ...
$ bw       : num 2.48
$ n        : int 32
$ call     : language density.default(x = mtcars$mpg)
$ data.name: chr "mtcars$mpg"
$ has.na   : logi FALSE
- attr(*, "class")= chr "density"
> plot(d)

plot(d$x,d$y) //what's the difference?
> m = matrix(data=c(1, 0, 2, 3, 3, 3), nrow=2, ncol=3, byrow=TRUE)
> m
     [,1] [,2] [,3]
[1,]    1    0    2
[2,]    3    3    3
> m = matrix(data=c(1, 0, 2, 3, 3, 3), nrow=2, ncol=3)
> m
     [,1] [,2] [,3]
[1,]    1    2    3
[2,]    0    3    3

> layout(m)   //what does it do?
> m==1
      [,1]  [,2]  [,3]
[1,]  TRUE FALSE FALSE
[2,] FALSE FALSE FALSE
> m==2
      [,1]  [,2]  [,3]
[1,] FALSE  TRUE FALSE
[2,] FALSE FALSE FALSE
> m==3
      [,1]  [,2] [,3]
[1,] FALSE FALSE TRUE
[2,] FALSE  TRUE TRUE

> layout.show(n=3) //???  makes us to have 3 plots within one page //makes complicated graphs


m=matrix(data=c(3,3,3,1,0,2)),nrows=2,ncols=3,byrow=TRUE)
m
layout(m)
layout.show(3)


layout(matrix(data=1,nrows=1,ncols=1)) //what does this do??


> curve(expr=x*sin(1/x), from=-pi/6, to=pi/6, n=200)
> curve(expr=x*1,add=TRUE,col="red")
> curve(expr=x*-1,add=TRUE,col="green")
> legend("topright", legend=c("x*sin(1/x)", "x"), col=c("black", "red"), lty=c(1, 1))

> legend("topright", legend=c("x*sin(1/x)", "x"), col=c("black", "red"), lty=c(1, 1)) //what does it do?
//lty??

> curve(expr=x*-1,add=TRUE,col="green",lty=2)
> curve(expr=x*1,col="green",lty=1)
> curve(expr=x*1,col="green",lty=2)
> curve(expr=x*1,col="green",lty=3)
//lty changes the way lines are shown


pairs(mtcars) //shows scatter plot for every term
//a great way to look at the relationship between the variables

a = 1:20; b = sqrt(a); plot(a, b, main = expression(b = sqrt(a)))
 make.table(nr,nc)//?????


install.packages("knitr")
require("knitr")

