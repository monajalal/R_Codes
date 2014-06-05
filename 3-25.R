d<-runif(1)
if(d>0.5){cat("\n\n mad benjamins \n\n")}
x=3
if(x<0){
  x=-1*x
} else
{cat("mona")}

x = 5 # example
if ((x %% 2) == 0) {
  parity = "even"
  cat("\n")
  cat(x)
} else {
  parity = "odd"
  cat("\n")
  cat(x)
}

square.a = function(a=1, b=2) {
  cat(sep="", " square.a(a=", a, ", b=", b, ")\n")
  b = 100
  c = a*a
  return(c)
}


square.a(a=3, b=4) # two identical calls
square.a(b=4, 3)
square.a(4,9)
a = 5; b = 6; c = 7
square.a(b)
cat(sep="", "a=", a, ", b=", b, ", c=", c, "\n")


