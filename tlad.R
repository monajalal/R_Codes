sum.abs.dev<-function(beta,a=land,b=farm)
{
  total<-0
  n<-length(b)
  for (i in 1:n)
  {
    total <- total + (b[i]-beta[1]-beta[2]*a[i])
  }
  return(total)
}
lad <- function(y = "farm", x = "land", data="http://pages.stat.wisc.edu/~gvludwig/327-5/FarmLandArea.csv")
{
  
  dat <- read.csv(data)
  dat.x <- dat[[x]]
  dat.y <- dat[[y]]
  fit<-lm(dat.y~dat.x)
  beta.out=optim(fit$coefficients,sum.abs.dev)$par
  
  return(beta.out)
}