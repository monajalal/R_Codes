require(datasets)
data(nhtemp)
str(nhtemp)

y.hat<-function(beta,y=nhtemp){
  y.hat=numeric(length(y))
  y.hat[1]=y[1]
  y.hat[2]=y[2]
  for (i in 3:length(y))
  {
    y.hat[i]=beta*y[i-1]+(1-beta)*y.hat[i-1]
  }
  return(y.hat)
  
}
mona.function <- function(beta, y=nhtemp){
  n<-length(y)
  y.hat.value<-y.hat(beta,nhtemp)
  sq.sum=0
  for (i in 2:n)
  {
    sq.sum = sq.sum + (y[i]-y.hat.value[i])^2
    
  }
  return(sq.sum/n)
}

opt.result=optimize(mona.function, c(0,1), maximum=FALSE)
(y.hat.value=y.hat(opt.result$minimum,nhtemp))
plot(nhtemp)
lines(ts(y.hat.value, start = start(nhtemp)[1], end = end(nhtemp)[1]), col = "red")

beta=0.1
y.hat.value<-y.hat(beta,nhtemp)
lines(ts(y.hat.value, start = start(nhtemp)[1], end = end(nhtemp)[1]), col = "green")

beta=0.9
y.hat.value<-y.hat(beta,nhtemp)
lines(ts(y.hat.value, start = start(nhtemp)[1], end = end(nhtemp)[1]), col = "blue")
