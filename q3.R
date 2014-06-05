require(datasets)
data(nhtemp)
str(nhtemp)
mona.function <- function(beta, y=nhtemp){
  y.hat=numeric(length(y))
  y.hat[1]=y[1]
  y.hat[2]=y[2]
  for (i in 3:length(y))
  {
    y.hat[i]=beta*y[i-1]+(1-beta)*y.hat[i-1]
  }
  sq.sum=0
  for (i in 2:length(y))
  {
    sq.sum = sq.sum + (y[i]-y.hat[i])^2
    
  }
  plot(nhtemp,xlim=c(1912,1971))
  #lines(y.hat,col="red")
  lines(ts(y.hat, start = start(nhtemp)[1], end = end(nhtemp)[1]), col = "red")
  beta=0.1
  y.hat=numeric(length(y))
  y.hat[1]=y[1]
  y.hat[2]=y[2]
  for (i in 3:length(y))
  {
    y.hat[i]=beta*y[i-1]+(1-beta)*y.hat[i-1]
  }
  lines(ts(y.hat, start = start(nhtemp)[1], end = end(nhtemp)[1]), col = "green")
  
  beta=0.9
  y.hat=numeric(length(y))
  y.hat[1]=y[1]
  y.hat[2]=y[2]
  for (i in 3:length(y))
  {
    y.hat[i]=beta*y[i-1]+(1-beta)*y.hat[i-1]
  }
  lines(ts(y.hat, start = start(nhtemp)[1], end = end(nhtemp)[1]), col = "blue")
  
  
  return(sq.sum/length(y))
}
opt.result=optimize(mona.function, c(0,1), maximum=FALSE)
