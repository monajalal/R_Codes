farmland <- read.csv("http://pages.stat.wisc.edu/~gvludwig/327-5/FarmLandArea.csv")
str(farmland)
plot(farm~land,data=farmland)
fit=lm(farm~land,data=farmland)
abline(fit) #lease square regression line
abline(rlm(farm~land,data=farmland),col="red")
gr.descent <- function(der_f, x0, alpha=0.0001, eps=0.001, max.it = 50, verbose = FALSE){
  X1 <- x0
  cond <- TRUE
  iteration <- 0
  if(verbose) cat("X0 =",X1,"\n")
  while(cond){
    iteration <- iteration + 1
    X0 <- X1
    X1 <- X0 - alpha * der_f(X0)
    cond <- sum((X1 - X0)^2) > eps & iteration < max.it
    if(verbose) cat(paste(sep="","X",iteration," ="), X1, "\n")
  }
  print("mona")
  print(X1)
  return(X1)
}



rho<-function(t,k) ifelse(abs(t)<=k,t^2,(2*k*abs(t))-k^2)
k=19000
rho.prime<-function(t,k) ifelse (abs(t)<=k,2*t,(2*k*sign(t)))

dMMSE <- function(b,k=19000, y=farmland$farm, x=farmland$land){
  
  n = length(y)
  a=0
  d=0
  for (i in 1:n) {

    a = a + rho.prime(y[i]-b[1]-b[2]*x[i],k)
    d = d + x[i]*rho.prime(y[i]-b[1]-b[2]*x[i],k)
  }
  a <- (-a/n)
  d <- (-d/n)
  return(c(a,d))
}

grd=gr.descent(dMMSE, c(3500,0.33),alpha=0.0001, verbose=TRUE)

gr.descent2 <- function(der_f,x0, alpha=0.1, eps=0.001, max.it = 50, verbose = FALSE){
  X1 <- x0
  cond <- TRUE
  iteration <- 0
  if(verbose) cat("X0 =",X1,"\n")
  while(cond){
    iteration <- iteration + 1
    X0 <- X1
    X1 <- X0 - alpha * der_f(X0)
    alpha <- alpha/2
    cond <- sum((X1 - X0)^2) > eps & iteration < max.it
    if(verbose) cat(paste(sep="","X",iteration," ="), X1, "\n")
  }
  print("mona2")
  print(X1)
  return(X1)
}


grd2=gr.descent2(dMMSE, c(3500,0.33),alpha=0.1, verbose=TRUE)
#(beta0=grd2[1])
#(beta1=grd2[2])
plot(farm~land,data=farmland)
curve(rho(x,k=19000),xlim=c(-10,10),,col="blue", add="TRUE")

