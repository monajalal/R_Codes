MMSE <- function(b0, b1, y=farmland$farm, x=farmland$land){
  # Calls rho.prime() here with argument y-b0-b1*x
  
  
  #Why should we call rho.prime? in the html page you have used rho!?
  n = length(y)
  total = 0
  for (i in seq(1,n)) {
    #total = total + rho(t,k)*(y[i]-b0-b1*x[i])
    total = total + rho.prime(y-b0-b1*x,k)*(y[i]-b0-b1*x[i])
  }
  return(total/n)
}
