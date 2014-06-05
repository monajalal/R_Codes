require(manipulate)
bank <- scan("http://pages.stat.wisc.edu/~gvludwig/327-5/bank.txt")
plot.bank <- function(A, L){
  plot(density(bank), main = "Phone call durations", 
       ylim = c(0,0.005))
  dens <- function(x){
    return(dgamma(x, rate=L, shape=A))
  }
  curve(dens, min(bank), max(bank), 
        add=TRUE, col="Red")
}
manipulate(plot.bank(A, L), 
           A = slider(0.05, 2, step=.05), 
           L = slider(0.00001, 0.05))