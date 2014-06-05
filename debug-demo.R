rm(list = ls())

foo <- function() {
  x <- 1
  y <- 2
  z <- 3
}

bar <- function(){
  foo()
}

bar()