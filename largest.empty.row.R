largest.empty.row = function(board, col, debug=FALSE) {
  if (debug) {
    cat(sep="", "largest.empty.row(board=\n")
    print(board)
    cat(sep="", ", col=", col, ")\n")
  }
  col=board[,col]
  index_vector=which(col=="E")
  
  # ...
  return(max(index_vector)) # correct this return() statement
}

x = matrix(data=c(
  "E","E","E","E","E","E","E",
  "E","E","E","X","O","E","E",
  "E","E","X","O","X","E","E",
  "E","X","O","X","O","E","E",
  "X","O","O","O","X","E","E",
  "X","O","X","X","O","E","E"
), nrow=6, ncol=7, byrow=TRUE)

m=largest.empty.row(board=x, col=1)
print(m)
stopifnot(4 == largest.empty.row(board=x, col=1, debug=TRUE)) 
stopifnot(3 == largest.empty.row(board=x, col=2, debug=TRUE))