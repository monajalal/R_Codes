rm(list=ls()) # clear all defined objects
#setting up the connect_four board
four.in.a.row = function(player, v, debug=FALSE) {
  if (debug) {
    cat(sep="", "four.in.a.row(player=", player, ", v=", v, ")\n")
  }
  with(rle(v), any(lengths== 4 & values == player))
}
won = function(player, board, r, c, debug=FALSE) {
  if (debug) {
    cat(sep="", "won(player=", player, ", board=\n")
    print(board)
    cat(sep="", ", r=", r, ", c=", c, ")\n")
  }
  row_w=board[r,]
  cat("row is = ", row_w, "\n")
  col_w=board[,c]
  cat("col is = ", col_w, "\n")
  reverse_diag_w=board[row(board) + col(board) == r + c]
  cat("reverse diag is = ", reverse_diag_w, "\n")
  diag_w=x[row(board) - col(board) == r - c]
  cat("diag is = ", diag_w, "\n")
  return(four.in.a.row(player,row_w,debug=debug)  ||
           four.in.a.row(player,col_w,debug=debug)  ||
           four.in.a.row(player,diag_w,debug=debug) ||
           four.in.a.row(player,reverse_diag_w,debug=debug))
}
largest.empty.row = function(board, col, debug=FALSE) {
  if (debug) {
    cat(sep="", "largest.empty.row(board=\n")
    print(board)
    cat(sep="", ", col=", col, ")\n")
  }
  col=board[,col]
  index_vector=which(col=="E")
  if (length(index_vector)==0)
    return(NULL)
  return(max(index_vector))
}

par(pty="s") # square plot type
x = rep(1:7, each = 6)
y = rep(1:6, times = 7)

symbols(x, y, squares=rep(1, times=42),
        inches=FALSE, # match squares to axes
        xlim=c(0,8),
        ylim=c(7,0)) # flip y axis to match matrix format
board = matrix(rep("E", times=42), nrow=6, ncol=7)
player = "X"
for (i in 1:42) { # loop through 42 turns
  if (player == "X") {
  repeat { # get user input on empty square
    index = identify(x, y, n=1,plot=FALSE) # , plot=FALSE
    #row = y[index]
    col = x[index]
    row=largest.empty.row(board, col)
    if (is.null(row)){
      next
    }else if (is.null(col)){
      next
    } else if (board[row, col] == "E") {
      break
    }else {next}
  }
  }
  else { # computer chooses random empty square
    open.indices = which(c(board)=="E")
    index = sample(x=open.indices, size=1)
    #row = y[index]
    col = x[index]
    row=largest.empty.row(board, col)
  }
  board[row, col] = player
  #text(x=x[index], y=y[index], labels=player)
  text(x=col, y=row, labels=player)
  print(board)
  if (won(player, board, row, col)) {
    text(x=2, y=.2, labels=paste(player, "won!"), col="red")
    break
  }
  player = ifelse(player == "X", "O", "X")
}