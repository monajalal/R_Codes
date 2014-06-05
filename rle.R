four.in.a.row = function(player, v, debug=FALSE) {
  if (debug) {
    cat(sep="", "four.in.a.row(player=", player, ", v=", v, ")\n")
  }
  with(rle(v), any(lengths== 4 & values == player))
}
# Returns TRUE if (matrix) board (of character strings)
# contains at least four in a row of (string) player, who
# just played in position (r, c). (Here "r" means "row" and
# "c" means "column").
#
# Hint: this function should call four.in.a.row() four times.
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

  return(max(index_vector))
}

