#step1 Draw tic-tac-toe board.
#step2 Introduce a playing loop and board variable and need to 
rm(list=ls()) # clear all defined objects
#step4: check for win
#step5: add random computer player
#return whether there aee 3-in-a-row player on board
won <- function(oard,player){
  for (row in 1:3){
    if (sum(board[i,]==player=3)){
     return(TRUE) 
    }
  for (col in 1:3){
      if (sum(board[,i]==player)=3){
        return(TRUE) 
      }
  
  }
  
  
}

par(pty="s") # square plot type

x = rep(1:3, each = 3)
y = rep(1:3, times = 3)
symbols(x, y, squares=rep(1, times=9),
        inches=FALSE,
        xlim=c(0,4),
        ylim=c(4,0)
)
board=matrix(rep("E",times=9),nrow=3,ncol=3)
#we want to switch back and forth between X and O
#don't let a player click on a spot which is already clicked
#matrix("E",nrow=3,ncol=3)
#> y[index]
#[1] 1
#> x[index]
#[1] 1
#player.is.X=TRUE
player="X"
for (i in 1:9){
  if (player=="X")
  {
 # player=ifelse(i %% 2 ==0, "O", "X")
  repeat {
    index=identify(x,y,n=1,plot=FALSE)
    row=y[index]
    col=x[index]
    if(board[row,col]=="E"){
      break
    }
  }
  }else{
    open.indices=which (c(board)=="E")
    index=sample(x=open.indices,size=1)
    row=y[index]
    col=x[index]
  }
 #open.indices=which (c(board)=="E")
 #sample(x=open.indices,size=1)
 #?sample
  #index=identify(x,y,n=1)
  #row=y[index]
  #col=x[index]
  print(row)
  print(col)
  board[row,col]=player
  
  print(board)
  if (won(board,player)){
    text(x=2,y=0.2,labels=paste(player,"won!"),col="red")
    break
  }
  player=ifelse(player=="X", "O", "X")
  text(x[index],y[index],labels=player)
#  player.is.X=!player.is.X
  
#player=ifelse(test,yes,no)
}