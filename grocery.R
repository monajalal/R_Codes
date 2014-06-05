how.many<-function(fruit, number){
  string<-paste("How many",fruit,"?",sep=" ")
  fruit.number<-as.numeric(readline(string))
  while(fruit.number > number){
    print("ERROR: too many for the budget")
    string<-paste("How many",fruit,"?",sep=" ")
    fruit.number<-as.numeric(readline(string))
  }
  return(as.numeric(fruit.number))
}

grocery.list <- function(file,budget) {
  item.count<-0
  updated.price=budget
  final.price<-0
  purchased.items <- data.frame(count= numeric(0), item= character(0), price = numeric(0),quantity=numeric(0))
  outf<-read.csv(file,header=FALSE)
  colnames(outf)<-c("item","price")
  mat = as.matrix(outf)
  colnames(mat) <- NULL
 
  for (i in 1:dim[1])
  {
     
     if (updated.price-as.numeric(mat[i,2])>0)
     {
       number=how.many(mat[i,1],updated.price/as.numeric(mat[i,2]))
       updated.price=updated.price- (number* (as.numeric(mat[i,2])) )
       if (number>0){
         item.count=item.count+1
         new_row=data.frame(item=mat[i,1],price=as.numeric(mat[i,2]),quantity=number)
         purchased.items<-rbind(purchased.items,new_row)
       }
       
     }
     
    }
  colnames(purchased.items)=c("item","price", "quantity" )
  print(purchased.items)

  print(purchased.items)
  p.mat=as.matrix(purchased.items)
  colnames(p.mat)<-NULL
  rownames(p.mat)<-NULL
  print(p.mat)
  p.dim<-dim(p.mat)
  for (i in 1:p.dim[1])
      final.price<- final.price+ (as.numeric(p.mat[i,2]))*(as.numeric(p.mat[i,3]))
  
  return(final.price)
  
}

n=grocery.list("groceries.csv",20)
print(n)
#print(n)


