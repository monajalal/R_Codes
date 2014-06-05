how.many<-function(fruit, number){
  string<-paste("How many",fruit,"?",sep=" ")
  fruit_number<-readline(string)
  print("fruit number")
  print(fruit_number)
  print("number")
  print(number)
  while(fruit_number > number){
    print("inside while")
    print("fruit number")
    print(fruit_number)
    print("number")
    print(number)
    print("ERROR: too many for the budget")
    string<-paste("How many",fruit,"?",sep=" ")
    fruit_number<-readline(string)
  }
  return(as.numeric(fruit_number))
}
