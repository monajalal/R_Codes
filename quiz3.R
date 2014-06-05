v<- c(25,29,20,0,6,29,15,17,22,2,9,6,17,26,24,5,3,26,6,28,25,7,18,21,6,14,20,8,15,3)


sum(v[v %in% v2])
[1] 172
sum(v2[match(v, v2, nomatch = 0)])
[1] 172

sorted_v=sort(v)
sum(sorted_v[1:3])
v2<-c(0, 5, 25, 1, 2, 18, 17, 28, 18, 29, 16, 1, 8, 6, 1)
v21<-intersect(v2,v)
sum(v21)


g = list(fruit="apple", dairy=c("milk", "eggs"), lotto = c(4,2,1,7))
g[[3]][[4]]<-6
g$dairy<-c(g[[2]],"butter")
g$snacks<-"popcorn"

g[[1]]<-NULL #removing fruit itemv=


names = readLines('hamlet.txt')

idxs = grep("memory", names, ignore.case = TRUE)
names[idxs]
hamlet_char=scan("Hamlet.txt")

