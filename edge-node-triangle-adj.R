if (FALSE){
library(igraph)
#xlist<-read.table("cit-Patents.txt")
read.graph_xlist<-read.graph("cit-Patents.txt", format="edgelist",directed=TRUE)

#xlist <- graph.data.frame(xlist)
ij <- get.edgelist(read.graph_xlist)
library(Matrix)
m <- sparseMatrix(
  i = rep(seq(nrow(ij)), each=2),
  j = as.vector(t(ij)),
  x = 1
)


cl <- maximal.cliques(read.graph_xlist)
cl <- cl[ sapply(cl, length) > 2 ]
print(cl)
# Function to test if an edge is part of a triangle
triangle <- function(e) {
  any( sapply( cl, function(u) all( e %in% u ) ) )
}
print(triangle)
# Only keep those edges
kl <- ij[ apply(ij, 1, triangle), ]
print(kl)
# Same code as before
m <- sparseMatrix(
  i = rep(seq(nrow(kl)), each=2),
  j = as.vector(t(kl)),
  x = 1
)
print(m)


}


#if (FALSE){
library(igraph)
set.seed(1)
g <- erdos.renyi.game(7, .6)
#print(g)
plot(g)
ij <- get.edgelist(g)
print(ij)
library(Matrix)
m <- sparseMatrix(
  i = rep(seq(nrow(ij)), each=2),
  j = as.vector(t(ij)),
  x = 1
)
print(m)
# Maximal cliques of size at least 3
cl <- maximal.cliques(g)
print(cl)
cl <- cl[ sapply(cl, length) > 2 ]
print(cl)
# Function to test if an edge is part of a triangle
triangle <- function(e) {
  any( sapply( cl, function(u) all( e %in% u ) ) )
}
print(triangle)
# Only keep those edges
kl <- ij[ apply(ij, 1, triangle), ]
print(kl)
# Same code as before
m <- sparseMatrix(
  i = rep(seq(nrow(kl)), each=2),
  j = as.vector(t(kl)),
  x = 1
)
print(m)
out<-cocluster(as.matrix(m),datatype="binary",nbcocluster=c(2,3))
x=cliques(g, min=3, max=3) #finds number of triangles
length(x)
wc_g<-walktrap.community(g)
plot(wc_g,g)
#}