#install.packages("biglm")
#install.packages("bigmemory")
#library(clusterApply)
library(biglm)
library(bigmemory)


big.mat=read.big.matrix("cp2006.csv",header=T)
desc<-describe(big.mat)
x<-attach.big.matrix(desc)
summary(x[,])
jobs <- lapply(1:10, function(x) mcparallel(colMeans(is.na(big.mat[,]))*100, name = big.mat[,]))
res  <- mccollect(jobs)


cp.2006<-read.csv(file="cp2006.csv",head=TRUE)

#countNAs <- function(x) { 
#  sum(is.na(x)) 
#} 
#total=0
#for (i in col(cp.2006)) {
#  total=countNAs(i)+total
#}
#print(total)
#count<-apply(cp.2006, 1, function(x) sum(is.na(x)))

#
#print(NApercentage)
cols.NA<-apply(cp.2006, 2, function(col)sum(is.na(col))/length(col))*100
total.NA<-sum(is.na(cp.2006))
dims<-dim(cp.2006)
num<-dims[1]*dims[2]
total.NA.percentage<-(total.NA/num) * 100
col.NA.mean<-colMeans(is.na(cp.2006))*100
model<-biglm(SecurityDelay~DepDelay+AirTime,data=cp.2006)
bigcp<-as.big.matrix(cp.2006)
  