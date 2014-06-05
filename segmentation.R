############
# Import the image 
############
library(jpeg)
library(spatstat)

# http://www.eecs.berkeley.edu/Research/Projects/CS/vision/bsds/BSDS300/html/images/plain/normal/gray/86016.jpg
rawimg=readJPEG("moose.jpeg")
#rawblur=gblur(rawimg, sigma=4)

rawimg=t(rawimg)
#rawimg=t(blur(as.im(rawimg), sigma=6))
rawimg=rawimg[,ncol(rawimg):1]
image(rawimg,col = grey((0:12)/12))
#image(rawimg)


############
# Smooth the image
############
library(fields)
smoothimg=image.smooth(rawimg,theta=2)
image(smoothimg,col = grey((0:12)/12))

############
# Reduce Size of Image
############

olddim=dim(rawimg)
newdim=c(round(olddim/10))
prod(newdim)>2^31
img=matrix(NA,newdim[1],newdim[2])
for (r in 1:newdim[1]) {
  centerx=(r-1)/newdim[1]*olddim[1]+1
  lowerx=max(1,round(centerx-olddim[1]/newdim[1]/2,0))
  upperx=min(olddim[1],round(centerx+olddim[1]/newdim[1]/2,0))
  for (c in 1:newdim[2]) {
    centery=(c-1)/newdim[2]*olddim[2]+1
    lowery=max(1,round(centery-olddim[2]/newdim[2]/2,0))
    uppery=min(olddim[2],round(centery+olddim[2]/newdim[2]/2,0))
    img[r,c]=mean(smoothimg$z[lowerx:upperx,lowery:uppery])
  }
}
image(img,col = grey((0:12)/12))


############
# Convert matrix to vector
############

imgvec=matrix(NA,prod(dim(img)),3)
counter=1
for (r in 1:nrow(img)) {
  for (c in 1:ncol(img)) {
    imgvec[counter,1]=r
    imgvec[counter,2]=c
    imgvec[counter,3]=img[r,c]
    
    counter=counter+1
  }
}


############
# Similarity Matrix
############

pixdiff=2 ####come back
sigma2=.01 #var(imgvec[,3])
#sigma2=0.1
simmatrix=matrix(0,nrow(imgvec),nrow(imgvec))
for(r in 1:nrow(imgvec)) {
  cat(r,"out of",nrow(imgvec),"\n")
  simmatrix[r,]=ifelse(abs(imgvec[r,1]-imgvec[,1])<=pixdiff & abs(imgvec[r,2]-imgvec[,2])<=pixdiff,exp(-(imgvec[r,3]-imgvec[,3])^2/sigma2),0)
}

############
# Weighted and Unweighted Laplacian
############

D=diag(rowSums(simmatrix))
Dinv=diag(1/rowSums(simmatrix))
L=diag(rep(1,nrow(simmatrix)))-Dinv %*% simmatrix
U=D-simmatrix

############
# Eigen and k-means
############

evL=eigen(L,symmetric=TRUE)
evU=eigen(U,symmetric=TRUE)

kmL=kmeans(evL$vectors[,(ncol(simmatrix)-1):(ncol(simmatrix)-0)],centers=2,nstart=5)
segmatL=matrix(kmL$cluster-1,newdim[1],newdim[2],byrow=T)
if(max(segmatL) & sum(segmatL==1)<sum(segmatL==0)) {segmatL=abs(segmatL-1)}

kmU=kmeans(evU$vectors[,(ncol(simmatrix)-1):(ncol(simmatrix)-0)],centers=2,nstart=5)
segmatU=matrix(kmU$cluster-1,newdim[1],newdim[2],byrow=T)
if(max(segmatU) &sum(segmatU==1)<sum(segmatU==0)) {segmatU=abs(segmatU-1)}

############
# Plotting the clusters
############

image(segmatL, col=grey((0:15)/15))
image(segmatU, col=grey((0:12)/12))

############
# Overlaying the original and the clusters
############
image(seq(0,1,length.out=olddim[1]),seq(0,1,length.out=olddim[2]),rawimg,col = grey((0:12)/12),xlim=c(-.1,1.1),ylim=c(-.1,1.1),xlab="",ylab="")

segmat=segmatU
linecol=2
linew=3
for(r in 2:newdim[1]) {
  for (c in 2:newdim[2]) {
    if(abs(segmat[r-1,c]-segmat[r,c])>0) {
      xloc=(r-1)/(newdim[1])
      ymin=(c-1)/(newdim[2])
      ymax=(c-0)/(newdim[2])
      segments(xloc,ymin,xloc,ymax,col=linecol,lwd=linew)
    }
    if(abs(segmat[r,c-1]-segmat[r,c])>0) {
      yloc=(c-1)/(newdim[2])
      xmin=(r-1)/(newdim[1])
      xmax=(r-0)/(newdim[1])
      segments(xmin,yloc,xmax,yloc,col=linecol,lwd=linew)
    }
  }
}
