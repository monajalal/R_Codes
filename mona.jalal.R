##Implementing a parallel Gaussian filter
##Mona Jalal--Stat692

require(stats)
##padding the matrix
mat.pad<-function(X,k){ 
  dims<-dim(X)
  n<-dims[1]
  m<-dims[2]
  pad.X <- matrix(0, n + 2 * k, m + 2 * k)
  pad.X[(k + 1):(n + k), (k + 1):(m + k)] <- X
  return(pad.X)
}
## task:: from k+1 to n+k and from k+1 to m+k, move your window around, taking averages.
smooth.mat<-function(mat.pad,k){
    dims=dim(mat.pad)
    n=dims[1]
    m=dims[2]
    smoothed.mat <- matrix(0, nrow=n, ncol=m) 
    for (row in (k+1):(n-k)){
      for(col in (k+1):(m-k)){
        smoothed.mat[row,col]<-mean(mat.pad[(row-k):(row+k),(col-k):(col+k)])   
      }
    }
    return(smoothed.mat)
}
##unpadding the matrix
unpad.mat<-function(smoothed.mat,k){ ##works fine
  dims<-dim(smoothed.mat)
  m<-dims[1]
  n<-dims[2]
  m2k <- m-2*k
  n2k <- n-2*k
  return(smoothed.mat[(k+1):(m2k+k), (k+1):(k+n2k)])
}

##smoothing the image using the Gaussian method
gaussian.filter<-function(mat,k){
  padded<-mat.pad(mat,k)
  smoothed<-smooth.mat(padded,k)
  unpadded<-unpad.mat(smoothed,k)
  return(unpadded)
}

## used the hint in #Piazza for this function
write.png<-function(x,y,z,USERNAME,k){
  arr <- array(data=c(x,y,z), dim=c(nrow(x),ncol(x),3)) # creates an array
  ##creates .png files in the form of USERNAME_k.png
  file.name<-paste(USERNAME,"_",k,".png",sep="")
  return(writePNG(arr, file.name))
}

## doing the Gaussian smoothing in parallel in all three main pigments of the image
png.mat<-function(image,k){
  require(png)  
  vg <- readPNG(image)
  ## I don't really needed the commented parts for the program purpose
  ## however they are really helpful in understanding the PNG
  #dim(vg)
  #red.vg <- vg[, , 1]
  #green.vg <- vg[, , 2]
  #blue.vg <- vg[, , 3]
  #layout(matrix(1:3, ncol = 3))
  #image(t(red.vg[nrow(red.vg):1, ]), col = gray((1:12)/13), main = "Red channel")
  #image(t(green.vg[nrow(green.vg):1, ]), col = gray((1:12)/13), main = "Green channel")
  #image(t(blue.vg[nrow(blue.vg):1, ]), col = gray((1:12)/13), main = "Blue channel")
  library(doMC) #it works fine on my laptop (with 4 cores)
  library(foreach) ##Is it really necessary? Because it works even without including it I guess
  registerDoMC(3) ## registering 3 workers (aka. 3 cores)
  ncores<-getDoParWorkers() ## getting number of workers
  system.time(image<-foreach(i=1:ncores) %dopar% gaussian.filter(vg[,,i],k)) ## here I should call the function
  USERNAME="Jalal"
  write.png(image[[1]],image[[2]],image[[3]],USERNAME,k)
}

##testing the program--creates the images in the current directory
png.mat("Van_Gogh_Wheatfield_with_Crows.png",1)
png.mat("Van_Gogh_Wheatfield_with_Crows.png",3)
png.mat("Van_Gogh_Wheatfield_with_Crows.png",5)


