png.mat<-function(image,k){
  require(png)  # if necessary, run once: install.packages('png')
  vg <- readPNG(image)
  dim(vg)
  red.vg <- vg[, , 1]
  green.vg <- vg[, , 2]
  blue.vg <- vg[, , 3]
  layout(matrix(1:3, ncol = 3))
  image(t(red.vg[nrow(red.vg):1, ]), col = gray((1:12)/13), main = "Red channel")
  image(t(green.vg[nrow(green.vg):1, ]), col = gray((1:12)/13), main = "Green channel")
  image(t(blue.vg[nrow(blue.vg):1, ]), col = gray((1:12)/13), main = "Blue channel")
  
  return(c(red.vg,green.vg,blue.vg))
}
#png.vg=png.mat("Van_Gogh_Wheatfield_with_Crows.png",1)
mat.pad<-function(X,k){
  dims<-dim(X)
  n<-dims[1]
  m<-dims[2]
  pad.X <- matrix(0, n + 2 * k, m + 2 * k)
  pad.X[(k + 1):(n + k), (k + 1):(m + k)] <- X
  return(pad.X)
}

