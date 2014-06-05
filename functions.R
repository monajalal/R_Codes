## Jon-Michael Deldin
## Pattern Recognition
## Edge Detection: Functions
## Spring 2013
##

library(biOps)
source('utilities.R')

## plot sobel detection with a white background
plotSobel <- function(image, filename) {
  plotImage(imgNegative(imgSobel(image)), filename)
}

## plot canny detection with a stddev of 1.4
plotCanny <- function(image, filename, stddev=1.4) {
  plotImage(imgCanny(image, stddev, 1, 255), filename)
}

## Returns the midpoint
getCenter <- function(size) {
  ceiling(size/2)
}

## Returns a Laplacian convolution mask of a given size.
getLaplacianMask <- function(size) {
  mask <- matrix(-1, size, size)
  center <- getCenter(size)
  mask[center, center] <- size * size - 1
  return(mask)
}

## first two rows, last two rows, and anything that will put us out of bounds
isBoundary <- function(coord, maxBorder, interval) {
  bad <- coord == 1 || coord == 2 || coord == maxBorder - 2 || coord == maxBorder
  out <- (coord + interval - 1) > maxBorder
  
  ## printf("...coord = %d, maxb = %d, interval=%d", coord, maxBorder, interval)
  
  return(bad || out)
}

## Normalize the color values
normalize <- function(x, upper, lower) {
  if (x > upper) { 255 }
  else if (x < lower) { 0 }
  else { x }
}

## Apply a convolution mask to an image
applyMask <- function(mask, imageData, upper=255, lower=0) {
  print("Applying mask...")
  interval <- ncol(mask)
  
  numRows <- nrow(imageData)
  numCols <- ncol(imageData)
  copy <- imageData
  for (row in 1:numRows) {
    for (col in 1:numCols) {
      ## if we're looking at one of the ignoreable zones, just zero out the pixel
      if (isBoundary(row, numRows, interval) || isBoundary(col, numCols, interval)) {
        copy[row, col] <- 0
      } else {
        ## otherwise, do convolution
        slice <- imageData[row:(row+interval-1), col:(col+interval-1)]
        ## highlight low-intensity edges near white
        pixel <- upper - normalize(sum(mask * slice), upper, lower)
        copy[row, col] <- pixel
      }
    }
  }
  return(imagedata(copy))
}

## Distance between two points
distance <- function(x1, y1, x2, y2) {
  sqrt((x2 - x1)^2 + (y2 - y1)^2)
}

## return a square distance matrix from a center point
distanceFromCenter <- function(distFromCenter) {
  n <- distFromCenter * 2 + 1
  mat <- matrix(ncol=n, nrow=n)
  center <- getCenter(n)
  
  for (i in 1:n) {
    for (j in 1:n) {
      mat[i, j] <- distance(i, j, center, center)
    }
  }
  return(mat)
}

## Returns the approximate Gaussian weights for a given radius and stddev
gaussWeights <- function(r, sigma) {
  exp(-(r**2) / (2 * sigma**2))
}

## z <- applyMask(GWs/sum(GWs), greyImg)
## zi <- z
