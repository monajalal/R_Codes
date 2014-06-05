## Jon-Michael Deldin
## Pattern Recognition
## Edge Detection
## Spring 2013
##
##
## USAGE
## ------
##  Rscript edges.R
##
## DEPENDENCIES
## ------------
## You'll need to install the libtiff-dev package on Linux and then install
## the biOps package in R:
##
##  install.packages('biOps')
##

## clear existing vars
rm(list = ls(all=TRUE))

source('utilities.R')
source('functions.R')

library(biOps)

mkdirp('data')
mkdirp('fig')

############
## VIOLET ##
############
violet <- readJpeg(system.file("samples", "violet.jpg", package="biOps"))

plotSobel(violet, 'violet-sobel-white.jpg')
plotCanny(violet, 'violet-canny.jpg')

####################
## ORIGINAL LENNA ##
####################

maybeDownload('data/lenna.jpg', 'http://www.cs.umt.edu/~dougr/PattRecFiles/projects/edgeDetection/Lenna.jpg')
rgb <- readJpeg('data/lenna.jpg')
greyImg <- imgRGB2Grey(rgb)

plotImage(greyImg, 'lenna.jpg')

####################
## LAPLACIAN MASK ##
####################
lpMask <- getLaplacianMask(5)
lpImg <- applyMask(lpMask, greyImg)
plotImage(lpImg, 'lenna-laplacian.jpg')

###################
## GAUSSIAN BLUR ##
###################

## quantiles for a 5x5 matrix
qVals <- distanceFromCenter(2)

## gaussian weights with a stddev of 1.4
blurMask <- round(15 * gaussWeights(qVals, 1.4))

## normalize the weights by the sum of the values
blurMask <- blurMask/sum(blurMask)

smoothedImg <- applyMask(blurMask, greyImg)
plotImage(smoothedImg, 'lenna-gaussian.jpg')

smoothedLpImg <- applyMask(lpMask, smoothedImg)
plotImage(smoothedLpImg, 'lenna-gaussian-laplacian.jpg')

higherThresholdImg <- applyMask(lpMask, smoothedImg, lower=100)
plotImage(higherThresholdImg, 'lenna-gaussian-100.jpg')

###########
## CANNY ##
###########
plotImage(imgCanny(greyImg, 1.4), 'lenna-canny.jpg')