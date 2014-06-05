##
## Handy utilities to share across assignments.
##

library(MASS)

## create the directory to download the data (like `mkdir -p` on *nix)
mkdirp <- function(directory) {
  dir.create(directory, recursive=TRUE, showWarnings=FALSE)
}

## download a file if it doesn't exist
maybeDownload <- function(filename, url) {
  if (!file.exists(filename)) {
    download.file(url, filename)
  }
}

## drop a specific element from a list
dropElement <- function(elt, lst) {
  lst[-which(lst == elt)]
}

## runs lda() and returns an LDA object
getLda <- function(data, train, classes, tolerance) {
  return(lda(data[train,], classes[train], tol=tolerance))
}

## returns both projections
projectLda <- function(data, ldaObj) {
  mat <- as.matrix(data)
  first <- mat %*% ldaObj$scaling[,1]
  second <- mat %*% ldaObj$scaling[,2]
  return(list(first=first, second=second))
}

## Prints a formatted string
printf <- function(str, ...) {
  cat(sprintf(str, ...), "\n")
}

## Returns the Euclidean distance between two columns
eucDist <- function(x1, x2) {
  sqrt(sum((x1 - x2) ^ 2))
}

## sets the initial width, height, and margins
initPlot <- function(bottom=2.5, left=2.5, top=0.5, right=0.1) {
  X11(width=4, height=4)
  par(mar=c(bottom, left, top, right), mgp=c(1.5, .5, 0))
}

## save the plot to PDF
savePlot <- function(filename, type=pdf) {
  dev.copy(type, paste('fig/', filename, sep=''))
  dev.off()
}

## set sane defaults for plotting an image
initImagePlot <- function() {
  initPlot()
  par(mar=c(0,0,0,0))
}

## save a plot as a JPEG
saveJpeg <- function(filename) {
  savePlot(filename, jpeg)
}

## plot an imagedata() instance
plotImage <- function(image, filename) {
  initImagePlot()
  plot(image)
  saveJpeg(filename)
}