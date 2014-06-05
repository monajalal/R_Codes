haarum <- function(memberships, x, threshold=0) {
  
  #---------------------------------------------------------------------------
  # Hierarchic Haar wavelet transform, threshold filtering, inverse transform
  # FM, 2003/10/6
  #
  # Inputs: 
  # a: cluster membership array, produced by cutree
  # x: input data array
  # threshold: filtering threshold
  #
  # Example of running on Fisher's iris data:
  # data(iris)
  # x <- iris[,1:4]
  # xres <- haarum(cutree(hclust(dist(x)),1:150), as.matrix(x), 0.1)
  # Prints: number of zero coefficients found, and mean square error 
  # between input data and reconstructed data. 
  #
  # An object of class haarum is a list with components:
  # wt: the hierarchic or ultrametric Haar wavelet transform
  # xout: the reconstructed data following filtering 
  #---------------------------------------------------------------------------
  
  # Elementary input checking
  n <- nrow(memberships)
  if (n != nrow(x)) 
    cat("Problem: expecting nrow(memberships) = nrow(x).\n")
  if (n != ncol(memberships)) 
    cat("Problem: expecting nrow = ncol for memberships.\n")
  m <- ncol(x)
  
  # Reformat memberships so that they have the following format.
  # In the following, column 8 contains singletons
  # Column 7 indicates cluster 9 agglomerating 1 and 8
  # Column 6 entails agglomeration of 1, 8 and 7 into new cluster 10
  # And so on, until column 1 shows one cluster with all observations
  # This data was obtained from median hierarchic clustering of 
  # the first 8 observations in Fisher's iris data (originally 4-dimensional)
  # 15 14 10 10 10 10  9  1 
  # 15 14 13  3  3  3  3  3
  # 15 14 13 12 11  4  4  4
  # 15 14 13 12 11  6  6  6
  # 15 14 10 10 10 10  9  8
  # 15  2  2  2  2  2  2  2
  # 15 14 13 12  5  5  5  5
  # 15 14 10 10 10 10  7  7
  
  a <- memberships
  for (j in (n-1):1) {
    changedvals <- memberships[,j] != memberships[,j+1]
    clusval <- min(memberships[changedvals,j])
    for (i in 1:n) {
      if (memberships[i,j] == clusval) a[i,j] <- 2*n-j
      if (memberships[i,j] != clusval) a[i,j] <- a[i,j+1]
    }
  }
  
  # First, forward transform
  
  #ident <- matrix(0, nrow=n, ncol=n)
  #diag(ident) <- 1                        # ident, identity matrix, = input 
  ident <- x
  xout <- x                               # Reconstructed output data array
  #smooth <- matrix(0, nrow=n, ncol=n-1)   # Sequence of signal smooths
  #detail <- matrix(0, nrow=n, ncol=n-1)   # Sequence of signal detail 
  smooth <- matrix(0, nrow=m, ncol=n-1)   # Sequence of signal smooths
  detail <- matrix(0, nrow=m, ncol=n-1)   # Sequence of signal detail 
  
  # In the succession of signal smooth and detail vectors, we have this scheme: 
  # Col n-1 contains in particular details of new cluster, n+1        = q1
  # Col n-2                                                n+2        = q2
  # Col n-3                                                n+3        = q3
  # ... 
  # Col n-(n-2)                                            n+(n-2)    = q(n-2)
  # Col n-(n-1)                                            n+(n-1)    = q(n-1)
  
  # We must also cater for left and right branching in the dendrogram
  # We call left branch: aine (elder),
  # and right branch: benjamin (younger)
  
  index <- 1:n                # Set of observations, 1 through n
  for (j in 1:(n-1)) {        # Set of hierarchy levels, 1 through n-1 
    
    # Get cluster members, proceeding through q1, q2, ... q(n-1)
    mmbrs <- index[a[,n-j]==n+j]
    
    # Find subclusters: there are only two; and they will be found 
    # in the column that follows.  We do this to characterize aine, benjamin
    nextcolmmbrs <- a[mmbrs,n-j+1]
    
    temp <- sort(nextcolmmbrs)   # Always, by convention, aine < benjamin
    aine <- temp[1]
    benjamin <- temp[length(nextcolmmbrs)]    
    
    if (aine <= n) sub1 <- ident[aine,]            # Aine is a singleton
    if (benjamin <= n) sub2 <- ident[benjamin,]    # Benjamin in a singleton
    
    if (aine > n) sub1 <- smooth[,2*n-aine]
    if (benjamin >n) sub2 <- smooth[,2*n-benjamin]
    
    smooth[,n-j] <- 0.5*(sub1 + sub2)              # Smooth s(qj)
    detail[,n-j] <- 0.5*(sub1 - sub2)              # Detail d(qj)
    
    
  }   # End of j-loop (hierarchy levels)
  
  # Filter
  cat("Filtering threshold used (= 0 implies no filtering) ",threshold,"\n")
  EPS = 1.e-10
  waszero = 0
  nowzero = 0 
  for (i in 1:m) {
    for (j in 1:(n-1)) {
      if (abs(detail[i,j]) < EPS) waszero <- waszero + 1 
      if (abs(detail[i,j]) <= threshold) detail[i,j] <- 0
      if (abs(detail[i,j]) < EPS) nowzero <- nowzero + 1 
    }
  }
  cat("Formerly and now, no. zero elts. = ",waszero," or ",
      100*waszero/(m*(n-1)),"% ; ", nowzero, " or ", 100*nowzero/(m*(n-1)),"%\n")
  
  
  # Second, inverse ultrametric Haar wavelet transform
  # We need: cluster member array, a; and transform: smooth[,1]; and detail.
  
  for (obs in 1:n) {                     # For all observations 
    wasclusnum     <- obs
    reconstruction <- rep(0,m)         # m<-m Reconstructed vectors 
    
    for (j in 1:(n-1)) {               # For all levels in the hierarchy    
      
      # Get cluster members, proceeding through q1, q2, ... q(n-1)
      mmbrs <- index[a[,n-j]==n+j]
      
      # Characterize as aine or benjamin
      # Find subclusters: there are only two; and they will be found 
      # in following column.
      nextcolmmbrs <- a[mmbrs,n-j+1]
      temp <- sort(nextcolmmbrs)     # By design, aine < benjamin always
      aine <- temp[1]                # So aine is the smallest value
      benjamin <- temp[length(nextcolmmbrs)]   # Benjamin is largest value
      # cat("Information: cluster ", j+n, " with members ", a[mmbrs,n],
      #     " and aine and benjamin = ", aine, " ", benjamin, "\n")
      
      clusnum <- 0
      for (k in 1:length(mmbrs)) {   # Check through all cluster members
        if (obs == a[mmbrs[k],n]) {    # Is obs a member here?
          # Have found that obs is in cluster at level j (1 <= j <= n-1)
          # There can be only one occurrence of this, in set mmbrs
          clusnum <- j + n        # Change cluster numbering convention
          # Aine branch - In the following add or subtract detail signal
          if (wasclusnum == aine) 
            reconstruction <- reconstruction + detail[,2*n-clusnum]
          # Benjamin branch
          if (wasclusnum == benjamin) 
            reconstruction <- reconstruction - detail[,2*n-clusnum]
          wasclusnum <- clusnum
        }
      }   # End of k-loop, checking through cluster members
      
    }   # End of j-loop (levels of tree)
    
    reconstruction <- reconstruction + smooth[,1] # Add continuum (DC component)
    # cat("Reconstruction of observation ",a[obs,n]," is: ",reconstruction,"\n")
    xout[obs,] <- reconstruction
    
  }   # End of obs-loop (set of all observations)
  
  cat("Mean square error between input data and reconstructed data = ",
      sum((xout-x)^2)/(n*m),"\n")
  
  ret <- list ( wt = cbind(smooth[,1],detail),
                # Return ultrametric wavelet transform, dims n x n
                xout = xout)
  # and approximate (if filtering threshold > 0) reconstruction of 
  # input data.
  
}   # End of function haarum (ultrametric Haar wavelet transform + inverse) 
