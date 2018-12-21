# Take a sample from original data with gaussian kernel

KDE.r <- function(n, x, bw, kernel = 'gaussian'){
  if (missing(bw)){
    bw = density(x)$bw
  }
  rkernel = rnorm(n, 0, sd = bw)
  sample(x, n, replace=TRUE) + rkernel
}

#  the new generated numbers
KDE.density <- function(KDE_output, x){
  plot(density(KDE_output), col="green", main = 'red:raw data')
  lines(density(x), col="red")
}

# Example:
n = 100
x <- runif(100,3,5)
bw = density(x)$bw
y = KDE.r(n = 1000, x, bw)


KDE.density(KDE_output = y, x = x)

# ---------------------- Use 'ks' package to do the same thing
library(ks)
x2 <- x
y2 <- rkde(fhat=kde(x=x2, h=hpi(x2)), n=1000)
plot(density(y2), main = 'red: raw data')
lines(density(x), col="red")


