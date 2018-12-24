# KDE to seperately generate random numbers for multiple datasets .
set.seed(100)
# Method 1: Use loop

# Single 
KDE_single_generator <- function(n, x, bw, kernel){
  # n: The number of random number to be generated
  # x: raw data. A sequence of numeric data
  # bw: bandwidth
  # kernel: kernel function
  
  if (missing(bw) && (missing(kernel)) || kernel == 'gaussian'){
    bw = density(x)$bw
    rkernel = rnorm(n, 0, sd = bw)
  }
  
  if (missing(bw) && kernel == 'rectangle'){
    rkernel = runif(n, -0.1, 0.1)
  }
  if (missing(bw) && kernel == 'logistic'){
    rkernel = rlogis(n, 0, 1)
  }
  if (missing(bw) && kernel == 'epanechnikov'){
    library(epndist)
    bw = density(x, kernel = 'epanechnikov')$bw
    rkernel = repan(n, 0, bw)
  }
  if (missing(bw) && kernel == 'triangle'){
    library(triangle)
    bw = density(x, kernel = 'triangular')$bw
    rkernel = rtriangle(n, 0, bw)
  }
  
  sample(x, n, replace=TRUE) + rkernel
}

# Example ----------

df = geyser
n = 500
ncol = 2
y = matrix(nrow = n, ncol = ncol)
system.time(for (i in 1 : ncol(df)){
  y[,i] = KDE_single_generator(n = n, x = df[,i])
})
# user  system elapsed 
# 0.005   0.000   0.005 

# Method 2 : Vectorization

KDE_multiple_generator = function(n, x, ncol, SIGMA, kernel = 'rectangle') {
  # n: The number of random number to be generated
  # x: raw data. A sequence of numeric data
  # ncol: The number of variables to do the KDE genearting numbers
  
  if (missing(SIGMA)){
    SIGMA <- sqrt(1/12) #
  }
  c <- 1 / sqrt(1 + SIGMA^2)
  
  meanMatrix <- matrix(rep(colMeans(x), each = n), ncol = ncol)
  sdMatrix <- matrix(rep(apply(x, 2, sd), each = n), ncol = ncol)  
  
  Zi <- matrix(runif(n * ncol, -.5, .5), ncol = ncol)  #
  x1 = matrix(nrow = n, ncol = ncol)
  for (i in 1 : ncol(x)){
    x1[, i] = sample(x = x[, i], size = n, replace = T)
  }
  
  output <- meanMatrix + c * (x1 -  meanMatrix + sdMatrix * Zi)
  
  return(output)
}

y_vectorization = KDE_multiple_generator(n = 500, x = df, ncol = ncol(df))
system.time(KDE_multiple_generator(n = 500, x = df, ncol = ncol(df)))

# user  system elapsed 
# 0.001   0.000   0.001 

# Plot ---------

library(ggplot2) # To get the data

data("geyser")
x = geyser$waiting
if(require("graphics")) {
  with(MASS::geyser, {
    hist(waiting, freq=FALSE, main="", border="grey", las=1)
    lines(stats::density(waiting), col="skyblue", lwd=8)
    lines(density(KDE_single_generator(x = waiting, n = 4096, kernel = 'gaussian')), col = 'green', lwd = 3)
    lines(density(KDE_single_generator(x = waiting, n = 4096, kernel = 'rectangle')), col = 'black', lwd = 3)
    lines(density(KDE_single_generator(x = waiting, n = 4096, kernel = 'triangle')), col = 'red', lwd = 3)
    rug(jitter(waiting), col="blue")
    legend("topleft", c("density histogram",
                        "KDE gaussian (denstiy)", "KDE gaussian (kde)",
                        "KDE rectangular (kde)", 'KDE triangle (kde)'), lty = "solid", lwd=c(1,8,1,1,1),
           col=c("grey", "skyblue", "green", "black",'red'), bty="n")
  })
}


