##################### Functions and utilities of the model #####################

min_max_scale <- function(x) # scaling in the range (0 to 1)
{
  (x - min(x)) / (max(x) - min(x))
}

add_missing <- function(datf, rm) # add missing data
{
  nval = dim(datf)[1] * dim(datf)[2]
  nmissing = floor(rm * nval)
  
  xi = sample.int(dim(datf)[1], size=nmissing, replace=T)
  ci = sample.int(dim(datf)[2], size=nmissing, replace=TRUE)
  
  for(i in 1:nmissing)
  {
    datf[xi[i], ci[i]] = NA
  }
  return(datf)
}

impute_by_fun <- function(X, imp_fun =mean ) # basic imputation(Mean, Median)
{
  for(var in names(X))
  {
    X[is.na(X[var]), var] = imp_fun(X[,var], na.rm=TRUE)
  }
  return(X)
}

############################ Model initialization ##############################

# initialize_at_random 
initialize_at_random <- function(dat2, cn)  
{
  M0 = list()
  S0 = list()
  # initialization
  ids = sample(dim(dat2)[1],cn, replace=FALSE)
  med1 = apply(dat2, 2, median, na.rm=TRUE) # median input
  for(i in 1:cn)
  {
    M0[[i]] = as.numeric(dat2[ids[i],])
    S0[[i]] = diag(dim(dat2)[2])
    idna = which(is.na(M0[[i]]))
    M0[[i]][idna] = med1[idna]
    # choose a random data point
  }
  pi0 = rep(1.0/cn, cn)
  return(list(M0=M0, S0=S0, pi0=pi0))
}

# initialize_at_zero
initialize_at_zero <- function(dat2, cn)
{
  M0 = list()
  S0 = list()
  
  Ndim = dim(dat2)[2]
  # initialization
  
  ids = sample(dim(dat2)[1], cn, replace=FALSE)
  med1 = apply(dat2, 2, median, na.rm=TRUE) # median input
  
  for(i in 1:cn)
  {
    M0[[i]] = rep(0, dim(dat2)[2])
    
    S0[[i]] = matrix(runif(Ndim*Ndim,0.01,0.1), ncol=Ndim) + 2*diag(Ndim)
    S0[[i]] = (S0[[i]]+ t(S0[[i]]))/2
    idna = which(is.na(M0[[i]]))
    M0[[i]][idna] = med1[idna]
  }
  pi0 = rep(1.0/cn, cn)
  return(list(M0=M0, S0=S0, pi0=pi0))
}

# load and save data
{
#data <- read.csv("file.csv", header = TRUE, na.strings = "NA")
#write.csv(data, file = "file.csv", row.names = FALSE)
}
