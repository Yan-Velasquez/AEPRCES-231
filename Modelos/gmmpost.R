library(MASS)
library(mvtnorm)

# Función de predicción GMM
GMM_prediction <- function(mix, X, Nx, Ny, Nmix, Cov_type) {
  Ndata <- nrow(X)
  
  # Crear el modelo GMM para X
  mix_X <- list()
  mix_X$centres <- lapply(mix$centres, function(c) c[1:Nx])
  mix_X$priors <- mix$priors
  
  # Verificar la dimensión de la matriz de covarianza
  ndim <- length(dim(mix$covars[[1]]))
  
  if (ndim == 2) {
    mix_X$covars <- lapply(mix$covars, function(covar) covar[1:Nx, 1:Nx])
  }
  
  # Calcular las responsabilidades (probabilidades posteriores)
  resp <- gmmpost(mix_X, X, Nx)
  
  # Inicializar la salida
  E_YX <- matrix(0, nrow = Ndata, ncol = Ny)
  
  for (j in 1:Nmix) {
    # Extraer la matriz de covarianza
    Cov_mat <- mix$covars[[j]]
    CovXX <- Cov_mat[1:Nx, 1:Nx]
    CovYY <- Cov_mat[(Nx + 1):nrow(Cov_mat), (Nx + 1):ncol(Cov_mat)]
    CovXY <- Cov_mat[1:Nx, (Nx + 1):ncol(Cov_mat)]
    CovYX <- t(CovXY)
    
    # Estimar m_j(X)
    muX <- mix$centres[[j]][1:Nx]
    muY <- mix$centres[[j]][(Nx + 1):length(mix$centres[[j]])]
    M_jX <- CovYX %*% solve(CovXX) %*% t(X - matrix(muX, nrow = Ndata, ncol = Nx, byrow = TRUE))
    M_jX <- matrix(muY, nrow = Ndata, ncol = Ny, byrow = TRUE) + t(M_jX)
    
    # Estimar Ej_YX
    E_YX <- E_YX + resp[, j] * M_jX
  }
  
  return(E_YX)
}

# Función gmmpost para calcular las probabilidades posteriores
gmmpost <- function(mix, X, Nx) {
  Ndata <- nrow(X)
  Nmix <- length(mix$priors)
  resp <- matrix(0, nrow = Ndata, ncol = Nmix)
  
  for (i in 1:Ndata) {
    for (j in 1:Nmix) {
      x <- X[i, 1:Nx]
      mean <- mix$centres[[j]][1:Nx]
      sigma <- mix$covars[[j]][1:Nx, 1:Nx]
      prob <- mix$priors[j] * dmvnorm(x, mean = mean, sigma = sigma)
      resp[i, j] <- prob
    }
    resp[i, ] <- resp[i, ] / sum(resp[i, ])
  }
  
  return(resp)
}
