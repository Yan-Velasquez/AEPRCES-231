# seed for reproducibility
set.seed(123)
# load Library
library(MGMM)
library(mice)
library(dplyr)
source("./Functions.R")
source("./gmmpost.R")
source("./calculate_error.R")
# Cargar el dataset
data_complete <- read.csv("dataset_perovskite.csv", header = TRUE, na.strings = "NA")


# number of components
cn <- 3

############################### Training model #################################

# choose a model
compute_data_cluster <- function(dat2, cn, fix.means=F, always.random=F)
{
  print(paste("Compute", cn, "clusters" ))
  # Apply clustering
  print(sum(complete.cases(dat2)))
  if(fix.means)
  {
    print("FIX initialize")
    L = initialize_at_zero(dat2, cn)
    #fit.GMM(as.matrix(X[,1:2]), k=k, fix.means=T, M=means)
    Mt = FitGMM(as.matrix(dat2), init_means=L$M0, k=cn, fix.means, maxit = 200, report=TRUE)
  }
  else{
    print((sum(complete.cases(dat2)) < (dim(dat2)[1]*0.95)))
    print(always.random)
    if((sum(complete.cases(dat2)) < (dim(dat2)[1]*0.95)) || (always.random))
    {
      print("initialize at random")
      L = initialize_at_random(dat2, cn)
      print(L$M0)
      print(L$S0)
      Mt = FitGMM(as.matrix(dat2), init_means=L$M0, init_covs=L$S0,
                  init_props=L$pi0, maxit = 300, eps = 1e-06
                  ,  k=cn,report=TRUE)
    }
    else{
      print("initialize with k-means")
      L = initialize_at_zero(dat2, cn)
      Mt = FitGMM(as.matrix(dat2), k=cn, maxit = 200, report=TRUE) #init_covs=L$S0,
    }
  }
  print(Mt)
  return(list( dataset = dat2, MNMmix=Mt, Mean=mean(Mt), Covariance=vcov(Mt), 
               Proportion=Mt@Proportions))
}

#___________________________ Training model robust ____________________________#
compute_data_cluster_robust <- function(dat2, cn, fix.means=F,always.random=F, 
                                        max_trial = 15)
{
  i=0
  while(i < max_trial)
  {
    i = i + 1
    tryCatch({
      cl_res = compute_data_cluster(dat2, cn, fix.means, always.random)
      return(cl_res)
    }, error = function(e){
      print(paste('Error', e, "Occured for trial", i))
    })
  }
  return(NULL)
}



# Crear los índices para 5 k-folds
folds <- cut(seq(1, nrow(data_complete)), breaks = 5, labels = F)

# Inicializar listas para almacenar modelos, predicciones y errores
# Inicializar listas para 5 k-folds
models_MGMM_kfold <- vector("list", 5)
E_YX_kfold <- vector("list", 5)
predictions_kfold <- vector("list", 5)
errores_kfold <- vector("list", 5)

# Valores de faltantes para cada fold
#rm_values <- 0.001
rm_values <- c(0, 0.05, 0.101, 0.15, 0.199)

# Preparar tratamiento de datos para cada fold
for (k in 1:5) {
  # Separar datos de entrenamiento y validación
  test_indices <- which(folds == k, arr.ind = TRUE)
  data_train <- data_complete[-test_indices, ]  # Entrenamiento
  data_test <- data_complete[test_indices, ]  # Validación
  
  # Procesar y normalizar datos de entrenamiento como antes
  data1 <- data_train %>%
    filter(JV_PCE > 10 & JV_PCE <= 17.3)
  data2 <- data_train %>%
    filter(JV_PCE > 17.3 & JV_PCE <= 18.9)
  data3 <- data_train %>%
    filter(JV_PCE > 18.9)
  
  # Aplicar 'scale'
  data1_scaled <- scale(data1)
  data2_scaled <- scale(data2)
  data3_scaled <- scale(data3)
  
  # Unir los dataframes escalados
  data_train_scaled <- rbind(data1_scaled, data2_scaled, data3_scaled)
  
  # Inicializar las sublistas para cada porcentaje de faltantes
  models_MGMM_kfold[[k]] <- vector("list", length(rm_values))
  E_YX_kfold[[k]] <- vector("list", length(rm_values))
  predictions_kfold[[k]] <- vector("list", length(rm_values))
  errores_kfold[[k]] <- vector("list", length(rm_values))
  
  # Crear datasets con diferentes porcentajes de faltantes
  datasets_missing <- list()
  for (i in 1:length(rm_values)) {
    data_missing <- add_missing(data_train_scaled[, 1:11], rm = rm_values[i])
    data_missing <- cbind(data_missing, data_train_scaled[, 12:15])
    datasets_missing[[i]] <- data_missing
  }
  
  # Entrenar el modelo para cada porcentaje de datos faltantes
  models_MGMM_kfold[[k]] <- list()
  for (i in 1:length(datasets_missing)) {
    Model_MGMM <- compute_data_cluster_robust(datasets_missing[[i]], cn, fix.means = FALSE, always.random = FALSE)
    models_MGMM_kfold[[k]][[i]] <- Model_MGMM
  }
  
  # Para predicción y validación
  # Cargar datos de validación y procesarlos (similar a lo que ya tienes)
  X <- scale(data_test[, 1:11])
  Y <- data_test[, 12:15]
  
  # Número de entradas y salidas
  N_inputs <- min(ncol(X), nrow(X))
  N_outputs <- min(ncol(Y), nrow(Y))
  
  # Número de mezclas
  N_mixtures <- 3
  
  mean_Y <- c(1.09533053221288, 22.2931279178338, 0.735461157796452, 17.9212616822430)
  std_Y <- c(0.0440609290423808, 1.08667652060200, 0.0320021621207063, 1.96126188102015)
  
  # Predicciones
  predictions_kfold[[k]] <- list()
  for (i in 1:length(models_MGMM_kfold[[k]])) {
    Model_MGMM <- models_MGMM_kfold[[k]][[i]]
    
    priors <- Model_MGMM[["Proportion"]]
    centres <- list(Model_MGMM[["Mean"]][[1]], Model_MGMM[["Mean"]][[2]], Model_MGMM[["Mean"]][[3]])
    covars <- list(Model_MGMM[["Covariance"]][[1]], Model_MGMM[["Covariance"]][[2]], Model_MGMM[["Covariance"]][[3]])
    
    mix <- list(priors = priors, centres = centres, covars = covars)
    
    E_YX <- GMM_prediction(mix, X, N_inputs, N_outputs, N_mixtures, Cov_type)
    
    # Desnormalizar las predicciones
    E_YX_std <- sweep(E_YX, 2, std_Y, "*")
    Y_predict <- sweep(E_YX_std, 2, mean_Y, "+")
    
    # Guardar las predicciones y E_YX
    E_YX_kfold[[k]][[i]] <- E_YX
    predictions_kfold[[k]][[i]] <- Y_predict
  }
  
  # Convertir a matrices antes de calcular el error
  Y <- as.matrix(Y)
  Y_predict <- as.matrix(Y_predict)
  
  # Calcular errores para cada modelo
  errores_kfold[[k]] <- list()
  for (i in 1:length(predictions_kfold[[k]])) {
    Y_predict <- predictions_kfold[[k]][[i]]
    error <- calculate_error(Y, Y_predict)
    errores_kfold[[k]][[i]] <- error
  }
}
