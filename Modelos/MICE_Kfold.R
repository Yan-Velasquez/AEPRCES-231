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

# Imputación de datos faltantes con MICE
impute_missing_data <- function(data, method = "pmm", m = 15) {
  print("Imputando datos faltantes con MICE")
  imputed_data <- mice(data, method = method, m = m, maxit = 130, seed = 270)
  completed_data <- complete(imputed_data)
  return(completed_data)
}

# Procesar y normalizar datos
process_and_normalize <- function(data) {
  data1 <- data %>%
    filter(JV_PCE > 10 & JV_PCE <= 17.3)
  data2 <- data %>%
    filter(JV_PCE > 17.3 & JV_PCE <= 18.9)
  data3 <- data %>%
    filter(JV_PCE > 18.9)
  
  data1_scaled <- scale(data1)
  data2_scaled <- scale(data2)
  data3_scaled <- scale(data3)
  
  return(rbind(data1_scaled, data2_scaled, data3_scaled))
}

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
                   init_props=L$pi0, maxit = 250, k=cn,report=TRUE)
    }
    else{
      print("initialize with k-means")
      L = initialize_at_zero(dat2, cn)
      Mt = FitGMM(as.matrix(dat2), k=cn, maxit = 200, report=TRUE)
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
      tryCatch({
        cl_res = compute_data_cluster(dat2, cn, fix.means, always.random=T)
        return(cl_res)
      },error = function(e){
        print(paste('Error', e, "Occured for trial", i))
      })
    })
  }
  return(NULL)
}

# Crear los índices para 5 k-folds
folds <- cut(seq(1, nrow(data_complete)), breaks = 5, labels = F)

# Inicializar listas para almacenar modelos, predicciones y errores
models_MICE_kfold <- vector("list", 5)
E_YX_kfold_MICE <- vector("list", 5)
predictions_kfold_MICE <- vector("list", 5)
errores_kfold_MICE <- vector("list", 5)

# Valores de faltantes para cada fold
#rm_values <- c(0.001, 0.1)
rm_values <- c(0.001, 0.05, 0.1, 0.15, 0.2)

# Preparar tratamiento de datos para cada fold
for (k in 1:5) {
  # Separar datos de entrenamiento y validación
  test_indices <- which(folds == k, arr.ind = TRUE)
  data_train <- data_complete[-test_indices, ]  # Entrenamiento
  data_test <- data_complete[test_indices, ]  # Validación
  
  # Crear datasets con diferentes porcentajes de faltantes
  datasets_missing <- list()
  for (i in 1:length(rm_values)) {
    data_missing <- add_missing(data_train[, 1:11], rm = rm_values[i])
    data_missing <- cbind(data_missing, data_train[, 12:15])
    datasets_missing[[i]] <- data_missing
  }
  
  # Imputar datos faltantes con MICE
  datasets_MICE <- list()
  for (i in 1:length(rm_values)) {
    mice_imp <- impute_missing_data(datasets_missing[[i]])
    datasets_MICE[[i]] <- mice_imp
  }
  
  # Normalizar los datos de entrenamiento
  data_train_MICE <- list()
  for (i in 1:length(rm_values)) {
    data_train_scaled_mice <- process_and_normalize(datasets_MICE[[i]])
    data_train_MICE[[i]] <- data_train_scaled_mice
  }

  
  # Inicializar las sublistas para cada porcentaje de faltantes
  models_MICE_kfold[[k]] <- vector("list", length(rm_values))
  E_YX_kfold_MICE[[k]] <- vector("list", length(rm_values))
  predictions_kfold_MICE[[k]] <- vector("list", length(rm_values))
  errores_kfold_MICE[[k]] <- vector("list", length(rm_values))
  
  # Entrenar el modelo para cada porcentaje de datos faltantes
  models_MICE_kfold[[k]] <- list()
  for (i in 1:length(data_train_MICE)) {
    Model_MICE <- compute_data_cluster_robust(data_train_MICE[[i]], cn, fix.means = FALSE, always.random = FALSE)
    models_MICE_kfold[[k]][[i]] <- Model_MICE
  }
  
  # Para predicción y validación
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
  predictions_kfold_MICE[[k]] <- list()
  for (i in 1:length(models_MICE_kfold[[k]])) {
    Model_MICE <- models_MICE_kfold[[k]][[i]]
    
    priors <- Model_MICE[["Proportion"]]
    centres <- list(Model_MICE[["Mean"]][[1]], Model_MICE[["Mean"]][[2]], Model_MICE[["Mean"]][[3]])
    covars <- list(Model_MICE[["Covariance"]][[1]], Model_MICE[["Covariance"]][[2]], Model_MICE[["Covariance"]][[3]])
    
    mix <- list(priors = priors, centres = centres, covars = covars)
    
    E_YX_MICE <- GMM_prediction(mix, X, N_inputs, N_outputs, N_mixtures, Cov_type)
    
    # Desnormalizar las predicciones
    E_YX_std <- sweep(E_YX_MICE, 2, std_Y, "*")
    Y_predict_MICE <- sweep(E_YX_std, 2, mean_Y, "+")
    
    # Guardar las predicciones y E_YX
    E_YX_kfold_MICE[[k]][[i]] <- E_YX_MICE
    predictions_kfold_MICE[[k]][[i]] <- Y_predict_MICE
  }
  
  # Convertir a matrices antes de calcular el error
  Y <- as.matrix(Y)
  Y_predict_MICE <- as.matrix(Y_predict_MICE)
  
  # Calcular errores para cada modelo
  errores_kfold_MICE[[k]] <- list()
  for (i in 1:length(predictions_kfold_MICE[[k]])) {
    Y_predict_MICE <- predictions_kfold_MICE[[k]][[i]]
    error_MICE <- calculate_error(Y, Y_predict_MICE)
    errores_kfold_MICE[[k]][[i]] <- error_MICE
  }
}
