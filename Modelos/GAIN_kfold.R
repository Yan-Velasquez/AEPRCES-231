# seed for reproducibility
set.seed(123)
# load Library
library(MGMM)
library(dplyr)
source("./Functions.R")
source("./gmmpost.R")
source("./calculate_error.R")
# Cargar el dataset
data_complete <- read.csv("dataset_perovskite.csv", header = TRUE, na.strings = "NA")
# Inicializar la lista datasets_GAIN
datasets_GAIN <- vector("list", 5)

# Rutas de los archivos
file_paths <- list(
  c("gain/data_train_k1.csv", "gain/dt_imp_k1_x5.csv", "gain/dt_imp_k1_x10.csv", "gain/dt_imp_k1_x15.csv", "gain/dt_imp_k1_x20.csv"),
  c("gain/data_train_k2.csv", "gain/dt_imp_k2_x5.csv", "gain/dt_imp_k2_x10.csv", "gain/dt_imp_k2_x15.csv", "gain/dt_imp_k2_x20.csv"),
  c("gain/data_train_k3.csv", "gain/dt_imp_k3_x5.csv", "gain/dt_imp_k3_x10.csv", "gain/dt_imp_k3_x15.csv", "gain/dt_imp_k3_x20.csv"),
  c("gain/data_train_k4.csv", "gain/dt_imp_k4_x5.csv", "gain/dt_imp_k4_x10.csv", "gain/dt_imp_k4_x15.csv", "gain/dt_imp_k4_x20.csv"),
  c("gain/data_train_k5.csv", "gain/dt_imp_k5_x5.csv", "gain/dt_imp_k5_x10.csv", "gain/dt_imp_k5_x15.csv", "gain/dt_imp_k5_x20.csv")
)

# Cargar los datasets en la lista datasets_GAIN
for (k in 1:5) {
  datasets_GAIN[[k]] <- vector("list", 5)
  for (i in 1:5) {
    datasets_GAIN[[k]][[i]] <- read.csv(file_paths[[k]][i], header = TRUE)
  }
}

# Ahora tienes datasets_GAIN[[k]][[i]] con los archivos cargados
# number of components
cn <- 3

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
      print(paste('Error', e, "Occured for trial", i))
    })
  }
  return(NULL)
}

# Crear los índices para 5 k-folds
folds <- cut(seq(1, nrow(data_complete)), breaks = 5, labels = F)

# Inicializar listas para almacenar modelos, predicciones y errores
models_GAIN_kfold <- vector("list", 5)
E_YX_kfold_GAIN <- vector("list", 5)
predictions_kfold_GAIN <- vector("list", 5)
errores_kfold_GAIN <- vector("list", 5)

# Valores de faltantes para cada fold
#rm_values <- 0.001
rm_values <- c(0, 0.05, 0.1, 0.15, 0.2)

# Preparar tratamiento de datos para cada fold
for (k in 1:5) {
  # Separar datos de entrenamiento y validación
  test_indices <- which(folds == k, arr.ind = TRUE)
  data_test <- data_complete[test_indices, ]  # Validación
  
  # Normalizar los datos de entrenamiento
  data_train_GAIN <- list()
  for (i in 1:length(rm_values)) {
    data_train_scaled_gain <- process_and_normalize(datasets_GAIN[[k]][[i]])
    data_train_GAIN[[i]] <- data_train_scaled_gain
  }
  
  # Inicializar las sublistas para cada porcentaje de faltantes
  models_GAIN_kfold[[k]] <- vector("list", length(rm_values))
  E_YX_kfold_GAIN[[k]] <- vector("list", length(rm_values))
  predictions_kfold_GAIN[[k]] <- vector("list", length(rm_values))
  errores_kfold_GAIN[[k]] <- vector("list", length(rm_values))
  
  # Entrenar el modelo para cada porcentaje de datos faltantes
  models_GAIN_kfold[[k]] <- list()
  for (i in 1:length(data_train_GAIN)) {
    Model_GAIN <- compute_data_cluster_robust(data_train_GAIN[[i]], cn, fix.means = FALSE, always.random = FALSE)
    models_GAIN_kfold[[k]][[i]] <- Model_GAIN
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
  predictions_kfold_GAIN[[k]] <- list()
  for (i in 1:length(models_GAIN_kfold[[k]])) {
    Model_GAIN <- models_GAIN_kfold[[k]][[i]]
    
    priors <- Model_GAIN[["Proportion"]]
    centres <- list(Model_GAIN[["Mean"]][[1]], Model_GAIN[["Mean"]][[2]], Model_GAIN[["Mean"]][[3]])
    covars <- list(Model_GAIN[["Covariance"]][[1]], Model_GAIN[["Covariance"]][[2]], Model_GAIN[["Covariance"]][[3]])
    
    mix <- list(priors = priors, centres = centres, covars = covars)
    
    E_YX_GAIN <- GMM_prediction(mix, X, N_inputs, N_outputs, N_mixtures, Cov_type)
    
    # Desnormalizar las predicciones
    E_YX_std <- sweep(E_YX_GAIN, 2, std_Y, "*")
    Y_predict_GAIN <- sweep(E_YX_std, 2, mean_Y, "+")
    
    # Guardar las predicciones y E_YX
    E_YX_kfold_GAIN[[k]][[i]] <- E_YX_GAIN
    predictions_kfold_GAIN[[k]][[i]] <- Y_predict_GAIN
  }
  
  # Convertir a matrices antes de calcular el error
  Y <- as.matrix(Y)
  Y_predict_GAIN <- as.matrix(Y_predict_GAIN)
  
  # Calcular errores para cada modelo
  errores_kfold_GAIN[[k]] <- list()
  for (i in 1:length(predictions_kfold_GAIN[[k]])) {
    Y_predict_GAIN <- predictions_kfold_GAIN[[k]][[i]]
    error_GAIN <- calculate_error(Y, Y_predict_GAIN)
    errores_kfold_GAIN[[k]][[i]] <- error_GAIN
  }
}
