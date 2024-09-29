calculate_error <- function(Y, Y_predict) {
  # Validar que los vectores sean del mismo tamaño
  if (length(Y_predict) != length(Y)) {
    stop('Los vectores Y_predict y PCE deben tener el mismo tamaño.')
  }
  
  # Calcular el error cuadrático medio (MSE)
  mse <- colMeans((Y - Y_predict)^2)
  
  # Calcular la raíz cuadrada del MSE (RMSE)
  rmse <- sqrt(mse)
  
  # Calcular el error absoluto medio (MAPE)
  mape <- colMeans(abs((Y_predict - Y) / Y))
  
  # Mostrar el error MSE, RMSE y MAPE
  #cat('El error MSE es:', mse, '\n')
  #cat('El error RMSE es:', rmse, '\n')
  #cat('El error MAPE es:', mape, '\n')
  
  # Devolver una lista con los errores
  return(list(MSE = mse, RMSE = rmse, MAPE = mape))
}
