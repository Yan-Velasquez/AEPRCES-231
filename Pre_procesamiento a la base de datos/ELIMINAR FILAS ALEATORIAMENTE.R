# ELIMINAR FILAS ALEATORIAMENTE

eliminar_lineas_aleatoriamente <- function(df, num_lineas_a_eliminar, ruta_nuevo_archivo = NULL) {
  
  # Verificar que el número de líneas a eliminar no sea mayor que el número de filas en el data frame
  if (num_lineas_a_eliminar > nrow(df)) {
    stop("El número de líneas a eliminar es mayor que el número de filas en el data frame.")
  }
  
  # Seleccionar aleatoriamente las filas a eliminar
  set.seed(123)  # Fijar la semilla para reproducibilidad
  filas_a_eliminar <- sample(nrow(df), num_lineas_a_eliminar)
  
  # Filtrar el data frame para eliminar las filas seleccionadas
  df_filtrado <- df[-filas_a_eliminar, ]
  
  # Mostrar el data frame filtrado
  #print(df_filtrado)
  
  # Guardar el data frame resultante en un nuevo archivo CSV (opcional)
  if (!is.null(ruta_nuevo_archivo)) {
    write.csv(df_filtrado, ruta_nuevo_archivo, row.names = FALSE)
  }
  
  return(df_filtrado)
}


# Leer el archivo CSV en un data frame
df <- read.csv("RUTA_ARCHIVO.csv")

# Ruta a guardar el archivo
ruta_dt <- "RUTA_ARCHIVO.csv"

# Especificar el número de líneas que deseas eliminar
num_lineas_a_eliminar <- 1000  # Cambia este valor según tus necesidades

# Llamar a la función para eliminar líneas aleatoriamente y guardar el resultado en un nuevo archivo CSV
df_filtrado <- eliminar_lineas_aleatoriamente(df, num_lineas_a_eliminar, ruta_dt)