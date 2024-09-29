# --------------------DETECTAR DATOS ANOMALOS

# Ruta data complete
data_p <- read.csv("Ruta base de datos completos")

#Ruta para guardar data sin anomalos
ruta_dt <- "ruta data sin anomalos"

# "---------" PRE PROCESAMIENTO "------------------------------------"

# Definir la función para procesar la columna
procesar_columna <- function(data_p, columna_a_analizar) {
  # Seleccionar la columna específica excluyendo la primera fila
  dt_columna <- data_p[-1, columna_a_analizar]
  
  # Convertir la columna a numérica, manejando los valores no convertibles
  dt_columna <- as.numeric(as.character(dt_columna))
  
  return(dt_columna)
}
dt_columna <- procesar_columna(data_p, "Columna 1")


# "----------" APLICAR MAD "------------------------------------"

# Calcular la media de los valores numéricos en dt_columna
media <- mean(dt_columna, na.rm = TRUE)

# Calcular la desviación absoluta media (DAM)
dam <- mean(abs(dt_columna - media), na.rm = TRUE)

# Establecer un umbral (por ejemplo, 2 veces la DAM)
umbral <-  1 * dam


# "----------------" IDENTIFICAR DATOS ANOMALOS "---------------------------------"


identificar_anomalias <- function(dt_columna, media, umbral) {
  # Identificar los índices de los datos anómalos
  indices_anomalias <- which(abs(dt_columna - media) > umbral)
  
  # Obtener los valores anómalos
  anomalias <- dt_columna[indices_anomalias]
  
  
  # Calcular las frecuencias de todos los valores en la columna
  frecuencias <- table(dt_columna)
  
  
  # Filtrar las frecuencias correspondientes a los valores anómalos
  frecuencias_anomalias <- frecuencias[names(frecuencias) %in% anomalias]
  
  # Convertir a data frame
  df_anomalias <- data.frame(valor = as.numeric(names(frecuencias_anomalias)),
                             frecuencia = as.numeric(frecuencias_anomalias))
  return(list(indices_anomalias = indices_anomalias, anomalias = anomalias, df_anomalias = df_anomalias))
}

resultados <- identificar_anomalias(dt_columna, media, umbral)
# Asignar los resultados a variables individuales
indices_anomalias <- resultados$indices_anomalias
anomalias <- resultados$anomalias
df_anomalias <- resultados$df_anomalias

# "--------------------" ELIMINAR DATOS ANOMALOS "---------------------------------"

# Eliminar las filas correspondientes a los valores anómalos del data frame original
eliminar_anomalias_y_guardar <- function(data_p, indices_anomalias, ruta_archivo) {
  # Eliminar las filas correspondientes a los valores anómalos del data frame original
  df_sin_anomalias <- data_p[-(indices_anomalias + 1), ]  # +1 porque excluimos la primera fila
  
  # Guardar el data frame resultante en un archivo CSV
  write.csv(df_sin_anomalias, ruta_archivo, row.names = FALSE)
}

eliminar_anomalias_y_guardar(data_p, indices_anomalias, ruta_dt)


# "-----------------ELIMINAR DATOS ANOMALOS ESPECIFICOS" -----------------------"

valores_a_excluir <- c(VALOR1, VALOR2, VALOR3)  # Reemplaza VALOR1, VALOR2 con los valores específicos

# Filtrar los valores anómalos para excluir los valores específicos
valores_a_eliminar <- anomalias[!anomalias %in% valores_a_excluir]


# Identificar los índices de los valores anómalos específicos a eliminar
indices_a_eliminar <- indices_anomalias[!anomalias %in% valores_a_excluir ]

# Eliminar las filas correspondientes a los valores anómalos específicos del data frame origina
df_sin_anomalias_especificas <- data_p[-(indices_a_eliminar + 1), ]  # +1 porque excluimos la primera fila

write.csv(df_sin_anomalias_especificas, "RUTA.csv", row.names = FALSE)