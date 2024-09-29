# ELIMIAR VALORES FALTANTES -------------------------------------------------

# Especificar las columnas que deseas analizar
columnas_a_analizar <- c("Columna 1", "Columna 2", "Columna 3", "Columna 3")

# Identificar las filas con valores faltantes (NA) o vacíos en cualquiera de las columnas especificadas
filas_a_eliminar <- 
  apply(dt_pp[, columnas_a_analizar], 1, function(fila) {
    any(is.na(fila) | trimws(as.character(fila)) == "")
  })

# Filtrar el data frame para eliminar esas filas
dt_pp_filtrado <- dt_pp[!filas_a_eliminar, ]


# Guardar el data frame resultante en un nuevo archivo CSV (opcional)
write.csv(dt_pp_filtrado, "Ruta a guardar archivo", row.names = FALSE)
