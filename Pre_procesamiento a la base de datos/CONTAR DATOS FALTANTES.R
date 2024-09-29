# CONTAR DATOS FALTANTES -----------------------------------------------------

# - - - - - - -- - - - - - - - - - 
# Leer el archivo CSV en un data frame
dt_pp <- read.csv("ruta del archivo")

# Especificar la columna que deseas analizar
columna_a_analizar <- "nombre columna"

# Contar el número de valores faltantes (NA) en la columna
# num_na <- sum(is.na(dt_pp[[columna_a_analizar]]))

# Contar el número de datos vacíos (cadenas vacías) en la columna
num_vacios <- sum(trimws(as.character(dt_pp[[columna_a_analizar]])) == "0", na.rm = TRUE)

# Sumar ambos conteos para obtener el total de valores faltantes o vacíos
total_faltantes_vacios <- num_na + num_vacios

# Mostrar el total de valores faltantes o vacíos
print(paste("Total de valores faltantes o vacíos en la columna", columna_a_analizar, ":", total_faltantes_vacios))