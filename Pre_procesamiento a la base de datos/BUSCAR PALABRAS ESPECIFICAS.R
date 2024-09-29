# BUSCAR PALABRAS ESPECIFICAS DENTRO DE LA BASE DE DATOS --------------------------

# Leer el archivo CSV en un data frame
df <- read.csv("Ruta del archivo")

# Especificar la palabra que deseas buscar
palabra_a_eliminar <- "palabra"

# Filtrar el data frame para eliminar las filas que contienen esa palabra
df_filtrado <- df[!apply(df, 1, function(fila) any(fila == palabra_a_eliminar)), ]

# Mostrar el data frame filtrado
#print(df_filtrado)

# Guardar el data frame resultante en un nuevo archivo CSV (opcional)
write.csv(df_filtrado, "datos_eliminados.csv", row.names = FALSE)
