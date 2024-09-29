# AEPRCES-231 - Trabajo de Grado
Autores: Daniel Andrés Cerro Ramos y Yan Carlos Velasquez Meneses


Implementación de algoritmos de entrenamiento de modelos probabilísticos para predecir el rendimiento de celdas solares de perovskita con datos incompletos

Este repositorio contiene el código y los archivos necesarios para entrenar y validar modelos basados en Mezcla Gaussiana Conscientes de Faltantes (MGMM)en y en modelos de mezclas gaussianas (GMM) utilizando técnicas de imputación de datos con MICE y GAIN en el contexto de datos de celdas solares de perovskita.

1. Pre_procesamiento a la base de datos

   En esta carpeta encontraras los archivos que te ayudaran a organizar tus datos antes de la implementaciones, encontraras los siguientes archivos:

   **BUSCAR PALABRAS ESPECIFICAS.R:** En caso de que necesites buscar algún palabra o dato especifico para filtrarlo por columna, eliminarlo o modificarlo, puedes realizar con este archivo.

   **CONTAR DATOS FALTANTES.R:** Te ayuda a identificar cuantos datos faltantes tienes en tu conjunto de datos.

   **DETECTAR DATOS ANOMALOS.R:** Se encarga de identificar, eliminar o evaluar datos anómalos en tu conjunto de datos.

   **ELIMIAR VALORES FALTANTES.R**

   **ELIMINAR FILAS ALEATORIAMENTE.R**



2. Modelos
   Se encuentran los siguiente archivos principales:
   
   MGMM_fold.R: Entrenamiento y validación del modelo MGMM utilizando k-fold cross-validation.
   
   MICE_fold.R: Entrenamiento y validación del modelo GMM con imputación MICE usando k-fold cross-validation.
   
   GAIN_fold.R: Entrenamiento y validación del modelo GMM con imputación GAIN usando k-fold cross-validation.
   
   calculate_error.R: Script para calcular las métricas de error (RMSE y MAPE) a partir de las predicciones generadas por los modelos.
   
   Functions.R: Contiene todas las funciones utilizadas en los modelos, como la inicialización, normalización de los datos, manejo de datos faltantes, entre otras.
   
   gmmpost.R: Implementación del modelo de regresión de mezcla gaussiana (GMM) utilizado para hacer predicciones.
   
   dataset_perovskite.csv: Base de datos con 1070 entradas y 15 parámetros, utilizada para entrenar y validar los modelos. Esta es la base de datos 2 del proyecto.
   Carpeta adicional:
   gain/: Contiene las imputaciones generadas por el método GAIN. Estas imputaciones fueron procesadas utilizando Python.
   
