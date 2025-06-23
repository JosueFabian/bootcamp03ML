# Cargar paquetes
library(reticulate)
library(dplyr)      
library(ggplot2)    
library(readr)
library(tidyverse)
library(skimr)
library(DataExplorer)
library(scales)
library(reshape2)


# Configurar el Entorno de Python
message("--- Configurando entorno de Python con reticulate ---")

# Ruta del entorno de python
python_env_path <- "C:/EnviromentR1" 

# Ejecutar el entorno virtual
tryCatch({
  use_virtualenv(virtualenv = python_env_path, required = TRUE)
  message(paste("Usando el entorno de Python:", py_config()$python))
  
  # Opcional: Instalar paquetes Python si no están en el entorno
  # Este paso solo es necesario la primera vez para asegurar que tienes los paquetes Python necesarios
  py_install(packages = c("pandas", "numpy", "scikit-learn"), envname = python_env_path)
  message("Paquetes Python (pandas, numpy, scikit-learn) verificados/instalados.")
  
}, error = function(e) {
  message("Error al inicializar el entorno Python:")
  message(e$message)
  message("Por favor, asegúrate de que la ruta al entorno virtual sea correcta y que esté funcional.")
  stop("El script se detiene debido a un error de configuración de Python.")
})


#Pasar Datos de R a Python y Realizar Análisis con scikit-learn ---
message("\n--- Pasando datos a Python ---")

#Importar módulos de Python
pd <- import("pandas")
np <- import("numpy")
sns <- import("seaborn")
plt <- import("matplotlib.pyplot")

# Leemos el Dataset en R
df <- read_csv("C:/EnviromentR1/bike_buyers.csv")

#Convertir el data frame de R a un DataFrame de Pandas
df_py <- r_to_py(df)
message("DataFrame de R convertido a Pandas DataFrame.")

# Primeras 5 filas
df_py$head()

# Ultimas 5 filas
df_py$tail()

# Dimensiones del dataset
df_py$shape

# Ver tipos de datos
df_py$dtypes

# Información sobre el dataset
df_py$info()

# Resumen estadístico
df_py$describe()


# Conteo de valores
bike_counts <- df_py['Purchased Bike']$value_counts()

# Pie chart
plt$clf()
plt$figure(figsize = c(6, 6))
plt$pie(bike_counts, labels = bike_counts$index, autopct = '%1.1f%%', colors = c("green", "red"))
plt$title("Distribución de Compra de Bicicleta")
plt$show()

#Histograma de edades

plt$clf()
sns$histplot(data = df_py, x = 'Age', bins = as.integer(10), kde = TRUE)
plt$title("Distribución de Edad")
plt$show()


# Distribución de ingresos
plt$clf()
sns$histplot(df_py['Income'], bins=as.integer(20), kde=TRUE, color='salmon')
plt$title("Distribución de Ingresos")
plt$show()


# Boxplot de ingreso por compra de bicicleta
plt$clf()
sns$boxplot(data = df_py, x = 'Purchased Bike', y = 'Income')
plt$title("Ingreso vs Compra de Bicicleta")
plt$show()


#Conclusión
message("
Conclusión:
Las bicicletas parecen ser más populares entre adultos de mediana edad, con ingresos medios, pocas responsabilidades familiares (0-2 hijos) y que poseen 1 o 2 autos. por lo cual la compra de bicicleta es para ejercicio, recreación y no para sustituir su transporte principal.
")