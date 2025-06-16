#Cargar paquetes necesarios

library(tidyverse)
library(readr)
library(ggplot2)
library(skimr)
library(DataExplorer)
library(dplyr)
library(naniar)

#Leer el dataset
df <- read_csv("C:/Users/alexa/OneDrive/Documentos/Bootcamp AI Machine Learning/Tarea R/bike_buyers.csv")

#Ver primeras filas
head(df)


#Ver ultimas filas
tail(df)

#Dimensiones
dim(df)

#Nombres de columnas
colnames(df)

#Ver estructura del dataset
str(df)


#Ver resumen estadistico
summary(df)


#Tipo de variables
sapply(df, class)


#Ver valores unicos
sapply(df, function(x) length(unique(x)))


#Verificar valores NA
colSums(is.na(df))


# Porcentaje de NA
mean(is.na(df)) * 100

#Visualización de valores faltantes
gg_miss_var(df)


#Imputar valores faltantes con la mediana en variables numericas
df$Income[is.na(df$Income)] <- median(df$Income, na.rm = TRUE)
df$Children[is.na(df$Children)] <- median(df$Children, na.rm = TRUE)
df$Cars[is.na(df$Cars)] <- median(df$Cars, na.rm = TRUE)
df$Age[is.na(df$Age)] <- median(df$Age, na.rm = TRUE)


# Función para obtener el valor más frecuente (modo)
get_mode <- function(x) {
  ux <- na.omit(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}

# Imputar valores faltantes en columnas categóricas con el modo
df <- df %>%
  mutate(across(where(is.character), ~ ifelse(is.na(.), get_mode(.), .)))


#Ver filas completas
sum(complete.cases(df))

#Ver duplicados
sum(duplicated(df))


#Usar skimr para resumen detallado
skim(df)

#Usar DataExplorer para overview
plot_intro(df)

barplot(table(df$`Purchased Bike`))

#Distribucion de variable objetivo
ggplot(df, aes(x = `Purchased Bike`, fill = `Purchased Bike`)) +
  geom_bar() +
  scale_fill_manual(values = c("Yes" = "forestgreen", "No" = "firebrick")) +
  labs(title = "Distribución de Compra de Bicicletas",
       x = "¿Compró bicicleta?",
       y = "Cantidad de personas") +
  theme_minimal()


# Promedio de ingresos por género
df %>%
  group_by(Gender) %>%
  summarise(AvgIncome = mean(Income, na.rm = TRUE))

# Porcentaje de personas que compraron bicicleta
prop.table(table(df$`Purchased Bike`)) * 100

#Distribución por genero

# Agrupar y contar por género
gender_counts <- df %>%
  filter(!is.na(Gender)) %>%
  count(Gender)

# Crear gráfico de pastel
ggplot(gender_counts, aes(x = "", y = n, fill = Gender)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5),
            color = "white", size = 5) +
  labs(title = "Distribución por Género",
       fill = "Género") +
  theme_void()

#Boxplot de ingresos por compra de bicicleta
ggplot(df, aes(x = `Purchased Bike`, y = Income, fill = `Purchased Bike`)) +
  geom_boxplot() +
  labs(title = "Distribución del Ingreso según Compra de Bicicleta",
       x = "¿Compró bicicleta?", y = "Ingreso") +
  theme_minimal()

#Histograma de edades
ggplot(df, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "white") +
  labs(title = "Distribución de Edad", x = "Edad", y = "Frecuencia") +
  theme_minimal()

#Gráfico combinado: Edad vs Compra
ggplot(df, aes(x = Age, fill = `Purchased Bike`)) +
  geom_histogram(binwidth = 5, position = "dodge") +
  labs(title = "Edad vs Compra de Bicicleta",
       x = "Edad", y = "Cantidad") +
  theme_minimal()

#Cantidad de Autos vs Compra de Bicicleta

ggplot(df, aes(x = as.factor(Cars), fill = `Purchased Bike`)) +
  geom_bar(position = "dodge") +
  labs(title = "Cantidad de Autos vs Compra de Bicicleta",
       x = "Cantidad de Autos",
       y = "Número de Personas",
       fill = "¿Compró bicicleta?") +
  theme_minimal()




