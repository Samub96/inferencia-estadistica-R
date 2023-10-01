# Instala y carga las bibliotecas necesarias
install.packages("readxl")
install.packages("ggplot2")
library(readxl)
library(ggplot2)

# Carga los datos desde el archivo Excel
data <- read_excel("C:/Users/Samuel/Desktop/graficas proyecto inferencia/cars_dataset.xlsx")  # Reemplaza "ruta_del_archivo.xlsx" con la ruta de tu archivo
data_filtered <- data[data$year >= 2015 & data$year <= 2020,]
# Verifica que los datos se hayan cargado correctamente
head(data_filtered)
#histograma de precios
ggplot(data_filtered, aes(x = price)) +
  geom_histogram(binwidth = 1000, fill = "blue", color = "black") +
  labs(title = "Histograma de Precios", x = "Precio", y = "Frecuencia")
#Diagrama de barras para el fabricante (Make):

ggplot(data_filtered, aes(x = factor(Make))) +
  geom_bar(fill = "green") +
  labs(title = "Diagrama de Barras de Fabricantes de Autos", x = "Fabricante", y = "Cantidad")
#Gráfico de barras para contar la cantidad de autos por año de fabricación

ggplot(data_filtered, aes(x = factor(year))) +
  geom_bar(fill = "orange") +
  labs(title = "Cantidad de Autos por Año de Fabricación", x = "Año de Fabricación", y = "Cantidad")
#Gráfico de dispersión para la relación entre el tamaño del motor (engineSize) y el precio (price):
ggplot(data_filtered, aes(x = engineSize, y = price)) +
  geom_point(color = "purple") +
  labs(title = "Diagrama de Dispersión de Tamaño del Motor vs Precio", x = "Tamaño del Motor", y = "Precio")
#Boxplot para visualizar la distribución de precios (price) por tipo de combustible (fuelType):
ggplot(data_filtered, aes(x = fuelType, y = price)) +
  geom_boxplot(fill = "yellow") +
  labs(title = "Distribución de Precios por Tipo de Combustible", x = "Tipo de Combustible", y = "Precio")

#Gráfico de barras apiladas para la cantidad de autos por fabricante (Make) y tipo de transmisión (trasmision):
ggplot(data_filtered, aes(x = factor(Make), fill = factor(transmission))) +
  geom_bar(position = "stack") +
  labs(title = "Cantidad de Autos por Fabricante y Tipo de Transmisión", x = "Fabricante", y = "Cantidad") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#Diagrama de dispersión para la relación entre el precio (price) y el consumo de combustible en millas por galón (mpg):
ggplot(data_filtered, aes(x = price, y = mpg)) +
  geom_point(color = "green") +
  labs(title = "Diagrama de Dispersión de Precio vs Consumo de Combustible (MPG)", x = "Precio", y = "MPG")
#Gráfico de líneas para mostrar la evolución del precio (price) a lo largo de los años (model year):
ggplot(data_filtered, aes(x = factor(year), y = price)) +
  geom_line(color = "blue") +
  labs(title = "Evolución del Precio a lo largo de los Años", x = "Año de Fabricación", y = "Precio") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# grafico de caja para el tamaño del motor
ggplot(data_filtered, aes(y = engineSize)) +
  geom_boxplot(fill = "blue", color = "black") +
  labs(title = "Gráfico de Caja y Bigotes del Tamaño del Motor", y = "Tamaño del Motor")
# grafico de caja para el precio
ggplot(data_filtered, aes(y = price)) +
  geom_boxplot(fill = "skyblue", color = "blue") +
  labs(title = "Gráfico de Caja y Bigotes para Precios", y = "Precio")

# grafico de caja para el kilometraje
ggplot(data_filtered, aes(y = mileage)) +
  geom_boxplot(fill = "skyblue", color = "blue") +
  labs(title = "Gráfico de Caja y Bigotes para Precios", y = "Precio")

#grafico de año x kilometraje

ggplot(data_filtered, aes(x = year, y = mileage)) +
  geom_point(color = "blue") +
  labs(title = "Relación entre Año del Modelo y Kilometraje",
       x = "Año del Modelo",
       y = "Kilometraje")
#impuestos por marca de auto
ggplot(data_filtered, aes(x = Make, y = tax, fill = Make)) +
  geom_bar(stat = "identity") +
  labs(title = "Impuestos por Marca de Autos", x = "Marca", y = "Impuestos") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#tipo de combustible por marca de auto
ggplot(data_filtered, aes(x = factor(Make), fill = fuelType)) +
  geom_bar(position = "stack") +
  labs(title = "Tipos de Combustibles por Marca de Auto", x = "Marca", y = "Cantidad") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#tipo de combustible con respecto al tamaño del motor
ggplot(data_filtered, aes(x = engineSize, y = factor(fuelType), color = fuelType)) +
  geom_point(size = 3) +
  labs(title = "Relación entre Tamaño del Motor y Tipo de Combustible", 
       x = "Tamaño del Motor", 
       y = "Tipo de Combustible") +
  theme_minimal()
#precios con respecto tamaño de motor por tipo de transmision
ggplot(data_filtered, aes(x = engineSize, y = price, color = factor(transmission))) +
  geom_point() +
  labs(title = "Precio vs. Tamaño del Motor por Tipo de Transmisión", 
       x = "Tamaño del Motor", y = "Precio", color = "Tipo de Transmisión") +
  theme_minimal()


