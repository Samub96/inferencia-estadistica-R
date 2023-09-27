library(readxl)
library(tidyverse)
library(ggplot2)
library(plotly)
library(pastecs)
library (fdth)
library(PASWR2)
library(descr)
library(lmtest)

datos <- read_excel("Taller 2.xlsx")
View(datos)

attach(datos)

# Determinar si la proporción de personas satisfechas es mayor del 70%
tabla.1 <- freq(satisfac)
tabla.1

Prueba <-prop.test(67, 90, p = 0.7, alternative = "greater")
Prueba

# Determinar el servicio solicitado con mayor frecuencia
tabla.2 <- freq(servicio)
tabla.2
tabla <- table(datos$servicio)
tabla
chisq.test(x = tabla)




