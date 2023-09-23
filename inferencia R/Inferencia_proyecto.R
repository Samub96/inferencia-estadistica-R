library(readr)


#directorio de trabajo
setwd('C:/Users/Admin/Desktop/R/inferencia-estadistica-R/inferencia R')

#directorio en el que se esta trabajando
getwd()

#Leer la base de datos

Base_datos <- read_csv("cars_dataset.csv")

#exploracion de datos 

summary(Base_datos)

attach(Base_datos)
