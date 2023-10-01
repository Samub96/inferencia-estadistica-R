library(readr)
library(tidyverse)
library(dplyr)
library(xts)
library(psych)
library(gmodels)
library(MASS)
library(fitdistrplus)
library(lmtest)
library(fdth)
library(readxl)
library(ggplot2)
library(plotly)
library(PASWR2)
library(lattice)
library(descr)
library(openxlsx)
library(kableExtra)
library(pastecs)


#directorio de trabajo
setwd('C:/Users/Admin/Desktop/R/inferencia-estadistica-R/inferencia R')

#directorio en el que se esta trabajando
getwd()

#Leer la base de datos

Base_datos <- read_csv("cars_dataset.csv")
muestra <- Base_datos%>%
  filter(year>=2015 & year<=2020)


#exploracion de datos de la muestra 



  summary(muestra)
  
  # variales y sus tipos
  str(muestra)

  
attach(muestra)
  
  summary( price)
  
  
 #estadisticas descrptiva de price
  
  ## max  y min
  min <- min(price)
  max <- max(price)  
  ## rango
  rango <- max-min
  
  ##Media
  media <- mean(price)
  media
  
  ##Desviacion estandar
  desv_estandar <- sd(price)
  desv_estandar
  
  #Coeficiente de variacion
  coef_var <- desv_estandar/media
  coef_var
  
  ##Cuartil 1
  q1 <- unname(quantile(price, 0.25))
  q1
  
  ##Cuartil 3
  q3 <- unname(quantile(price, 0.75))
  q3
  
  ##Rango intercuartil
  ric <- q3 - q1
  ric
  
  ##Cerco inferior 
  cerco_inf <- q1 - 1.5*ric
  cerco_inf
  
  ##Cerco superior
  cerco_sup <- q3 + 1.5*ric
  cerco_sup
  
  des_Price <- data.frame(
    MEDIA = media,
    DESVIACION = desv_estandar,
    COEFICIENTE = coef_var,
    MAX =max,
    MIN = min,
    RANGO = rango,
    CUARTIL1 = q1,
    CUARTIL2 = q3,
    RANGO_CUARTIL = ric,
    CERCO_INF = cerco_inf,
    CERCO_SUP = cerco_sup
  )
  
  #Ver cuales se salen de los cercos
  atipicos <- filter( muestra, `price`<cerco_inf | `price`>cerco_sup)
  kable(atipicos) %>% kable_styling(full_width = F)
  
  datos_sinatipicos <- filter( muestra, `price`>=cerco_inf &`price`<=cerco_sup)
  
  max <- max(datos_sinatipicos$price)
  max
  
 
  
  
  
  
  
  
  
  
  
