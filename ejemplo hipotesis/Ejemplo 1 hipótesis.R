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
setwd('D:/ADRIANA/Desktop/R')

#directorio en el que se está trabajando
getwd()

#Leer la base de datos

datos <- read_excel("datos.xlsx")
View(datos)

#calcular las estadísticas descriptivas
desc <- summary(datos, datos$`PESO PERDIDO`)
desc

attach(datos)

#Mínimo
min <- min(`PESO PERDIDO`)
min

#Máximo
max <- max(`PESO PERDIDO`)
max

#Rango
rango <- max-min
rango

#Media
media <- mean(`PESO PERDIDO`)
media

#Desviación estándar
desv_estandar <- sd(`PESO PERDIDO`)
desv_estandar

#Coeficiente de variación
coef_var <- desv_estandar/media
coef_var

##Cuartil 1
q1 <- unname(quantile(`PESO PERDIDO`, 0.25))
q1

##Cuartil 3
q3 <- unname(quantile(`PESO PERDIDO`, 0.75))
q3

#Rango intercuartil
ric <- q3 - q1
ric

#Cerco inferior 
cerco_inf <- q1 - 1.5*ric
cerco_inf

#Cerco superior
cerco_sup <- q3 + 1.5*ric
cerco_sup

#Ver cuales se salen de los cercos
atipicos <- filter(datos, `PESO PERDIDO`<cerco_inf | `PESO PERDIDO`>cerco_sup)
kable(atipicos) %>% kable_styling(full_width = F)

datos_sinatipicos <- filter(datos,`PESO PERDIDO`>=cerco_inf & `PESO PERDIDO`<=cerco_sup)

max <- max(datos_sinatipicos$`PESO PERDIDO`)
max

dt_programa3 <- datos_sinatipicos %>% 
  filter(PROGRAMA==3)

max <- max(dt_programa3$`PESO PERDIDO`)
max

attach(datos_sinatipicos)

# colocar labels a una variable categorica: importante Respetar el orden
Genero <- factor(datos_sinatipicos$GENERO, labels= c ("femenino", "masculino"))
barchart(Genero)

# gráfico de cajas

boxplot_edad = ggplot(datos_sinatipicos, aes(x='', y=EDAD)) +
  geom_boxplot(color='grey10', width=0.5) +
  labs(x='', y='Edad', title='Gráfico de cajas para la edad') +
  theme_minimal()
ggplotly(boxplot_edad)

# estadísticas descriptivas

summary(datos_sinatipicos)

descriptivas <- stat.desc(datos_sinatipicos$EDAD)
descriptivas

options(scipen = 999999)

# gráficos y tablas

tabla <- table(GENERO)
pie(tabla, main="Género de los clientes")
tabla.1 <- freq(GENERO)
tabla.1
edad2 <- cut(EDAD, c(seq(from = 19, to = 65, by = 10),65), include.lowest=TRUE)
tabla.2 <- freq(ordered(edad2), plot = TRUE)

tabla.3 <- crosstab(GENERO, edad2, prop.c = TRUE, plot=FALSE)
tabla.3
tabla.4 <- crosstab(GENERO, edad2, prop.r = TRUE,plot=FALSE)
tabla.4

# exploraciÓn de datos

eda(EDAD)

# HipÓtesis

t.test(EDAD, mu=40)

t.test(EDAD, mu=40, alternative = "less")

t.test(EDAD, mu=40, alternative = "greater")