#librerias

library(readxl)

#directorio
setwd("C:/Users/Admin/Desktop/R/inferencia-estadistica-R/taller clase")
#base de datos 
Diabeticos <- read_excel("Diabeticos.xlsx")

#calcular las estad?sticas descriptivas
desc <- summary(Diabeticos)
desc

attach(Diabeticos)


#a.	Si la proporción de personas satisfechas con el servicio es mayor al 70%


media <- mean(`servicio`)
media


#b.	El servicio que utilizan con mayor frecuencia
#c.	El género que asiste con mayor frecuencia 
#d.	La entidad que remite con mayor frecuencia
#e.	Cuál es el diagnóstico que se presenta con mayor frecuencia
#f.	La edad promedio de sus pacientes diabéticos
#g.	Si se puede establecer que la edad promedio de los pacientes es de 40 años
#h.	El peso promedio perdido por los pacientes que se someten a la dieta 
