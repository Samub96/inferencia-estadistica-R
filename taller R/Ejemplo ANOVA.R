library(readxl)
library(tidyverse)
library(ggplot2)
library(plotly)
library(pastecs)
library (fdth)
library(PASWR2)
library(descr)
library(lmtest)

datos <- read_excel("D:/ADRIANA/Desktop/R/datos.xlsx")
View(datos)

# colocar labels a una variable categorica: importante Respetar el orden
Genero <- factor(datos$GENERO, labels= c ("femenino", "masculino"))
barchart(Genero)

attach(datos)

# gráfico de cajas

boxplot_edad = ggplot(datos, aes(x='', y=EDAD)) +
  geom_boxplot(color='grey10', width=0.5) +
  labs(x='', y='Edad', title='Gráfico de cajas para la edad') +
  theme_minimal()
ggplotly(boxplot_edad)

# estadísticas descriptivas

summary(datos)

descriptivas <- stat.desc(EDAD)
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

#Ho: No existe relación entre el género y el rango de edad
#H1: Existe relación entre el genero y el rango de edad

with(datos, chisq.test(GENERO, edad2, correct = TRUE))

# exploraciÓn de datos

eda(EDAD)

# HipÓtesis

t.test(EDAD, mu=40)

t.test(EDAD, mu=40, alternative = "less")

t.test(EDAD, mu=40, alternative = "greater")

var.test(EDAD~GENERO)

t.test(EDAD~GENERO, var.equal=TRUE)


t.test(x = datos$`PESO ANTES`, y = datos$`PESO DESPUÉS`, alternative = "two.sided",
       mu = 0, paired = TRUE, conf.level = 0.95)

# Anova

tratamiento <-as.factor(PROGRAMA)
boxplot(`PESO PERDIDO`~PROGRAMA)
tapply(`PESO PERDIDO`,PROGRAMA, mean)
anova<-aov(lm(`PESO PERDIDO`~tratamiento))
summary(anova)
TukeyHSD(anova)