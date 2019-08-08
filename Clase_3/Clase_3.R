#Nallely Aguirre
#07/08/2019
#Clase_3

#comparacion de medias

Grupo <- gl(2, 12, labels = c("Fotografia", "Araña"))
Ansiedad <- c(30, 35, 45, 40, 50, 35, 55, 25, 30, 45,40, 50, 40, 35, 50,
              55, 65, 55, 50, 35, 30, 50,60,39)
Datos <- data.frame(Grupo, Ansiedad)
head(Datos)
summary(Datos)

#analisis de muestras independientes
boxplot(Datos$Ansiedad ~ Datos$Grupo, col = "lightgreen", ylab = "Nivel de ansiedad")

tapply(Datos$Ansiedad, Datos$Grupo, mean)

#Ho No existe una diferencia significativa entre las variables Fotografia y araña

#H1 Existe una diferencia significativa entre las variables Fotografia y Araña

#Sapiro test
shapiro.test(Datos$Ansiedad)

library(pastecs)


# 2. ----------------------------------------------------------------------

#Ho la media es igual a 80
#H1 la media es menor a 80

costal <- c(87.7, 80.01, 77.28, 78.76, 81.52, 74.2, 80.71, 79.5, 77.87, 81.94, 80.7,
             82.32, 75.78, 80.19, 83.91, 79.4, 77.52, 77.62, 81.4, 74.89, 82.95,
             73.59, 77.92, 77.18, 79.83, 81.23, 79.28, 78.44, 79.01, 80.47, 76.23,
             78.89, 77.14, 69.94, 78.54, 79.7, 82.45, 77.29, 75.52, 77.21, 75.99,
             81.94, 80.41, 77.7)
summary(costal)
mean(costal)
#la media es estadisticamente menor a 80, es decir se acepta la H1 

#Determinar el numero de observaciones
n <-length(costal)

#Determinar la media
costa.media <- mean(costal)

#Desviacion estandar
costa.sd <- sd(costal)
sd(costal)
#formula para obener el valor de t
costa.se <- costa.sd/sqrt(n)

# valor de T
costa.T <- (costa.media - 80)/costa.se

#calcular el valor de p
pt(costa.T, df = n-1)

t.test(costal, mu= 80, alternative = "less")
t.test(costal, mu= 80, alternative = "greater")
