#Nallely Aguirre
#05/08/2019
#Clase 0

# pasos básicos -----------------------------------------------------------

2+2
a <- 2

a * a
a + 5


# importar datos ----------------------------------------------------------


diametro <- c(12, 8.6, 9.2, 7.7, 12.9, 11.7, 9.7, 14.2,
              11.8, 14.3, 12.5)

diametro

#Medidas de tendencia central

mean(diametro)
median(diametro)

#Medidas de disperción

sd(diametro)
var(diametro)
# Importar datos ----------------------------------------------------------

DB_alturas <- read.csv("C:/MCF202-2019/MCF202/Datos/alturas.csv")
boxplot(DB_alturas$crecimiento)
boxplot(DB_alturas$crecimiento ~ DB_alturas$tratamiento)
boxplot(DB_alturas$crecimiento ~ DB_alturas$tratamiento, 
        col="lightgreen",
        xlab= "tratamiento",
        ylab = "crecimiento (cm)",
        main = "efectos del fertilizante")

mean(DB_alturas$crecimiento)


# Restricciones -----------------------------------------------------------

sum(DB_alturas$crecimiento < mean(DB_alturas$crecimiento))
TratA <- DB_alturas[!(DB_alturas$tratamiento == "TA"),]

mean(TratA$crecimiento)



# Submuestra --------------------------------------------------------------

T.mean <- subset(DB_alturas,crecimiento >= mean(DB_alturas$crecimiento))

boxplot(T.mean$crecimiento ~ T.mean$tratamiento)


