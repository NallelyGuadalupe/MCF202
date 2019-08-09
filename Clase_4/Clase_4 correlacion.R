#Nalleyly Aguirre
#09/08/2019
#Clase_4

library(repmis)
erupciones <- source_data("https://dl.dropboxusercontent.com/s/liir6sil7hkqlxs/erupciones.csv")
#para cargar bases de datos desde el internet

plot(log(erupciones$waiting), log(erupciones$eruption), pch=19, col= "blue",
     xlab= "Tiempo de espera (min)",
     ylab = "Duración (min)")

library("pastecs")
stat.desc(erupciones$eruptions,basic=FALSE, norm=TRUE)

shapiro.test(log(erupciones$eruptions))
shapiro.test(erupciones$waiting)
#test de normalidad

cor.test(erupciones$eruptions, erupciones$waiting)
#la correlacion si es significativa porque el valor de p en la cor.test es 
#menor al alfa

#si la correlacion no es significativa no se puede hacer la regresion




# Actividad ebanos --------------------------------------------------------


#carga de datos

ebanos <- read.csv("C:/MCF202-2019/MCF202/Datos/ebanos.csv", header = T)
summary(ebanos)

#Hipotesis
#Ho, no existen diferencias significativas entre las variables altura y diametro de 
#los ebanos

#H1, existen diferencias significativas entre las variables altura y diametro de
#los ebanos

plot(log(ebanos$diametro), log(ebanos$altura), pch=20, col= "red", main = "Ebanos",
     xlab= "Diametro",
     ylab = "Altura")

library("pastecs")
stat.desc(ebanos$altura, basic=FALSE, norm=TRUE)
#los comandos de arriba solo utilizan la variable altura porque es la variable 
#dependiente, la altura depende del diametro

shapiro.test(ebanos$diametro)
#los datos no se distribuyen de forma normal ya que el valor de p (1.215e-05) 
#es menor al alfa establecido (0.05), entonces si existen diferencias estadisticamente
#significativas

shapiro.test(ebanos$altura)
#el valor de p (0.008242) obtenido en la prueba de normalidad para altura resulto 
#ser menor al alfa estabecido de 0.05 por lo tanto los datos no tienen una distribucion
#normal y si existen diferencias estadisticamente significativas

cor.test(ebanos$altura, ebanos$diametro)
#la correlacion si es significativa porque el valor de p (2.2e-16) es menor 
#al alfa estableciado (0.05), y por lo tanto si se puede llevar a cabo una regresion


# Regresion lineal --------------------------------------------------------

#Hipotesis de la actividad
#la duracion del tiempo d espera nos ayuda a predecir la duracion de explosion

#Ho, no es significativa, no predice una hacia la otra
#H1, si nos ayuda a predecir

lm.erup <- lm(erupciones$eruptions ~ erupciones$waiting)

plot(erupciones$waiting, erupciones$eruptions, pch=20, col= "red", main = "Erupciones",
     xlab= "Tiempo de espera (min)",
     ylab = "Duracion (min)")
abline(lm.erup,col= "black")
text(52,4.5, "Y= -1.87 + 0.07*x", pos=1)
text(52, 4, "r^2 = 0.81")
#pos, desde donde se quiere que inicie el texto

lm.erup
summary(lm.erup)
#lm: es lineal modelpara realizar la regresion
#Residuales: diferencia que existe entre el valor observado y el predicho, cuando
#son positivos y cuando son negativos, el valor observado es menor a la linea de
#tendencia central es positivo es porque esta por encima d ela linea y es negativo 
#cuando esta por debajo de la linea

length(erupciones$eruptions)

y.60 <- -1.87 + 0.07*60
y.60


# Datos de regresion ------------------------------------------------------

espera <- erupciones$waiting
duracion <- erupciones$eruptions
res <- resid(lm.erup)
res
sum(res)

pre <- fitted(lm.erup)
res.2 <- res^2

cuadro <- data.frame(espera, duracion, pre, res, res.2)

View(cuadro)
cuadro <- round(data.frame(espera, duracion, pre, res, res.2),4)

SSE <- sum(cuadro$res.2)
SSE

vari <- SSE/(length(erupciones$waiting)-2)
vari


# Prueba de hipotesis de regresion ----------------------------------------

an.erup <- anova(lm.erup)
an.erup

#se acepta la H1