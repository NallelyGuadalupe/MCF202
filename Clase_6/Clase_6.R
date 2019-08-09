#NGAO
#Clase 6


#la clase es una mezcla entre regresion y analisis de varianza

library(repmis)
edad <- source_data("https://www.dropbox.com/s/nxoijhgmutuho0s/datos_control_Rascon.csv?dl=1")

head(edad)
str(edad)

#str, de que tipo de variables estan compuestos los datos

#Identificar columna SP como factor
edad$SP <- factor(edad$SP)
str(edad)



# Separar por factores ----------------------------------------------------

ariz <- subset(edad, SP == "arizonica")

ariz.lm <- lm(ariz$EDAD ~ ariz$DAP)
summary(ariz.lm)

dura <- subset(edad, SP == "durangensis")


# Regresion 2 factores ----------------------------------------------------

cov.edad <- lm(edad$EDAD ~ edad$DAP + edad$SP)
summary(cov.edad)

plot(edad$DAP[edad$SP == "arizonica"], edad$EDAD[edad$SP == "arizonica"],
     col= "red", pch = "A", xlim=c(0,50), ylim=c(0,130))
abline(cov.edad$coefficients[1], cov.edad$coefficients[2], col="red")

text(30, 20, "Ya = -7.65 + 1.98 * x")

points(edad$DAP[edad$SP == "durangensis"], edad$EDAD[edad$SP == "durangensis"],
       col= "blue", pch= "B")
abline(cov.edad$coefficients[1] + cov.edad$coefficients[3],
       cov.edad$coefficients[2], col= "blue", lty = "dashed")

#Para cambiar los puntos a letras en el comando "pch" se le agrega la letra con 
#la que se desea representar y se coloca entre comillas "A"

text(15, 100, "Yd = 11.41 + 1.98 * x")

#Ho no existen diferencias significativas entre las lineas de regresion
#H1 existen diferencias significativas entre las lineas de regresion

#se acepta la H1 es decir que existen diferencias significativas para las
#lineas de regresion entre ambas especies
