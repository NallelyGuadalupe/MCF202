#Nallely Aguirre
#06/08/2019
#Clase_2


# Importar datos vivero ---------------------------------------------------

vivero <- read.csv("C:/MCF202-2019/MCF202/Datos/tvivero.csv", header = T)
summary(vivero)


# Prueba de t una muestra -------------------------------------------------

par(mfrow=c(1,1))
boxplot(vivero$IE)
t.test(vivero$IE, mu = 0.85)

#la media ibservada no es diferente estadisticamente ya que el valor de p
#es mayor que le alfa establecido (0.05). Ademas la media teoretica se
#encuentra dentro del rango de los valores del intervalo de confianza.

t.test(vivero$IE, mu = 0.90)
#la media observada es diferente a la media teorica por lo cual aceptamos
#la H1. El valor de p (0.01) es menor que el valor alfa establecido (0.05) 


# pruebas de t muestras independientes ------------------------------------

boxplot(vivero$IE ~ vivero$Tratamiento, col= "lightblue", xlab = "Tratamiento",
        ylab = "IE")
shapiro.test(vivero$IE)
var.test(vivero$IE ~ vivero$Tratamiento)

#las varianzas de ambos tratamientos son iguales asi lo prueba el valor de p
#obtenido mediante una prueba de varianzas (var.test).

t.test(vivero$IE ~ vivero$Tratamiento, var.equal =T)

#El valor de p no existe una diferencia significativa entre el IE de las plantulas fertilizadas
#El valor de p comprueba nuestra hipotesis de que el fertilizante "Power"
#mejora el IE

t.test(vivero$IE ~ vivero$Tratamiento)


# Prueba de t muestras dependientes ---------------------------------------

t.test(vivero$IE ~ vivero$Tratamiento, paired = T)

inventario <- read.csv("C:/MCF202-2019/MCF202/Datos/produccion.csv")
summary(inventario)

boxplot(inventario$Kgsem ~ inventario$Tiempo, col= "lightblue")
t.test(inventario$Kgsem ~ inventario$Tiempo, paired=T)

#no hay diferencias significativas entre las graficas 

boxplot(inventario$Germ ~ inventario$Tiempo, col= "green")
t.test(inventario$Germ ~ inventario$Tiempo, paired=T)

tapply(inventario$Germ, inventario$Tiempo, mean)
boxplot(inventario$Germ ~ inventario$Tirmpo)
t.test(inventario$Germ ~ inventario$Tiempo, paired = T)
