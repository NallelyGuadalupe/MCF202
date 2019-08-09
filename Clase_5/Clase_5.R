#Nallely Aguirre
#09/08/2019
#Clase_5


arena <- c(6, 10, 8, 6, 14,17,9, 11, 7, 11)
arcilla <- c(17, 15, 3, 11, 14, 12, 12, 8, 10, 13)
limo <- c(13, 16, 9,12, 15, 16, 17, 13, 18, 14)

y.ton <-c(arena, arcilla, limo)
suelo <- gl(3, 10, 30, labels=c("arena", "arcilla", "limo"))

prod <-data.frame(suelo, y.ton)
head(prod)

tapply(prod$y.ton, prod$suelo, mean)
tapply(prod$y.ton, prod$suelo, var)

shapiro.test(prod$y.ton)

bartlett.test(prod$y.ton, prod$suelo)
fligner.test(prod$y.ton, prod$suelo)

boxplot(prod$y.ton ~ prod$suelo, xlab = "Tipo de suelo",
        ylab= "Ton/ha", col= "Lightblue")
aov.suelo <- aov(prod$y.ton ~ prod$suelo)
aov.suelo
summary(aov.suelo)

par(mfrow=c(2,2))
plot(aov(prod$y.ton ~ prod$suelo))
par(mfrow=c(1,1))

TukeyHSD(aov.suelo, conf.level = 0.95)

plot(TukeyHSD(aov.suelo))

summary(aov.suelo)
summary.lm(aov.suelo)

#Ho, la media de la produccion de toneladas no es diferente en ninguno de los 
#tratamientos
#H1, la media de la producion d elos tratamientos de al menos uno es diferente a
#los demas

