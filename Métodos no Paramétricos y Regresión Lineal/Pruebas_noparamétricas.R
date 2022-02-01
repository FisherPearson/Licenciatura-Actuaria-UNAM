### Prueba t
antes = c(9,7,9,7,9,7,10,6,8,8,8,6,10,7,8,8,7,7)
despues = c(8,7,9,6,7,8,9,7,8,6,7,5,9,6,9,7,6,5)

# Paired two samples for means
t.test(antes,despues,paired = TRUE, alternative = "greater")
t.test(antes,despues,paired = TRUE, alternative = "two.sided")
t.test(antes,despues,paired = TRUE, alternative = "less")


### Prueba de Wilcoxon
Gemelo1 = c(77,91,86,68,91,70,71,88,71,77,87)
Gemelo2 = c(76,90,88,64,96,65,77,81,80,65,72)

wilcoxsign_test(Gemelo1~Gemelo2,paired = T, alternative = "greater", conf.level = .95)

install.packages("coin")
library(coin)

t.test(Gemelo1,Gemelo2,paired = T, alternative = "greater")


### Prueba Chi- cuadrada
freq = c(115,93,112,85,89,106)
chisq.test(freq, p = rep(1/6,6))


### Prueba de normalidad Lilliefors
library(nortest)
lillie = read.csv(choose.files())
lillie.test(lillie$ï..Z)

### Tabla de contingencia para independencia, CHi cuadrada
hombres = c(9.403,0.603,1.157,2.224,0.989,1.269)
mujeres = c(8.124,.521,1,1.922,.854,1.096)
educacion = c("No estudio", "Primaria Incompleta","Primaria Completa",
              "Secundaria", "Bachillerato", "Superior")
sexo = c("hombre","mujer")
matriz = matrix(c(hombres,mujeres), nrow = 6, ncol = 2, dimnames = c(educacion,sexo) )
row.names(matriz) = educacion
colnames(matriz) = sexo
matrizbuena = as.table(matriz)

chisq.test(matrizbuena)

