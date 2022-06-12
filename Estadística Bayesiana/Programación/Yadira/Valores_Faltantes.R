#Valores faltantes 
##Antes de correrlo definan una carpeta de trabajo ya que les hara archivos excel
# esto lo pueden hacer con control shift H 
#El caso mas sencillo y el peor de los casos (eliminarlos):
y <- c(1,2,3,4, NA, NA,5,6,7,8,9,NA)
table(is.na(y) )
mean(y)
#La función na.omit, devuelve el objeto con la eliminación en la lista de los valores 
#perdidos
z<-na.omit(y) #¡Ya no hay NA´s! 

#----------------- Paquetes mÃ¡s potentes para imputar datos en R -----------------

# 1) randomForest 
# 2) MICE
# 3) missForest
# 4) Amelia
# 5) Hmisc
# 6) missMDA
#7)imputeTS #SOLO PARA SERIES DE TIEMPO 

#------------------------------ 1) randomForest ----------------------------------

#install.packages("randomForest")

#Mandamos llamar a la libreria
library(randomForest)

# Primero vamos a crear valores NA aleatoriamente en la base de datos IRIS 
set.seed(100) #Fijamos una semilla
View(iris) #Sin Na
iris.na <- iris
for (i in 1:4) iris.na[sample(150, sample(50)), i] <- NA
View(iris.na) #Con NAÂ´s aleatorios
summary(iris.na)

#Hay 16 NA en Sepal.Length
#Hay 23 NA en Sepal.Width
#Hay 32 NA en Petal.Length
#Hay 22 NA en Petal.Width

# Procedemos a imputar los valores NA en nuevo data set
iris.imputed <- rfImpute(Species ~ ., iris.na)
View(iris.imputed) #¡Ya no hay valores faltantes!

#---------------------------- 2) MICE ---------------------------------------------

#Instala el package
#install.packages("mice")
#Mandamos llamar a la libreria
library(mice)

#Trabajando con la base de iris que tiene los NA´s aleatorios:
iris.na


#install.packages("VIM")
library(VIM)
#Ahora vamos a imputar los missing data
imputed_Data <- mice(iris.na, m=5, maxit = 50, method = 'pmm', seed = 500)
summary(imputed_Data)

#Los parámetros usados indican:
# m=5 Numero de imputaciones multiples. El valor predeterminado es m = 5.
# Maxit -> Se refiere al numero de iteraciones tomadas para imputar valores perdidos
# method -> Se refiere al metodo utilizado en la imputacion. Se utiliza "predictive mean matching"

#Podemos ver los valores imputados:
imputed_Data$imp$Petal.Width

#Completamos la base
completIris<- complete(imputed_Data,2) #Â¡Ya no hay valores faltantes!

# ------------------------------- 3) Amelia -------------------------------------


#Hace las siguientes suposiciones:

#-Todas las variables de un conjunto de datos tienen Distribucion Normal Multivariable (MVN). 
#-Utiliza medias y covarianzas para resumir datos.
#-Los datos que faltan son de naturaleza aleatoria.

#Instalamos y cargamos la libreria
#install.packages("Amelia")
library(Amelia)

#Seguimos trabajando con la base de datos iris con los NA´s
amelia_fit <- amelia(iris.na, m=10, parallel = "multicore", noms = "Species")

#Solo hay que tener cuidado con los parametros que le pasamos
# noms es para guardar las variables nominales

write.amelia(amelia_fit, file.stem = "imputed_data_set")

#No es recomendable usar por el supuesto de normalidad en los datos

#--------------------------- 4) HMISC ----------------------------------------

#Supuestos importantes de este paquete:
#-Supone linealidad en las variables que se predice.
#-El metodo de puntuacion optima de Fisher se utiliza para predecir variables categoricas

#Instalamos el paquete y la libreria:
#install.packages("Hmisc")
library(Hmisc)

#Imputamos los NA usando la media 
iris.na$imputed_sepal.length<- with(iris.na, impute(Sepal.Length, mean))

#Al data frame que teniamos de iris.na le vamos "pegando" las imputaciones por
#variable, en este caso estamos rellenando usando la media, nos faltarian las otras
#variables

#Si imputaramos usando un random
iris.na$imputed_sepal.length1<- with(iris.na, impute(Sepal.Length, 'random'))

#Otras medidas que pueden usarse: max, min, mediana, etc.


##
aux<-runif(150,0,14)
aux[sample(150, sample(10))] <- NA
table(is.na(aux))
library(imputeTS)
imp <- na.kalman(aux)
#distribuidos de la siguiente forma 
plotNA.distribution(imp)
##Vamos a ver donde estan los imputados 
plotNA.imputations(aux, imp, lwd=2) 
##Veamos si imputo todos 
table(is.na(imp))
##Excelente, por ultimo escribimos los datos en excel 
write.csv(imp,"imputados.csv")
