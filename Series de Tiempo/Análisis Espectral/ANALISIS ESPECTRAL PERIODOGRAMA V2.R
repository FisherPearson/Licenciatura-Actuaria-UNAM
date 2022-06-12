#
#   PROGR
library(astsa)    # BIBLIOTECA DE SERIES DE EJEMPLO

library(TTR)

library(tseries)

library("forecast")

library(LSTS)AMA DE CALCULO DEL ESPECTRO PERIODOGRAMA
#
#   Profr. Francisco S?nchez Villarreal

#   INSTALAR DEL CRAN Y CARGAR BIBLIOTECAS forecast  TTR  tseries LSTS ##############


#	LEE SERIE DE DATOS #####################################################

MANCHAS<-read.csv(file="D:RespaldoCesar/su CIENCIAS/SERIES DE TIEMPO/PROGRAMAS R/MANCHAS_SOLARES.csv",header=TRUE,dec=".",fill=TRUE)

MANCHAS

plot(MANCHAS$DATOS,type="l",xlab="Meses",ylab="N?mero de Manchas",main="Manchas Solares  1749 - 2013")

DATOS<-(MANCHAS$DATOS)


##########################################################

ESPECTRO<-spectrum(DATOS)

summary(ESPECTRO)

ESPECTRO

# Calcula la frecuencia de la serie

FRECUENCIA<-(ESPECTRO$freq)

FRECUENCIA

# Calcula el periodograma de la serie

PERIODOGRAMA<-(ESPECTRO$spec)

PERIODOGRAMA

plot(PERIODOGRAMA,type="l")


max(PERIODOGRAMA)

plot(FRECUENCIA,PERIODOGRAMA,type="l")

PERIODO=1/FRECUENCIA

PERIODO

plot(PERIODO,PERIODOGRAMA,type="l",col="red")


##########################################################

#GRABACI?N DE FRECUENCIA Y PERIODOGRAMA EN CSV

write.csv(FRECUENCIA, file="C:/CIENCIAS 2018/SERIES DE TIEMPO/PROGRAMAS R/FRECUENCIAS_MANCHAS_SOLARES.csv")


write.csv(PERIODOGRAMA, file="C:/CIENCIAS 2018/SERIES DE TIEMPO/PROGRAMAS R/PERIODOGRAMA_MANCHAS_SOLARES.csv")


