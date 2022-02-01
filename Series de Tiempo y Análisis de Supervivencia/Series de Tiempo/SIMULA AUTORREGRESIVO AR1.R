#
#   PROGRAMA DE SIMULACION Y AJUSTE DE MODELO DE SERIES DE TIEMPO AR1
#
#   Profr. Francisco S?nchez Villarreal

#	DEFINE EL DIRECTORIO DE REFERENCIA

#setwd("C:/RESP  ORIG/R LENGUAJE ESTADISTICO/EJERCICIOS DE PRUEBA")

library(tseries)

library(astsa)


#	SIMULA MODELO AUTORREGRESIVO DE PRIMER ORDEN AR(1,0,0)

SIM_AR1=arima.sim(list(order=c(1,0,0),
                       ar=-0.7),
                  n=156, # Numero de simulaciones
                  sd=0.5) # Desviación estandar

#  IMPRIME  LA SERIE SIMULADA 

SIM_AR1

#  GRAFICA LA SERIE SIMULADA

plot(SIM_AR1,type="l",main="Modelo AR(1) Simulado PHI = 0.8 ")

#  FUNCIONES DE AUTOCORRELACI?N SIMPLES Y PARCIALES JUNTAS

acf2(SIM_AR1,30)

#	FUNCIONES DE AUTOCORRELACI?N POR SEPARADO

SIMPLES<-acf(SIM_AR1,lag.max=30)

SIMPLES




?acf()


PARCIALES<-pacf(SIM_AR1,lag.max=30)

PARCIALES

#    MEDIA Y DESVIACION ESTANDAR DE LA SERIE

mean(SIM_AR1)
sd(SIM_AR1)

################################################################

#    GRABA EN DISCO OPCIONALMENTE EN ARCHIVO CSV LA SERIE SIMULADA 

#    write.csv(SIM_AR1,file="C:/RESP  ORIG/CURSO ECONOMETRIA 2011/CURSO 2016/SERIES DE TIEMPO/EJEMPLOS Y EJERCICIOS/AR1 SIMULADO.csv")


# AJUSTA MODELO AR1

MODELO1<-arima(SIM_AR1,order=c(1,0,0))

#  IMPRIME LOS PARAMETROS DEL MODELO

MODELO1

summary(MODELO1)

?arima()

MODELO1$residuals

#	OBTIENE PRON?STICOS A PARTIR DE RESIDUALES Y SERIE ORIGINAL


PRONOSTICO<-SIM_AR1-MODELO1$residuals

plot(PRONOSTICO,type="l",col="blue")
lines(SIM_AR1,col="green",lwd=3)


