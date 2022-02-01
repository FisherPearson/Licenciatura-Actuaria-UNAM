#
#   PROGRAMA DE AJUSTE DE MODELO AR2 SIMULADO
#
#   
#   Profr. Francisco Sánchez Villarreal

#   CARGAR BIBLIOTECAS forecast  TTR  tseries

library(TTR)

library(tseries)

library("forecast")

library(astsa)


#	SERIE SIMULADA CON PHI1= 0.6  PHI2=0.3

SERIE_AR2<-read.csv(file="C:/RESP  ORIG/CURSO ECONOMETRIA 2011/CURSO 2016/SERIES DE TIEMPO/EJEMPLOS Y EJERCICIOS/AR2 SIMULADO.csv",header=TRUE,dec=".",fill=TRUE)

SERIE_AR2

#  GRAFICA LA SERIE SIMULADA

plot(SERIE_AR2$Zt,type="l")




#  FUNCIONES DE AUTOCORRELACIÓN SIMPLES Y PARCIALES 

SIMPLES=acf(SERIE_AR2$Zt,lag.max=30)

SIMPLES

PARCIALES=pacf(SERIE_AR2$Zt,lag.max=30)

PARCIALES



###### AJUSTA MODELO AR 2 ###############################################

MODELO_AR2<-arima(SERIE_AR2$Zt,order=c(2,0,0))

MODELO_AR2

summary(MODELO_AR2)

attributes(MODELO_AR2)

MODELO_AR2$residuals

plot(MODELO_AR2$residuals)


#  PRONOSTICOS DE 20 PERIODOS

PRONOSTICO<-predict(MODELO_AR2,n.ahead=20)

PRONOSTICO

attributes(PRONOSTICO)


plot(PRONOSTICO$pred)




#	REQUIERE GGPLOT2
#	plot(forecast(SERIE_AR2$Zt,20))




