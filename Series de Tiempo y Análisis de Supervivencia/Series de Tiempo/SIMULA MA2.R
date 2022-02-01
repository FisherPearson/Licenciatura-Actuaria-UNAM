#
#   PROGRAMA DE SIMULACION Y AJUSTE DE MODELO DE SERIES DE TIEMPO MA2
#
#   Profr. Francisco Sánchez Villarreal


library(astsa)

library(tseries)

library(forecast)


SIM_MA2=arima.sim(list(order=c(0,0,2), ma=c(0.6,0.4)), n=160,sd=0.3)


#  IMPRIME  LA SERIE SIMULADA 

SIM_MA2

#  GRAFICA LA SERIE SIMULADA MA2


plot(SIM_MA2,type="l",main="Modelo MA(2)Simulado Theta1= 0.6 Theta2=-0.4 s=0.3")

#  FUNCIONES DE AUTOCORRELACIÓN SIMPLES Y PARCIALES 

SIMPLES<-acf(SIM_MA2,lag.max=30)

SIMPLES

PARCIALES<-pacf(SIM_MA2,lag.max=30)

PARCIALES

#	FUNCIONES DE AUTOCORRELACIÓN SIMPLES Y PARCIALES JUNTAS

acf2(SIM_MA2)



#    MEDIA Y DESVIACION ESTANDAR DE LA SERIE

mean(SIM_MA2)
sd(SIM_MA2)

################################################################

#    GRABA OPCIONALMENTE EN ARCHIVO CSV LA SERIE SIMULADA 

#    write.csv(SIM_MA2,file="C:/CURSO ECONOMETRIA 2011/CURSO 2016/SERIES DE TIEMPO/EJEMPLOS Y EJERCICIOS/MA2 SIMULADO.csv")


# AJUSTA MODELO MA2

MODELO1<-arima(SIM_MA2,order=c(0,0,2))

#  IMPRIME LOS PARAMETROS DEL MODELO

MODELO1

summary(MODELO1)

names(MODELO1)


#	GRAFICA AMBOS OBSERVADOS Y ESTIMADOS JUNTOS

RESIDUAL<-residuals(MODELO1)

RESIDUAL

plot(RESIDUAL,,type="l")


ESTIMADOS<-SIM_MA2-RESIDUAL
ESTIMADOS


plot(SIM_MA2,type="l",col="2")
par(new=TRUE)

plot(ESTIMADOS,type="l",col="3")



#     AUTOCORRELACIONES SIMPLES Y PARCIALES DE RESIDUALES PARA VERIFICAR QUE NO HAY
#     PATRONES SUBYACENTES

SIMPLES<-acf(RESIDUAL,lag.max=30)

SIMPLES

PARCIALES<-pacf(RESIDUAL,lag.max=30)

PARCIALES

#	PRONOSTICOS HACIA ADELANTE DEL MODELO POR 12 PERIODOS

sarima(SIM_MA2,0,0,2,0,0,0,0)

PRONOSTICOS<-sarima.for(SIM_MA2,12,0,0,2,0,0,0,0)



plot(SIM_MA2,type="l",col="2")
par(new=TRUE)

plot(ESTIMADOS,type="l",col="3")










#	OBTIENE LOS VALORES ESTIMADOS POR EL MODELO

#	AJUSTADOS<-fitted.values(MODELO1)
#	AJUSTADOS

#	plot(AJUSTADOS,type="l",col="1")





























