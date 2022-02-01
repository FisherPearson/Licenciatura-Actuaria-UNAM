#     PROGRAMA PARA SUPERVIVENCIA
#     EJEMPLO DE NOTAS SOBRE COMPARACIÓN DE 2 FUNCIONES DE SUPERVIVENCIA LOG RANK
#     CARGAR PAQUETES survival KMsurv ggfortify  
#
#     Profr. Francisco Sánchez Villarreal
# ..................................................................................................................................

#  Librerías en proceso de análisis 
#  install.packages("survminer")
#  library(survminer)
#  install.packages("ggplot2") 


library(KMsurv)

library(survival)

library(ggfortify)
library(ggplot2)

TIEMPO1<- c(6,6,6,7,10,13,16,22,23,6,9,10,11,17,19,20,25,32,32,34,35)
STATUS1<- c(1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0)

TIEMPO2<- c(1,1,2,2,3,4,4,5,5,8,8,8,8,11,11,12,12,15,17,22,23)
STATUS2<- c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)

#	SE OBTIENEN LAS FUNCIONES DE SUPERVIVENCIA KAPLAN MEIER POR SEPARADO

CURVA1 <- survfit(Surv(TIEMPO1, STATUS1) ~ 1)
CURVA2 <- survfit(Surv(TIEMPO2, STATUS2) ~ 1)

CURVA1
summary(CURVA1)

CURVA2
summary(CURVA2)

#	SE GRAFICAN AMBAS CURVAS DE SUPERVIVENCIA

plot(CURVA1,conf.int=F,col="blue",xlab="Tiempo",ylab="Probabilidad de Supervivencia")

lines(CURVA2, conf.int=F,col = "red")

#	ETIQUETAS ADICIONALES PARA LA GRÁFICA

legend(21,1,c("Grupo 1(Trat.)", "Grupo 2(Placebo)"), col = c("blue","red"), lty = 1)
title(main="KM-Curvas de Supervivencia")




#     PRUEBA LOG RANK JI CUADRADA PARA COMPARACIÓN DE CURVAS
#     LA ESTRUCTURA DE DATOS SE PRESENTA CON AMBOS GRUPOS MEZCLADOS Y UN VECTOR DE ETIQUETAS DE PERTENENCIA AL GRUPO


TIEMPO <- c(6,6,6,7,10,13,16,22,23,6,9,10,11,17,19,20,25,32,32,34,35,1,1,2,2,3,4,4,5,5,8,8,8,8,11,11,12,12,15,17,22,23)
STATUS <- c(1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
TRATAMIENTO <- c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2)


TEST_LOG_RANK<- survdiff(Surv(TIEMPO, STATUS) ~ TRATAMIENTO)

TEST_LOG_RANK

names(TEST_LOG_RANK)


summary(TEST_LOG_RANK)

