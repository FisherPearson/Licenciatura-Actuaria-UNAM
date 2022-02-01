#     PROGRAMA PARA SUPERVIVENCIA
#     EJEMPLO 2 DE NOTAS SOBRE KAPLAN MEIER SIN CENSURA
#     INSTALAR Y CARGAR PAQUETES survival) KMsurv ggplot2 ggfortify  
#
#     Profr. Francisco Sánchez Villarreal
# ..................................................................................................................................


library(KMsurv)

library(survival)


#  SE LEE TABLA DE DATOS 

#  Sustituir la ruta de acceso a los datos de acuerdo a su propia carpeta

EJEMPLO1<-read.csv(file="C:/RESP  ORIG/R LENGUAJE ESTADISTICO/EJERCICIOS DE PRUEBA/SUPERVIVENCIA KAPLAN MEIER EJEMPLO 1 NOTAS.csv",header=TRUE,dec=".",fill=TRUE)

attach(EJEMPLO1)

EJEMPLO1

MISURV<-Surv(EJEMPLO1$TIEMPO,EJEMPLO1$STATUS)

MISURV

#    SE ESTIMA LA FUNCIÓN DE SUPERVIVENCIA KAPLAN MEIER ERROR ESTÁNDAR CON FÓRMULA DE GREENWOOD


KAPLAN<-survfit(Surv(EJEMPLO1$TIEMPO,EJEMPLO1$STATUS)~0)


KAPLAN

summary(KAPLAN)

#	GRÁFICA DE LA FUNCIÓN DE SUPERVIVENCIA

plot(KAPLAN,xlab="Tiempo",conf.int=T,ylab="Probabilidad de Supervivencia",main="Kaplan Meier Ejemplo 2 Notas")

names(KAPLAN)


SUPER<-plot(KAPLAN$surv)


#	CALCULAMOS LA FUNCIÓN DE DISTRIBUCIÓN ACUMULATIVA F(x)

FX<-1-KAPLAN$surv

FX

par(new=TRUE)

plot(FX)

