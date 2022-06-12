
#	BOOTSTRAP PARA LA ESTADISTICA MEDIA MUESTRAL


library(boot)

?boot


DATOS<-c(12,13,14,13,16,17,12,14,15,13,18,21,11,13,15,16,17,15,13,12,18)

n=length(DATOS)
n

DATOS

MEDIA<-mean(DATOS)

MEDIA


var(DATOS)

VAR_MEDIA<-var(DATOS)/n

VAR_MEDIA

EEMEDIA<-sqrt(var(DATOS)/n)

#	ERROR ESTÁNDAR DE LA MEDIA CALCULADA POR FÓRMULA


EEMEDIA


#	SE DEFINE FUNCIÓN PARA CALCULAR LA MEDIA DE LA MUESTRA BOOTSTRAP


?sample()

FC <- function(DATOS, sample){mean(DATOS[sample])}

FC

#	SE OBTIENEN 1000 MUESTRAS BOOTSTRAP Y SE CALCULA LA MEDIA DE CADA MUESTRA CON FC

RESULTADO=boot(data = DATOS,statistic = FC ,R = 1000)

RESULTADO

#	ESTRUCTURA DEL ARCHIVO RESULTADO

str(RESULTADO)

#	LAS 10000 MUESTRAS BOOTSTRAP

MUESTRA<-RESULTADO$t

MUESTRA

#	EL ERROR ESTÁNDAR DE LA MEDIA A PARTIR DE LAS 1000 MUESTRAS BOOTSTRAP

sd(MUESTRA)

#	ERROR ESTÁNDAR POR FÓMULA ORIGINA S2/n 


EEMEDIA


#	HISTOGRAMA DE LAS 1000 MUESTRAS BOOTSTRAP

hist(MUESTRA,breaks = "Sturges",col = "blue",main = "Histograma de Muestras Bootstrap",ylab = "Frecuencia")

summary(RESULTADO$t)

#	CÁLCULO DEL SESGO

mean(RESULTADO$t)


SESGO_MEDIA<-mean(RESULTADO$t) - MEDIA

SESGO_MEDIA


SESGO_EE<- 0.5235679-EEMEDIA

SESGO_EE




#	CÁLCULO DEL ERROR ESTÁNDAR

sd(RESULTADO$t)

EEMEDIA=sd(DATOS)/sqrt(21)

#	INTERVALO DE CONFIANZA DE 95% MEDIANTE BOOTSTRAP

boot.ci(boot.out = RESULTADO, conf=0.95,type = c("norm", "perc"))




#	INTERVALO DE CONFIANZA PARA LA MEDIA NORMAL Y BOOTSTRAP

boot.ci(boot.out = bootcorr, conf=0.95,type = c("norm", "basic", "perc", "bca"))


?boot.ci()
















