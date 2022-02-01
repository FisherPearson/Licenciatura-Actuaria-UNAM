#	JARQUE_BERA
#	PRUEBA DE NORMALIDAD JARQUE BERA
#
#	

install(normtest)

library(normtest)

library(moments)

MUESTRA<-c( 441.89, 
 484.14, 
 392.62, 
 518.33, 
 542.11, 
 555.27, 
 592.17, 
 574.36, 
 627.02, 
 597.48, 
 571.04, 
 483.21, 
 545.08, 
 588.62, 
 515.17, 
 502.11, 
 412.80, 
 397.98, 
 482.18, 
 563.74, 
 544.29, 
 528.22, 
 491.71, 
 517.26, 
 591.26, 
 455.27, 
 476.38, 
 441.48, 
 416.74, 
 519.85, 
 488.72, 
 578.00, 
 470.61, 
 498.86, 
 426.52)

MUESTRA

min(MUESTRA)
max(MUESTRA)

MEDIA<-mean(MUESTRA)
MEDIA

SD<-sd(MUESTRA)
SD
#	ASIMETRIA

skewness(MUESTRA)

#	KURTOSIS

?kurtosis()

kurtosis(MUESTRA)



#	HISTOGRAMA DE FRECUENCIAS

hist(MUESTRA,prob=TRUE,ylim=c(0,.010), col="green")

X<-seq(min(MUESTRA-50),max(MUESTRA+50),length=40)
X

F<-dnorm(X,mean=MEDIA,sd=SD)
F

lines(X, F, col = "red", lwd = 2)

#	GRÁFICA DISTRIBUCIÓN NORMAL

plot(X,F, type = "l",col="red",lwd=3,main="Distribución Normal")

#	PRUEBA JARQUE BERA

jb.norm.test(MUESTRA)
 


