#############################################################
#############Transformación inversa##########################
#############################################################
############### Exponencial (4) #############################
#############################################################
n=100000
aux=rexp(n,4)
u=runif(n)
plot(density(aux), col="red",xlim=c(0,2),ylim=c(0,4),lwd=3 , main="Transformada inversa", xlab="", ylab="")
par(new=TRUE)
plot(density(-1/4*log(1-u)), xlim=c(0,2),ylim=c(0,4), main="Transformada inversa", col="blue", lwd=3, xlab="", ylab="", lty=2)
legend("topright", legend=c("Exponencial", "T. Inversa"), col=c("red", "blue"), lty=1, cex=1.2)


#############################################################
transformacion_inversa <- function(inversa_funcion, n) {
  inversa_funcion(runif(n,0,1))
}
##Por ejemplo para simulacion fX(x)=4x^3,0<x<1 la distribucion 
##seria x^3 y la inversa 
inversa_funcion <- function(u) u^(1/4)
simulacion <- transformacion_inversa(inversa_funcion, 5e4)
head(simulacion)
#comprobamos
hist(simulacion, breaks=50, freq=FALSE, main=expression("Muestra vs densidad real de f(x)=" ~4*x^3~""), ylab = "",xlab="", las=1, col="plum")
curve(4*x^3, 0, 1, col="red", lwd=2, add=T)#Efectivamente 
