#Estadística Bayesiana 
##Metodo de aceptacion y rechazo 
aceptacion_rechazo <- function(f, c, g, rg, n) { 
  acepta_r     <- 0
  result <- rep(NA, n)
  
  while (acepta_r < n) {
    y <- rg(1)               # Paso 1
    u <- runif(1,0,1)        # Paso 2
    if (u < f(y)/(c*g(y))) { # Paso 3
      acepta_r <- acepta_r+1
      result[acepta_r] = y
    }
  }
  
  result
}
#pasos para utilizar la función
#NO CORRE#
# #Para utilizar la funcion debemos de definir, la funcion f, la funcion g y el valor de c
# f  <- function(x)               # densidad f
# g  <- function(x)               # g(x) 
# rg <- function(n) runif(n,0,1)  # uniforme
# c  <-                           # valor de c


###Ejemplo 
f  <- function(x) 20*x*(1-x)^3     # densidad Beta
g  <- function(x) x/x           # g(x) = 1 
rg <- function(n) runif(n,0,1)  # uniforme
c  <-135/64                # maximo 
##Aplicamos el algoritmo
set.seed(140)
resultado <- aceptacion_rechazo(f, c, g, rg, 100) 
hist(resultado, breaks=30, freq=FALSE, main="Aceptacion_rechazo vs densidad real", xlim=c(0,1), ylim=c(0,2.5), xlab="", ylab="", las=1)
curve(f,0,1,n=100,col='plum',lwd=4,add=TRUE)


##Veamos lo que hace el metodo por partes 
dibujar_segmento <- function(empieza, termina) {
  segments(c(empieza,termina,termina,empieza), c(0,0,c*1.025,c*1.025), 
           c(termina,termina,empieza,empieza), c(0,c*1.025,c*1.025,0))
  n.pts <- 100
  us <- runif(n.pts, 0, 1)
  ys <- empieza + rg(n.pts)*(termina-empieza)
  accepted <- us < f(ys)/(c*g(ys))
  points(ys, c*us, col=ifelse(accepted,"green","red"), pch=18)  
}
#Aplicamos la funcion 
xs <- seq(0, 1, len=100)
curve(f,0,1,n=100,col='plum',lwd=4, ylim=c(0,3))
lines(xs, c*g(xs), type="l", col="blue", lwd=2)
legend(0.7,3,legend=c("f(x)","c*g(x)"), col=c("plum","blue"), lwd=2,lty=1:2, cex=0.8,
       box.lty=0) 
dibujar_segmento(0,.1) 
dibujar_segmento(0.40, 0.55)
dibujar_segmento(0.75, 0.85)#

