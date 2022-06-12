#Estadística Bayesiana 
#Simulación estocastica
##Bien definidos los limites 
library(beepr)

aux=NULL
montecarlo1=function(g,a,b,n)
{
  t <- proc.time()##Veamos el costo computacional
  x=runif(n)
  rango=seq(from=1, to=n, by=1)
  valoresg=g((b-a)*x+a)
  s=c()
  for (i in rango) {
    s[i]=(b-a)*sum(valoresg[1:i])/i
  }
  s
  plot(s, type="l",xlab="n",ylab="Estimación integral",main="Monte Carlo para integrales de una variable",col="purple", lwd=2)
  numerico=integrate(g, lower = a, upper = b)
  lines(c(1,n),c(numerico[1],numerico[1]),col="green",lwd=3)
  Resultado_real=numerico$value
  aux=mean(s)
  cat("El valor real de la integral es: ", Resultado_real, "Mientras que el simulado
      Monte Carlo es:", aux, "\n")
  proc.time()-t 
  beep(2)
}
##PASOS PARA UTILIZAR LA FUNCION 
#NO CORRE
# g=function(x)       #funcion 
# montecarlo1(g,a,b,n)#g es la funcion que acabamos de definir
#                     # a es el limite inferior
#                     #b el limite superior
#                     #n es el número de simulaciones
###Veamos un ejemplo para ver como cambia el valor
#al cambiar el numero de iteraciones y el costo 
#computacional comparando como se acerca el valor real
# y el simulado 
g=function(x) exp(-x)
montecarlo1(g,-10,25,10)
montecarlo1(g,-10,25,100)
montecarlo1(g,-10,25,1000)
montecarlo1(g,-10,25,10000)
montecarlo1(g,-10,25,40000)
#montecarlo1(g,-10,25,100000)#
##Otro ejemplo, ahora con la normal
g=function(x) (1/sqrt(2*pi))*exp(-(x^2)/2)
montecarlo1(g,0,25,100)
montecarlo1(g,0,25,1000)



#############################################################




##sqrt(tangente) ....
g=function(x) sqrt(tan(x))
montecarlo1(g,0,pi/2,100)
montecarlo1(g,0,pi/2,1000)
montecarlo1(g,0,pi/2,10000)
montecarlo1(g,0,pi/2,100000)




#############################################################


###Veamos lo que hace el algoritmo  paso a paso 
n = 5000
x1 = runif(n, min =0 , max =1 )
y1 = runif(n, min =0 , max =1 )
plot(x1,y1,col='purple',pch=2 , main="Integracion Monte Carlo")
f <- function(x) ((sin(10*x^2))^2*sin(x))*x+0.1#por ejemplo
curve(f,0,1,n=100,col='red',lwd=5,add=TRUE)
##Lo que debemos estimar son los puntos que caen debajo de la
#curva propuesta 

#############################################################

###Entonces los vamos a pintar de diferente color y agregar 
#la curva
n = 50
x1 = runif(n, min =0 , max =1 )
y1 = runif(n, min =0 , max =1 )
f1 <- ((sin(10*x1^2))^2*sin(x1))*x1+0.1
plot(x1[(y1 > f1)],y1[(y1 > f1)],col='green',pch=2, main="Integracion Monte Carlo")
points(x1[(y1 <= f1)],y1[(y1 <= f1)],col='purple',pch=6)
f <- function(x) ((sin(10*x^2))^2*sin(x))*x+0.1
curve(f,0,1,n=100,col='red',lwd=4,add=TRUE)
#Contamos los puntos
area_1 = length(y1[(y1>f1)])
area_2 = length(y1[(y1<=f1)])
# Damos el resultado
aux<-integrate(f,0,1)
cat("El valor de la integral es: ", aux$value, "Mientras que el 
    simulado Monte Carlo es", area_2/n)

#############################################################

n = 500
x1 = runif(n, min =0 , max =1 )
y1 = runif(n, min =0 , max =1 )
f1 <- ((sin(10*x1^2))^2*sin(x1))*x1+0.1
plot(x1[(y1 > f1)],y1[(y1 > f1)],col='green',pch=2, main="Integracion Monte Carlo")
points(x1[(y1 <= f1)],y1[(y1 <= f1)],col='purple',pch=6)
f <- function(x) ((sin(10*x^2))^2*sin(x))*x+0.1
curve(f,0,1,n=100,col='red',lwd=4,add=TRUE)
#Contamos los puntos
area_1 = length(y1[(y1>f1)])
area_2 = length(y1[(y1<=f1)])
# Damos el resultado
aux<-integrate(f,0,1)
cat("El valor de la integral es: ", aux$value, "Mientras que el 
    simulado Monte Carlo es", area_2/n)


#############################################################

n = 1000
x1 = runif(n, min =0 , max =1 )
y1 = runif(n, min =0 , max =1 )
f1 <- ((sin(10*x1^2))^2*sin(x1))*x1+0.1
plot(x1[(y1 > f1)],y1[(y1 > f1)],col='green',pch=2, main="Integracion Monte Carlo")
points(x1[(y1 <= f1)],y1[(y1 <= f1)],col='purple',pch=6)
f <- function(x) ((sin(10*x^2))^2*sin(x))*x+0.1
curve(f,0,1,n=100,col='red',lwd=4,add=TRUE)
area_2 = length(y1[(y1<=f1)])
aux<-integrate(f,0,1)
cat("El valor de la integral es: ", aux$value, "Mientras que el 
    simulado Monte Carlo es", area_2/n)


#############################################################

n = 10000
x1 = runif(n, min =0 , max =1 )
y1 = runif(n, min =0 , max =1 )
f1 <- ((sin(10*x1^2))^2*sin(x1))*x1+0.1
plot(x1[(y1 > f1)],y1[(y1 > f1)],col='green',pch=2, main="Integracion Monte Carlo")
points(x1[(y1 <= f1)],y1[(y1 <= f1)],col='purple',pch=6)
f <- function(x) ((sin(10*x^2))^2*sin(x))*x+0.1
curve(f,0,1,n=100,col='red',lwd=4,add=TRUE)
area_2 = length(y1[(y1<=f1)])
aux<-integrate(f,0,1)
cat("El valor de la integral es: ", aux$value, "Mientras que el 
    simulado Monte Carlo es", area_2/n)


