# Análisis espectral.

t<-1:128
x1 <- 2*cos(2*pi*t*6/100) + 3*sin(2*pi*t*6/100)

x2 <- 4*cos(2*pi*t*10/100) + 5*sin(2*pi*t*10/100)

x3 <- 6*cos(2*pi*t*40/100) + 7*sin(2*pi*t*40/100)

x.dat <- x1 + x2 + x3

par(mfrow=c(2,2)) # Divide la ventana gráfica en cuatro partes

plot.ts(x1, ylim=c(-16,16), main="frec=6/100")
plot.ts(x2, ylim=c(-16,16), main="frec=10/100")
plot.ts(x3, ylim=c(-16,16), main="frec=40/100")
plot.ts(x.dat, ylim=c(-16,16), main="Suma")

par(mfrow=c(1,1)) # Restaura la ventana gráfica para dibujar un sólo gráfico

#Cálculo del periodograma haciendo uso del algoritmo FFT (Fast Fourier Transform)
#fft(datos)

periodograma.x<- abs(2*fft(x.dat)/128)^2  
f<- 0:64/128
plot(f, periodograma.x[1:65], type="o", xlab="frecuencia", ylab="Periodograma")

#También con esta función 

spec.pgram(x, log="no") 
#Representación del periodograma en la escala de los datos
spec.pgram(x.dat, log="no")

t<-1:100
x1 <- 2*cos(2*pi*t*6/100) + 3*sin(2*pi*t*6/100)
x2 <- 4*cos(2*pi*t*10/100) + 5*sin(2*pi*t*10/100)
x3 <- 6*cos(2*pi*t*40/100) + 7*sin(2*pi*t*40/100)
y.dat<- x1 + x2 + x3 + rnorm(100,0,5)
plot.ts(y.dat)

f<- 0:50/100
periodograma.y<- abs(2*fft(y.dat)/100)^2  # Calcula el periodograma haciendo uso del algoritmo FFT

plot(f, periodograma.y[1:51], type="o", xlab="frecuencia", ylab="Periodograma")


