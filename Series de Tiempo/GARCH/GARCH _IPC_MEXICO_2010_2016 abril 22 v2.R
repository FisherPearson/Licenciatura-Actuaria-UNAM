#
#MODELO GARC(1,1) PARA DATOS DEL IPC BOLSA DE VALORES DE MEXICO
#
#	PROFRS. Francisco S?nchez Villarreal y Susana Barrera Ocampo
######################################################################################################################


library("FinTS")
library("forecast") 
library("fGarch") 
library("xtable") 
library("tseries")
library("astsa")

DATOS<-read.csv("C:/Users/actda/Downloads/datos nasdaq.csv",header=TRUE,dec=".",fill=TRUE) # "~/SEMESTRE 2022-2/Series de Tiempo/GARCH/IPC_MEX_2010_2016.csv"
DATOS

IPCMEX<-(DATOS)[,"nasdaq"] #,"CIERRE"

head(IPCMEX)
IPCMEX
tail(IPCMEX)

summary(IPCMEX) 

plot(IPCMEX,type="l")

acf2(IPCMEX,50)

IPCMEX<-log(IPCMEX)
IPCMEX<-diff(IPCMEX)
IPCMEX

plot(IPCMEX,type="l")


acf2(IPCMEX,50)

adf.test(IPCMEX)

"Como resultado de la prueba obtenemos que el p- value es 0.01, lo cual indica rechazo de
Ho en favor de H1. Es decir, la serie de Retornos es estacionaria, permiti?ndonos
continuar con el an?lisis."

## A partir de estos resultados se presentan algunos ejemplos de modelos arima
## recuerde que para este ejemplo a los datos originales se uso logaritmo y la primer diferencia


arima210 <- arima(IPCMEX, order=c(2, 0, 0)) 
arima210
summary(arima210)

arima012 <- arima(IPCMEX, order=c(0, 0, 2)) 
arima012
summary(arima012)

arima111 <- arima(IPCMEX, order=c(1, 0, 1)) 
arima111 
summary(arima111)

arima113 <- arima(IPCMEX, order=c(1, 0, 3)) 
arima113
summary(arima113)
sarima()

arima112 <- arima(IPCMEX, order=c(1, 0, 2)) 
arima112

MODELO2<-sarima(IPCMEX, 1,0,3, 0,0,0,12)
MODELO2

MODELO3<-sarima(IPCMEX, 1,0,2, 0,0,0,12)
MODELO3

MODELO.auto<-sarima(IPCMEX, 2,0,2, 0,0,0,12)
MODELO.auto
MEJOR = auto.arima(IPCMEX)

MODELO.auto1<-sarima(IPCMEX, 0,0,2, 0,0,0,12)
MODELO.auto1
## EN UNA TABLA SE PRESENTAN LOS RESULTADOS DE LOS AIC PARA SELECCIONAR EL QUE TENGA EL VALOR MAS PEQUE?O

aic210 <- arima210$aic 
aic012 <- arima012$aic 
aic111 <- arima111$aic 
aic113 <- arima113$aic
aic112 <- arima112$aic

nombres <- c("aic210","aic012","aic111","aic113","aic112")
aic <- as.numeric(c(aic210,aic012,aic111,aic113,aic112)) 
table <- data.frame(nombres,aic) 

table

"Vemos residuales de arima(1,1,2)"


arima112 <- arima(IPCMEX, order=c(1, 0, 2)) 
arima112
res_arima112 <- arima112$res 

plot(res_arima112,type="l")

acf2(res_arima112,50)

Box.test(coredata(res_arima112), type="Ljung-Box", lag=12) 

jarque.bera.test(res_arima112)



### se debe probar si es adecuado el uso de un modelo de volatilidad para 
#la serie de tiempo, con la funci?n ArchTest().


"Para determinar si existen efectos GARCH en la serie de residuos del modelo ARIMA,
realizamos las pruebas de hip?tesis: Ljung-Box y Multiplicador de Lagrange para efectos
GARCH. El resultado de estas pruebas, nos indicar? si es necesario realizar la estimaci?n
de un modelo GARCH."

ArchTest(IPCMEX)

"Esta prueba, confirma lo visto en la prueba anterior. Y confirma la necesidad de estimar
un modelo GARCH para serie de retornos Google, ya que el modelo ARIMA(1,1,2) no
cumple con la condici?n requerida de homocedasticidad de los errores, para ser un buen
estimador de media condicional."

res_arima112 <- arima112$res 
res_arima112_2 <- arima112$res^2 

par(mfcol=c(3, 1)) 

plot(res_arima112,main='Residuales del modelo ARIMA') # media

acf2(res_arima112,50)


plot(res_arima112_2,main='Residuales al cuadrado del modelo ARIMA') # varianza

acf2(res_arima112_2,50)

## sE MODELA CON ARCH Y GARCH  

garch11=garch(res_arima112,order=c(1,1),trace=F) 
garch11
summary(garch11)


garch22=garch(res_arima112,order=c(2,2),trace=F) 
garch22
summary(garch22)

garch23=garch(res_arima112,order=c(2,3),trace=F) 
garch23
summary(garch23)

garch12=garch(res_arima112,order=c(1,2),trace=F) 
garch12
summary(garch12)


arch02=garch(res_arima112,order=c(0,2),trace=F) 
arch02
summary(arch02)


aicgarch11=AIC(garch11)
aicgarch22=AIC(garch22) 
aicgarch23=AIC(garch23)
aicgarch12=AIC(garch12) 
aicarch02=AIC(arch02) 

 
nombres2 <-c("aicgarch11","aicgarch22","aicgarch23","aicgarch12","aicarch02") 
aic2 <- as.numeric(c(aicgarch11,aicgarch22,aicgarch23,aicgarch12,aicarch02)) 
table2 <- data.frame(nombres2,aic2) 
table2

"Comprobaci?n del modelo ajustado"

{
  plot.ts(residuals(arima112),main = 'Residuos,
Modelo en Varianza, Garch(1,1)')
  qqnorm(residuals(arima112))
  qqline(residuals(arima112))
  acf(residuals(arima112)^2,na.action = na.omit)
  pacf(residuals(arima112)^2,na.action = na.omit)
}


{
library('forecast')
fit <- fitted.values(arima112)
fitgarch <- fitted.values(garch11)[,1]
low <- fit - (1.96 * fitgarch)
high <- fit + (1.96 * fitgarch)
plot(IPCMEX, main="Retornos vs Ajustes de Retornos con Modelo
     ARIMA(1,0,2)-GARCH(1,1)",type="l")
lines(low,col = 'blue')
lines(high,col = 'blue')
lines(fit,col = 'red')
}


PRONOS<-predict(garch11)
PRONOS

plot(IPCMEX,type="l",main="Retornos VS Ajuste de Retornos con Modelo ARIMA(1,1,2)-GRACH(1,1)")
lines(PRONOS[,1],col="blue",type="l")
lines(PRONOS[,2],col="red",type="l")


