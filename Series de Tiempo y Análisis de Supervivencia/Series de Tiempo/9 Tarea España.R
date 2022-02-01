library(tidyverse)
library(lubridate)
library(car)
library(tseries)
library(astsa)
library(foreign)
library(timsac)
library(vars)
library(lmtest)
library(mFilter)
library(dynlm)
library(nlme)
library(lmtest)
library(broom)
library(kableExtra)
library(knitr)
library(MASS)
library(parallel)
library(mlogit)
library(dplyr)
library(tidyr)

library(forecast)
#install.packages('fpp2', dependencies = TRUE)
library(fpp2)



#Comando para saber cuantas diferenciaciones se requieren para un SARIMA
nsdiffs(euretail)

#Realiza la grafica de serie de tiempo
plot.ts(euretail, main="Euretail")
acf(euretail)
pacf(euretail)

#Solicitar el mejor modelo de ARIMA
#La funcion auto.arima() calcula el mejor modelo ARIMA(p, d, q) de acuerdo a diferentes criterios: AIC, AICC o BIC value.


model=auto.arima(euretail,stepwise=FALSE,approximation=FALSE) 

#Resumen del Modelo SARIMA
summary(model)
residuals=resid(model)
plot(residuals, main="Residuals", col="Blue")
adf.test(residuals)

#SARIMA(0,1,3)(0,1,1)[4] 

#Pronóstico del modelo
forecast(model,h=10)

ggseasonplot(euretail, main="Plot SARIMA") 

############################# TAREA ESPAÑA

setwd("C:/Users/actda/OneDrive/Documentos/SEMESTRE 2022-1/SERIES DE TIEMPO/Series de Tiempo")

library(readxl)
df <- read_excel("CONSUMO ENERGIA ESPAÑA 2021.xlsx", sheet = 2)

# Convertir la serie de tiempo a clase ts
attach(df)
df.ts = ts(Energia, start = c(1967,1), frequency = 12)

# Serie sin ajustes
plot(df.ts)

#Serie con ajuste logaritmico para reducir varianza
dflog = log(df.ts)
plot(dflog)

# Para proponer un modelo ARIMA, se necesita que la serie de tiempo sea
# estacionaria; media y varianza constantes. Claramente la serie de tiempo
# no es estacionaria. 

# El test de Dickey-Fuller nos ayuda a saber cuando una serie es estacionaria
# o no, teniendo: H0: No es estacionaria (dependencia del tiempo) y Ha: Estacionaria
# (No dependencia del tiempo)

# ¿Es la serie original (sin log) estacionaria?
adf.test(dflog, alternative = "stationary")#?

acf2(dflog)

# Tomare la primera diferencia para volver la serie estacionaria

LOG_D1<-diff(dflog,lag=1,differences=1)
LOG_D1

plot(LOG_D1,type="l",col="blue",lwd=2)


# Se pretende quitar la estacionalidad de la serie (7 meses)
LOG_D1D12<-diff(LOG_D1,lag=12,differences=1)

LOG_D1D12

plot(LOG_D1D12,main="Serie con D1 y D12",lwd=2)


acf2(LOG_D1D12)


### AUTO ARIMA
model=auto.arima(LOG_D1D12,stepwise=FALSE,approximation=FALSE) 
summary(model)

plot(forecast(model,h=12))

RONOSTICO<-predict(model,n.ahead=12)

PRONOSTICO

plot(PRONOSTICO$pred,type="l")


EXP_PRON<-exp(PRONOSTICO$pred)

EXP_PRON

plot(EXP_PRON)



AJUSTADOS<-df.ts-exp(model$residuals)
AJUSTADOS


plot(AJUSTADOS,type="l",col="blue",lwd=3)
