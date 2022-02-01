# Librerias para AR
library(tseries)

library(astsa)

library(TTR)

# Leer archivos excel 
library(readxl)

# Fijar directorio
getwd()

"Serie 1"

S1 <- read_excel("7 Tarea Series.xlsx", col_types = c("numeric"))

plot(S1$S1,type="l",main="Serie 1")

acf2(S1$S1,30, main = "Autocorrelogramas S1")  # Se trata de un modelo AR2 pos-neg

MODELO_AR2<-arima(S1$S1,order=c(2,0,0))

MODELO_AR2

# ### CON FALLA
# MODELO_AR2falla<-arima(S1$S1,order=c(1,0,0))
# MODELO_AR2falla

"Serie 2"

S2 <- read_excel("7 Tarea Series.xlsx", col_types = c("numeric"), sheet = 2)

plot(S2$S2,type="l",main="Serie 2")

acf2(S2$S2,30, main = "Autocorrelogramas S2")  # Se trata de un modelo AR2 pos-neg

MODELO_AR1<-arima(S2$S2,order=c(1,0,0))

MODELO_AR1

# ### CON FALLA
# MODELO_AR1falla<-arima(S2$S2,order=c(2,0,0))
# 
# MODELO_AR1falla


