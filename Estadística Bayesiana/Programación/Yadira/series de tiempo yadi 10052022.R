library(forecast)
library(astsa)
library(lmtest)
library(tseries)

###################

plot(Lynx)

autoplot(Lynx)

tsdisplay(Lynx, col = "purple")

Y = as.numeric(Lynx)
X = 1:length(Lynx)

# SUPUESTOS

#Varianza constante
bptest( Y ~ X)
#Estacionario en tendenca
adf.test(Lynx)
#Varianza constante
kpss.test(Lynx)

# MODELO AUTOARIMA
modelo1 = auto.arima(Lynx)
modelo1

# INTERVALOS DE CONFIANZA
confint(modelo1)


# FITTED VALUES
ARMA_ajuste = Lynx - residuals(modelo1)

ts.plot(Lynx, lwd = 3, main = "Comparacion", ylab = "", xlab = "")
points(ARMA_ajuste, type = "l", col = "orange", lty = 2)

# NORMALIDAD PARA RUIDO BLANCO
library(nortest)
qqnorm(modelo1)




# 

checkresiduals(modelo1$residuals)

ggtsdisplay(modelo1$residuals, main = "Residuales")
tsdiag(modelo1, gof.lag = 80) # que en la prueba de LB no este ningun punto por debajo de la linea azul 

#
t.test(modelo1$residuals)
confint(modelo1)

plot(forecast(modelo1, 10), col = "blue", las = 1)




########################## JAGS
# AR1
library(rjags)
library(coda)


n = length(Lynx)

data = list(
  n = n,
  y = as.integer(Lynx)
)

inits = function(){
  
  list(
    
    alpha = rnorm(1),
    rho1 = rnorm(1),
    tau = rgamma(1,1,1)
  )
  
}


params = c("alpha","rho1", "tau", "sigma2")


modeloAR1 = "model{

for(i in 2:n){

  y[i] ~ dnorm(f[i], tau)
  f[i] = alpha + rho1*y[i-1]

}

rho1 ~ dnorm(0,0.01)
tau ~ dgamma(0.001,0.001)
alpha ~ dnorm(0,0.01)
sigma2 = 1/tau

}
"

fitAR1 = jags.model(textConnection(modeloAR1), data = data, inits = inits, n.chain = 3)

update(fitAR1, 1000)

sampleAR1 = coda.samples(fitAR1, params, n.iter = 10000)

plot(sampleAR1)

gelman.plot(sampleAR1)


############################ JAGS
# ARMA(2,1)

n = length(Lynx)

data = list(
  n = n,
  y = as.integer(Lynx)
)


modeloARMA = "model{

for(i in 3:n){

  y[i] ~ dnorm(f[i], tau)
  f[i] = alpha + rho1*y[i-1] + rho2*y[i-2] + theta1*z[i-1]

}

for(i in 1:n){
z[i] ~ dnorm(0,1/sigma2z)
}

rho1 ~ dnorm(0,0.001)
rho2 ~ dnorm(0,0.001)
theta1 ~ dnorm(0,0.001)
tau ~ dgamma(0.001,0.001)
tau_z ~ dgamma(0.001,0.001)
alpha ~ dnorm(0,0.01)
sigma2 = 1/tau
sigma2z = 1/tau_z

}
"


inits = function(){
  
  list(
    
    alpha = rnorm(1),
    rho1 = rnorm(1),
    rho2 = rnorm(1),
    theta1 = rnorm(1),
    tau = rgamma(1,1,1),
    tau_z = rgamma(1,1,1)
  )
  
}


params = c("alpha","rho1","rho2" ,"theta1")



fitARMA = jags.model(textConnection(modeloARMA), data = data, inits = inits, n.chain = 3)

update(fitARMA, 1000)

sampleARMA = coda.samples(fitARMA, params, n.iter = 10000)

plot(sampleARMA)

gelman.plot(sampleARMA)

summary(sampleARMA)
