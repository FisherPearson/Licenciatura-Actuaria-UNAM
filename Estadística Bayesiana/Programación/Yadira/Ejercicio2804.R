library(rjags)
library(coda)
x = c(1, 2, 3, 4, 5, 6)
y = c(1, 3, 3, 3, 5, 7)

######################## REGRESION  
reg = lm(y~x)
summary(reg)

## Intervalos de confianza
confint(reg, level = 0.95)
confint(reg, level = 0.99)

# Prueba F
anova(reg)

sqrt(0.7048) # Error estandar del modelo


######################### JAGS

ecuacion = reg$coefficients[1] + reg$coefficients[2] * x

eivar = 0.7048
n = 6 #numero de datos

# Hiperparametros
data = list(
  y = y,
  x = x,
  n = n,
  zeros = c(0,0),
  diagonal = diag(1/100,2)
)

parametros = c("Beta","Sigma2")

inicial = function(){
  list(
    "Beta" = rnorm(2,0,1),
    "Tau" = rgamma(1,1,1)
  )
}

ruta = "model.bug" #alt+shift+h



fit = jags.model(ruta, data, inicial, n.chains = 4)

update(fit, 1000)

muestreo = coda.samples(fit,parametros, n.iter = 4000, thin = 1)

plot(muestreo)

summary(muestreo)

####################################

data("birthwt")
data2 = birthwt

reg1 = lm(data2$bwt ~ data2$lwt)

n = nrow(data2$bwt)

