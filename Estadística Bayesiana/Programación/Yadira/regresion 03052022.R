library(MASS)
library(lmtest)
library(rjags)
library(coda)
library(nortest)

data("airquality")

base = airquality

table(is.na(airquality)) # > 5% datos faltantes

base = na.omit(airquality)

cor(base)
attach(base)
summary(lm(Ozone ~., data = base)) # Quitar Solar.R

reg1 = lm(Ozone ~ Wind + Temp, data = base)
summary(reg1) #Residual standard error: 21.73 estimador insesgado de la varianza

confint(reg1) # Intervalos de confianza

hist(reg1$residuals, main = "Frecuentista") # Histograma residuales

# NORMALIDAD
ad.test(reg1$residuals)
cvm.test(reg1$residuals)

# Heterocedasticidad
bptest(reg1)

# Autocorrelacion
dwtest(reg1, alternative = "two.sided")


#################################### JAGS

# Parametros
n = length(base$Ozone)
y = base$Ozone
x = base$Wind
x2 = base$Temp


data = list(
  y = y,
  x1 = x,
  x2 = x2,
  n = n,
  zeros = c(0,0,0), # 3 parametros b0, b1, b2
  diagonal = diag(1/1000,3) # varianza covarianzas 
)

params = c("Beta", "Sigma2")

inits = function(){list(
  "Beta" = rnorm(3,0,1),
  "Tau" = rgamma(1,1,1)
)
}

fit = jags.model("regresion 03052022.bug", data,inits, n.chains = 4)

update(fit,1000)

sample = coda.samples(fit, params, n.iter = 4000)

plot(sample)

gelman.plot(sample) # Convergencia

summary(sample)

## Residuales

auxX = cbind(rep(1.0,111),base$Wind, base$Temp)

simulaciones = do.call(rbind,sample)

parametros = colMeans(simulaciones)

yhat1 = drop(auxX%*%parametros[1:3])

residuales = data$y - yhat1
hist(residuales, main = "Bayesiano", col = "red")

