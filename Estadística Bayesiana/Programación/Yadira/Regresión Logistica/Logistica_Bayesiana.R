##################### Regresión logística: Frecuentista
# Base
datos = read.csv("Datoslog.csv")

# NA
table(is.na(datos))

# Table de contigencia
xtabs(~admit+rank, data = datos)

table(datos$admit) #Sesgo hacia no aceptados

# Correlograma
library(corrplot)
library(polycor)

cor_tabl = hetcor(datos)
corrplot(cor_tabl$correlations)


# Modelo logistico

library(glm2)
regL = glm(admit ~ ., data = datos, family = binomial(link = "logit"))

summary(regL)

# Intervalo de confianza
confint(regL)

# Arreglo para clasificar si o no dependiendo del umbral
predi_aux = ifelse(regL$fitted.values >= 0.5,1,0)

# Matriz de confusión
table(datos$admit, predi_aux)


################## Regresión Logistica: Bayesiano

library(rjags)
library(coda)

attach(datos)

n = length(admit)

data = list(
  y = admit,
  x1 = gre,
  x2 = gpa,
  x3 = rank,
  n = n
)

params = c("alpha","Beta1","Beta2","Beta3")

inits = function(){list(
  "alpha" = rnorm(1), 
  "Beta1" = rnorm(1),
  "Beta2" = rnorm(1),
  "Beta3" = rnorm(1)
  )
}

modelo = "model{
for(i in 1:n){
y[i] ~ dbern(p[i])
p[i] = 1/(1.0001 + exp(-(alpha + Beta1 * x1[i] + Beta2 * x2[i] + Beta3 * x3[i]))) 

}


alpha ~ dnorm(0,1)
Beta1 ~ dnorm(0,1)
Beta2 ~ dnorm(0,1)
Beta3 ~ dnorm(0,1)

}
"

fit = jags.model(textConnection(modelo), data, inits, n.chain = 3)

update(fit,3000)

sample = coda.samples(fit, params, n.iter = 4000, thin = 1)

plot(sample)

gelman.plot(sample)

summary(sample)

x = cbind(rep(1,n), gre,gpa,rank)
aux_4 = do.call(rbind, sample)
parametros = colMeans(aux_4)

aux_para = c(parametros[4],parametros[1:3])

yhat = drop(x%*%aux_para)

probas = 1 / (1 + exp(-(yhat)))

jags.aux = ifelse(probas >= .5,1,0)
table(datos$admit,jags.aux)

plot(ecdf(probas))

plot(ecdf(aux_4[,4]), main = "Beta3")

