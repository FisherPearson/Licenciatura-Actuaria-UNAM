library(bayess)
library(corrplot)
library(polycor)
library(glm2)
library(rjags)
library(coda)

set.seed(8)

data("bank")
attach(bank)

############################# FRECUENTISTA

# Resumen de los datos
summary(bank)

# na's
table(is.na(bank)) # No hay Nas

# Proporción de variables respuesta
table(y) #regP1 = glm(y ~., data = bank, family = binomial(link = "probit"))
summary(regP1) #Hay 50% / 50%

# Correlograma
cor_tabl = hetcor(bank)
corrplot(cor_tabl$correlations)



regP2 = glm(y ~ x1 + x3 + x4, data = bank, family = binomial(link = "probit"))
summary(regP2)

regP3 = glm(y ~ x3 + x4, data = bank, family = binomial(link = "probit"))
summary(regP3)

regP4 = glm(y ~ x4, data = bank, family = binomial(link = "probit"))
summary(regP4)


# Intervalos de confianza 
confint(regP3)

predicted_frequentist = ifelse(regP3$fitted.values >= 0.5, 1, 0)

# Matriz de confusión
table(y, predicted_frequentist)



########################### JAGS (PRIMERA PARTE)
n = length(y)


data = list(
  y = y,
  x3 = x3,
  x4 = x4,
  n = n
)

params = c("Constante", "Beta1", "Beta2")

inits = function(){list(
  "Constante" = rnorm(1),
  "Beta1" = rnorm(1),
  "Beta2" = rnorm(1)
)
}






## MODELO CON INFO FRECUENTISTA


modelo = "model{

#### LIKELIHOOD

for(i in 1:n){

eta[i] = Constante + Beta1 * x3[i] + Beta2 * x4[i]
probit(p[i]) = eta[i]
y[i] ~dbern(p[i])

}

#### PRIORS

Beta1 ~ dnorm(1.6579,1) #(1.5,1) # Con uno funciona bien en todas
Beta2 ~ dnorm(1.1317,1) #(1.5,1)
Constante ~ dnorm(-225.9755,1) #(-220,1) # -225.9755


}
"

fit = jags.model(file = textConnection(modelo), data = data, inits = inits, n.chains = 3 )

update(fit, 6000)

sample = coda.samples(fit, params, n.iter = 20000, thin = 1)

plot(sample)

summary(sample)

# Convergencia
gelman.plot(sample) # Todas convergen

# Evaluación del modelo

xj1 = cbind(rep(1,n),x3,x4)
aux_j1 = colMeans(do.call(rbind,sample))
aux_params_j1 = c(aux_j1[3],aux_j1[1:2])
yhat = drop(xj1 %*% aux_params_j1)
probas_j1 = pnorm(yhat) 

# Matriz de confusión
jags.aux.j1 = ifelse(probas_j1 >= 0.5,1,0)

table(y, jags.aux.j1) # Igual que el frecuentista













## MODELO 0,1



modelo1 = "model{

#### LIKELIHOOD

for(i in 1:n){

eta[i] = Constante + Beta1 * x3[i] + Beta2 * x4[i]
#probit(p[i]) = eta[i]
p[i] = phi(eta[i])
y[i] ~dbern(p[i])

}

#### PRIORS

Beta1 ~ dnorm(0,1) #dnorm(1.6579,1) #(1.5,1) # Con uno funciona bien en todas
Beta2 ~ dnorm(0,1) #dnorm(1.1317,1) #(1.5,1)
Constante ~ dnorm(0,1) #dnorm(-225.9755,1) #(-220,1) # -225.9755


}
"

fit1 = jags.model(file = textConnection(modelo1), data = data, inits = inits, n.chains = 3 )

update(fit1, 30000)

sample1 = coda.samples(fit1, params, n.iter = 20000, thin = 1)

plot(sample1)

summary(sample1)

# Convergencia
gelman.plot(sample1) # Todas convergen

# Evaluación del modelo

xj1 = cbind(rep(1,n),x3,x4)
aux_j1 = colMeans(do.call(rbind,sample1))
aux_params_j1 = c(aux_j1[3],aux_j1[1:2])
yhat = drop(xj1 %*% aux_params_j1)
probas_j1 = pnorm(yhat) 

# Matriz de confusión
jags.aux.j1 = ifelse(probas_j1 >= 0.5,1,0)

table(y, jags.aux.j1) # Igual que el frecuentista



























########################### JAGS (SEGUNDA PARTE)
inits2 = function(){list(
  "Constante" = rnorm(1),
  "Beta1" = rnorm(1),
  "Beta2" = rnorm(1),
  "z" = rep(0,n)
)}


modelo2 = "model{

#### LIKELIHOOD

for(i in 1:n){

eta[i] = Constante + Beta1 * x3[i] + Beta2 * x4[i]
z[i] ~ dnorm(eta[i], 1)
p[i] = step(z[i]) * 0.99999999
y[i] ~ dbern(p[i])

}

#### PRIORS

Beta1 ~ dnorm(0,1) # Con uno funciona bien en todas
Beta2 ~ dnorm(0,1)
Constante ~ dnorm(0,1) # -225.9755


}
"

fit2 = jags.model(file = textConnection(modelo2), data = data, inits = inits2, n.chains = 3 )

update(fit2, 7000)

sample2 = coda.samples(fit2, params, n.iter = 15000, thin = 1)


plot(sample2)

summary(sample2)

# Convergencia
gelman.plot(sample2) # Todas convergen

# Evaluación del modelo

xj2 = cbind(rep(1,n),x3,x4)
aux_j2 = colMeans(do.call(rbind,sample2))
aux_params_j2 = c(aux_j2[3],aux_j2[1:2])
yhat2 = drop(xj2 %*% aux_params_j2)
probas_j2 = pnorm(yhat2) 

# Matriz de confusión
jags.aux.j2 = ifelse(probas_j2 >= 0.5,1,0)

table(y, jags.aux.j2) # Igual que el frecuentista

