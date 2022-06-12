library(extraDistr)
set.seed(8)
n = 20 # Número de muestras
r = 3 # Parámetro Binomial Negativa
theta = .4 # Parámetro Binomial Negativa sobre el que se encontrará distribución
alpha0 = 8 # Hiperparámetro Beta
beta0 = 6.5 # Hiperparámetro Beta
p = seq(0,1,0.01) # Probabilidades para primera gráfica
x = 0:n # valores para segunda gráfica
YY = c(0,round(n/3),round(2*n/3),n) # Numero de muestra iterativa


# Prior, Verosimilitud y Posteriori

par(mfrow=c(2,2))
for(j in 1:4){
  n = YY[j]
  
  # Muestra Binomial negativa
  X = rnbinom(n = n, size = r, prob = theta)
  
  # Priori
  Prior = dbeta(x = p, shape1 = alpha0, shape2 = beta0)
  
  # Verosimilitud
  verosimilitud = dbeta(x = p, shape1 = n*r + 1, shape2 = sum(X) + 1)
  
  # Posteriori
  Posterior = dbeta(x = p, shape1 = alpha0 + n * r, shape2 = beta0 + sum(X))
  
  
  plot(x = p, y = Prior,xlab = expression(theta), ylab = "Densidad", col = "red", type = "l", ylim = c(0,15), lty=1, lwd=4,main=paste0("Muestra N=",n)) # "Prior"
  
  lines(p,verosimilitud, col = "black", lty=2, lwd=4) # "Verosimilitud"
  
  lines(p,Posterior, col = "blue",lty=3, lwd=4) # "Posteriori"
  
  legend("topright", legend=c("Prior","Verosimilitud","Posterior"), lty=c(1,2,3), col=c("red","black","blue"), lwd=2)
}


# Predictiva Inicial y Predictiva Final

par(mfrow = c(2,2))
for(j in 1:4){
  n = YY[j]
  
  # Muestra Binomial negativa
  X = rnbinom(n = n, size = r, prob = theta)
  
  # predictiva inicial
  predict_inicial = dbnbinom(x = x, size = r, alpha = alpha0, beta = beta0)
  
  # predictiva final
  predict_final = dbnbinom(x = x, size = r, alpha = n*r + alpha0, beta = beta0 + sum(X))
  
  plot(x, predict_inicial, col = "black",ylab="Densidad",type="h",lwd=2, main=paste0("Muestra N=",n)) # "Predictiva Inicial"
  
  points(x, predict_final, col="blue",pch=19,cex=1.5) # "Predictiva Final"
  
  legend("topleft", legend=c("Predictiva Prior","Predictiva Posterior"), lty=c(1,NA), col=c("black","blue"), pch=c(NA,19))
}


# ---- 



# Muestra Binomial negativa
X = rnbinom(n = n, size = r, prob = theta)

# Priori
Prior = dbeta(x = p, shape1 = alpha0, shape2 = beta0)

# Verosimilitud
verosimilitud = dbeta(x = p, shape1 = n*r + 1, shape2 = sum(X) + 1)

# Posteriori
Posterior = dbeta(x = p, shape1 = alpha0 + n * r, shape2 = beta0 + sum(X))

# predictiva inicial
predict_inicial = dbnbinom(x = x, size = r, alpha = alpha0, beta = beta0)

# predictiva final
predict_final = dbnbinom(x = x, size = r, alpha = n*r + alpha0, beta = beta0 + sum(X))



# Prior, Verosimilitud y Posteriori

"Prior"
plot(x = p, y = Prior,xlab = expression(theta), ylab = "Densidad", col = "red", type = "l", ylim = c(0,15), lty=1, lwd=4)

"Verosimilitud"
lines(p,verosimilitud, col = "black", lty=2, lwd=4)

"Posteriori"
lines(p,Posterior, col = "blue",lty=3, lwd=4)


legend("topright", legend=c("Prior","Verosimilitud","Posterior"), lty=c(1,2,3), col=c("red","black","blue"), lwd=2)



# Predictiva Inicial y Predictiva Final

"Predictiva Inicial"
plot(x, predict_inicial, col = "black",ylab="Densidad",type="h",lwd=2)

"Predictiva Final"
points(x, predict_final, col="blue",pch=19,cex=1.5 )

legend("topleft", legend=c("Predictiva Prior","Predictiva Posterior"), lty=c(1,NA), col=c("black","blue"), pch=c(NA,19))


