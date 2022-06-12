library(HDInterval)

HDInterval::hdi(qnorm,.95,mean = 0,sd = 1)


# Muestra Normal(5,9)
theta = 5
var = 3
n = 10
datos = rnorm(n,theta,var)
mean = mean(datos)

# Prior Normal(2,1)
mu0 = 2
tao0 = 1

# Posterior
mu1 = (1/tao0 + n/var)^(-1) * (mu0/tao0 + n*mean/var)
tao1 = (1/tao0 + n/var)^(-1)


# FUNCION
HDInterval::hdi(qnorm,.95,mean = mu1,sd = sqrt(tao1))

# CONSTRUCCION
mu1 - qnorm(p = 1-0.05/2) * sqrt(tao1)

mu1 + qnorm(p = 1-0.05/2) * sqrt(tao1)

# --- 

# Posterior final
tao_final = var + tao1

# FUNCION
HDInterval::hdi(qnorm,.95,mean = mu1,sd = sqrt(tao_final))

# CONSTRUCCION
mu1 - qnorm(p = 1-0.05/2) * sqrt(tao_final)

mu1 + qnorm(p = 1-0.05/2) * sqrt(tao_final)


# ---

tao0 = 1000000000000

# Posterior
mu1 = (1/tao0 + n/var)^(-1) * (mu0/tao0 + n*mean/var)
tao1 = (1/tao0 + n/var)^(-1)


# FUNCION
HDInterval::hdi(qnorm,.95,mean = mu1,sd = sqrt(tao1))

# CONSTRUCCION
mu1 - qnorm(p = 1-0.05/2) * sqrt(var)/sqrt(n)

mu1 + qnorm(p = 1-0.05/2) * sqrt(var)/sqrt(n)

# ---

# Posterior final
tao_final = var + tao1

# FUNCION
HDInterval::hdi(qnorm,.95,mean = mu1,sd = sqrt(tao_final))

# CONSTRUCCION
mu1 - qnorm(p = 1-0.05/2) * sqrt(var)/sqrt(n) * sqrt(n + 1)

mu1 + qnorm(p = 1-0.05/2) * sqrt(var)/sqrt(n) * sqrt(n + 1)

