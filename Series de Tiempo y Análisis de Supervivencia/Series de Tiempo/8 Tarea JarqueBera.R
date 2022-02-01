# Librerias
library(readxl)
library(normtest)
library(moments)

# Importar series
serie1 <- read_excel("SEMESTRE 2022-1/SERIES DE TIEMPO/Series de Tiempo/JARQUE BERA 2021.xlsx", 
                               sheet = "serie1")
serie2 <- read_excel("SEMESTRE 2022-1/SERIES DE TIEMPO/Series de Tiempo/JARQUE BERA 2021.xlsx", 
                     sheet = "serie2")

# JB serie1
jarque.test(serie1$Valores)

# JB serie2
jarque.test(serie2$Valores)


