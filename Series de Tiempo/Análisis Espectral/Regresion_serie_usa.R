library(readxl)
library(dplyr)
library(magrittr)
library(caret)
library(leaps)
library(MASS)
df = read.csv("serie_usa.csv")


Y = df$ï..Datos
X = df %>% select(C1:S45)

model = lm(ï..Datos ~., data =  df)
summary(model)

anova.model = aov(ï..Datos ~., data =  df)
summary(anova.model)

step.model = stepAIC(model, direction = "both", trace = F)
