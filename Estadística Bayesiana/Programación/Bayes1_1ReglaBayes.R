##########
### Regla deBayes

### Una muestra
PBA <- function(p){
  (0.99*p)/(0.99*p+0.01*(1-p))
}
p = c(0.0001,0.001,0.01,0.1,0.5)
p1 = PBA(p)

### Dos muestras
PBA2 <- function(p){
  (0.99*0.99*p)/(0.99*0.99*p+0.01*0.01*(1-p))
}
p2 = PBA(p1)
PBA2(p)

### Tres muestras
PBA3 <- function(p){
  (0.99*0.99*0.99*p)/(0.99*0.99*0.99*p+0.01*0.01*0.01*(1-p))
}
PBA(p2)
PBA3(p)

