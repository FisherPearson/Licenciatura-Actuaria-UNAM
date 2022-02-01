#Funciones Especiales : Interpolador de Newton
M_Dif=function(xi,fxi){
  r1=length(xi)
  r2=length(fxi)
  
  if(r1!=r2){
    print("el metodo no se puede aplicar")
    break
  } else {mat=matrix(0,r1,r2)
  for (i in 1:r1){
    mat[i,1]=fxi[i]
  }
  for(i in 2:r1){
    for (j  in 2:i){
      mat[i,j]=(mat[i,(j-1)]-mat[(i-1),(j-1)])/(xi[i]-xi[i-(j-1)])
      
    }
  }
  return(mat)}
}
I_N=function(A,xi,r){
  a=dim(A)
  a1=a[1]
  a2=a[2]
  
  v=rep(0,a1)
  for(i in 2:a1){
    prod=1
    for(j in 1:(i-1)){
      prod=(prod*(r-xi[j]))
    }
    v[(i-1)]=prod
  }
  
  inter=A[1,1]
  for(i in 2:a1){
    inter=inter+(A[i,i])*v[(i-1)]
    
  }
  return(inter)
}


#Fijar Directorio
setwd("C:/Users/actda/OneDrive/Documentos/SEMESTRE 2021-2/Análisis Numérico/ProyectoFinal")

#eliminar notacion cientifica
options(scipen = 999)

#Cargar datos

"Informacion del año 2010 al 2020, por periodos anuales."
life_ex = read.csv("lif_ex.csv")


"Informacion del año 1994 al 2019, por periodos anuales."
sobremorta = read.csv("Sobremortalidad.csv")


"Interpolador para la esperanza de vida"

# Cambiar el nombre de las columnas.
colnames(life_ex) = c("anos","esperanza")

# Seleccionar años pares.
life_ex_par = life_ex[seq(1,11,2),c(1,2)]
life_ex_impar = life_ex[seq(2,10,2),c(1,2)]

#Implementar modelo de Newton en valores pares.
MD_life_ex = M_Dif(life_ex_par$anos,life_ex_par$esperanza)

#Calcular cada valor estimado y guardarlo en life_ex_impar como columna
estimaciones_life_ex = c()
for(i in life_ex_impar$anos){
  estimaciones_life_ex = c(estimaciones_life_ex,I_N(MD_life_ex,life_ex_par$anos,i))
}

#Agregar columna de las estimaciones.
life_ex_impar$Estimaciones = estimaciones_life_ex

#Agregar columna de error relativo.
life_ex_impar$Error_rel = abs(life_ex_impar$esperanza- life_ex_impar$Estimaciones)


"Interpolador para la sobremortalidad"

# Seleccionar años pares.
sobremorta_par = sobremorta[seq(1,26,2),c(1,2)]
sobremorta_impar = sobremorta[seq(2,24,2),c(1,2)]

#Implementar modelo de Newton en valores pares.
MD_sobremorta = M_Dif(sobremorta_par$Ano,sobremorta_par$Sobremortalidad)

#Calcular cada valor estimado y guardarlo en sobremorta_impar como columna
estimaciones_sobremorta = c()
for(i in sobremorta_impar$Ano){
  estimaciones_sobremorta = c(estimaciones_sobremorta,I_N(MD_sobremorta,sobremorta_par$Ano,i))
}

#Agregar columna de las estimaciones.
sobremorta_impar$Estimaciones = estimaciones_sobremorta

#Agregar columna de error relativo.
sobremorta_impar$Error_rel = abs(sobremorta_impar$Sobremortalidad- sobremorta_impar$Estimaciones)




