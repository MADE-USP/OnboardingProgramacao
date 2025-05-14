# Estilo imperativo
numeros <- 1:20
numeros_2 <- c()
for(i in 1:length(numeros)){
  if(numeros[i]%%2==0){
    numeros_2[i] <- numeros[i]**2
  }else{
    numeros_2[i] <- numeros[i]*2
  }
}
# Estilo funcional
funcaoNum <- function(x){
  if(x%%2==0){
    return(x**2)
  }else{
    return(x*2)
  }
}
numeros_2_func <- sapply(numeros, funcaoNum)