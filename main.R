library(openxlsx)
library(purrr)

Ruta<- ""
hojas = getSheetNames(Ruta)

lista_df = map(hojas,function(x){
  read.xlsx(Ruta ,sheet = x)
})

#Funciones

tables<-function(table){
  condiciones <- (length(table[[1]]) / 8)
  result<-matrix(0,nrow = condiciones , ncol = (length(table) - 3))
  
  if(condiciones == 3 ){
    for(i in 1:length(table[[1]])){
      for(j in 1:(length(table) - 3)){
        if(table[[3]][i] == 0){
          result[1,j] = result[1,j] + table[[j+3]][i]
        } else if(table[i,3] == 1){
          result[2,j] = result[2,j] + table[[j+3]][i]
        } else {
          result[3,j] = result[3,j] + table[[j+3]][i]
        }
      }
    }
  }else {
    for(i in 1:length(table[[1]])){
      for(j in 1:(length(table) - 3)){
        if(table[[3]][i] == 0){
          result[1,j] = result[1,j] + table[[j+3]][i]
        } else{
          result[2,j] = result[2,j] + table[[j+3]][i]
        } 
      }
    }
  }
  
  result<-as.table(result)
  dimnames(result) <- list(Condicion=c(0:((length(table[[1]]) / 8) -1)), colnames(table[1,4:length(table)]))
  return(result) 
}


# for(i in lista_df){
#   a<-tables(i)
#   print(a)
#   print(chisq.test(a))
# }
