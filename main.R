library(openxlsx)
library(purrr)
library(tidyr)
library(ggplot2)

Ruta<- ""
hojas = getSheetNames(Ruta)

Ruta2<- ""
hojas2 = getSheetNames(Ruta2)


lista_df = map(hojas,function(x){
  read.xlsx(Ruta ,sheet = x)
})

lista_df2 = map(hojas2,function(x){
  read.xlsx(Ruta2 ,sheet = x)
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


for(i in 1:length(lista_df)){
  a<-tables(lista_df[[i]])
  b<- hojas[i]
  print(b)
  print(a)
  print(chisq.test(a))
}

#Gráficos

#Reformar datos


for (i in lista_df2) {
  datos_long <- gather(i, Variable, Valor, -Año,-Trimestre, -Condicion)
  
  plot <- ggplot (data=datos_long, aes(x=factor(Condicion), y=Valor, col=factor(Año), shape=factor(Trimestre))) +
    geom_point() + labs(title = "Evolución de Condiciones por Año",
                        x = "Condicion",
                        y = "Cantidad de Mujeres", shape = "Trimestre") +
    scale_color_discrete(name = "Año") +
    facet_wrap(~Variable, scales = "free_y") +
    theme_minimal()
  
  print(plot)
  
}


#ggplot(datos_long, aes(x = factor(Condicion), y = Valor, color = as.factor(Año))) +
# geom_point() + 
#labs(title = "Evolución de Condiciones por Año",
#x = "Trimestre",
# y = "Valor") +
#scale_color_discrete(name = "Año") +
#facet_wrap(~Variable, scales = "free_y") +
#theme_minimal()



#ggplot(datos_long, aes(factor(Condicion), Valor, col = factor(Año)), shape=factor(Trimestre)) +
# geom_point(size=2) + 
#geom_smooth (method = "lm") + labs(title = "Evolución de Condiciones por Año",
#                                    x = "Condicion",
#                                   y = "Valor", shape = "Trimestre") +
#scale_color_discrete(name = "Año") +
#facet_grid(~Variable)
