# Se llama a las librerias a implementar en el código.
library(openxlsx)
library(purrr)
library(tidyr)
library(ggplot2)
library(readxl)
library(rcompanion)
library(dplyr)


# Se guarda la ruta del archivo exccel local en la varibale Ruta, se obtienen 
# los nombres de las hojas y finalmente se leen y guardan en una lista.
Ruta<- "C:/Users/svm12/Documents/Datos_proyecto_estadistica/Variables_a_usar_0_1.xlsx"
hojas = getSheetNames(Ruta)

list_of_df <- map(hojas,function(x){
  read.xlsx(Ruta ,sheet = x)
})


# Función encargada de generar las tablas a ser usadas en las funciones chisq.test
# y cramerV().
#
# @param table: Es un dataframe con la información a convertir en tabla.
# @return Retorna una tabla con el contenido del dataframe ingresado.
df_to_table <- function(table){
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
  } else {
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
  
  result <- as.table(result)
  dimnames(result) <- list(Condicion=c(0:((length(table[[1]]) / 8) -1)), colnames(table[1,4:length(table)]))
  return(result) 
}


# Esta función se encarga de generar todas las tablas requeridas para la
# investigación haciendo uso de la función ya creada df_to_table.
#
# @param dfs: Es una lista que contiene los dataframes a convertir.
# @return Retorna una lista de tablas.
dfs_to_tables <- function(dfs){
  tables <- list()
  for(i in 1:length(dfs)){
    tables[[i]] <- df_to_table(dfs[[i]])
  }
  return(tables)
}


# Se crean las tablas a utilizar en la investigación.
tables_to_use <- dfs_to_tables(list_of_df)

# Esta función corre la prueba chi-cuadrado en todas la tablas y guarda los 
# los resultados en una lista.
#
# @param tables: Es una lista de tablas.
# @return Retorna una lista con los resultados de la prueba sobre cada tabla.
chi_tests <- function(tables){
  results <- list()
  for(i in 1:length(tables)){
    results[[i]] <- chisq.test(tables[[i]])
  }
  return(results)
}


# Se guardan los resultados de la prueba chi-cuadrado sobre las tablas.
chi_test_results <- chi_tests(tables_to_use)


# Esta función se encarga de correr la prueba de la V de Cramer sobre todas las
# tablas.
#
# @param tables: Es una lista de tablas.
# @return Retorna una lista con los resultados de la prueba sobre cada tabla.
cramerv_test <- function(tables){
  results <- list()
  for(i in 1:length(tables)){
    results[[i]] <- cramerV(tables[[i]])
  }
  return(results)
}

# Se guardan los resultados de la prueba V de Cramer sobre las tablas.
cramerv_test_results <- cramerv_test(tables_to_use)

for(i in 1:length(list_of_df)){
  print(hojas[[i]])
  print(chi_test_results[[i]])
  print(cramerv_test_results[[i]])
}

df <- data.frame(cramerv_test_results)
colnames(df) <- hojas

write.xlsx(df, "resultados_cramer.xlsx")
