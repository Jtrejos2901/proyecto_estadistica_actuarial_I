# Se llama a las librerias a implementar en el código.
library(openxlsx)
library(purrr)
library(tidyr)
library(ggplot2)
library(ggmosaic)
library(readxl)
library(rcompanion)
library(dplyr)
library(DescTools)


# Se guarda la ruta del archivo exccel local en la varibale Ruta, se obtienen 
# los nombres de las hojas y finalmente se leen y guardan en una lista.
Ruta<- "Variables_a_usar_letras.xlsx"
hojas = getSheetNames(Ruta)

Ruta2 <- "Variables_utilizadas_con_porcentajes.xlsx"
hojas2 = getSheetNames(Ruta2)

list_of_df <- map(hojas,function(x){
  read.xlsx(Ruta ,sheet = x)
})

list_of_df2 = map(hojas2,function(x){
  read.xlsx(Ruta2 ,sheet = x)
})

# Función encargada de verificar el tipo de condición en términos de 0, 1 y 2.
#
# @param condition: Es una variable que puede ser un valor entero o un string 
#                   con el tipo de condición.
# @return Retorna un entero con el tipo de condición en términos de 0, 1 y 2.
type_c <- function(condition){
  if(condition == 0 | condition == "Sin hijos" | condition == "Sin pareja" | 
     condition == "0 a 5"){
    return(0)
  } else if(condition == 1 | condition == "Con hijos" | 
            condition == "Con pareja" | condition == "6 a 14"){
    return(1)
  } else {
    return(2)
  }
  
}

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
        if(type_c(table[i,3]) == 0){
          result[1,j] = result[1,j] + table[[j+3]][i]
        } else if(type_c(table[i,3]) == 1){
          result[2,j] = result[2,j] + table[[j+3]][i]
        } else {
          result[3,j] = result[3,j] + table[[j+3]][i]
        }
      }
    }
  } else {
    for(i in 1:length(table[[1]])){
      for(j in 1:(length(table) - 3)){
        if(type_c(table[[3]][i]) == 1){
          result[1,j] = result[1,j] + table[[j+3]][i]
        } else{
          result[2,j] = result[2,j] + table[[j+3]][i]
        } 
      }
    }
  }
  
  result <- as.table(result)
  if(condiciones == 3){
    x1 <- table[1,3]
    x2 <- table[2,3]
    x3 <- table[3,3]
    dimnames(result) <- list(Condicion=c(x1, x2, x3), colnames(table[1,4:length(table)]))
  } else {
    x1 <- table[1,3]
    x2 <- table[2,3]
    dimnames(result) <- list(Condicion=c(x1, x2), colnames(table[1,4:length(table)]))
  }
  
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
    results[[i]] <- chisq.test(tables[[i]])$p.value
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


# Esta función se encarga de correr la prueba del coeficiente de contingencia 
# sobre todas las tablas.
#
# @param tables: Es una lista de tablas.
# @return Retorna una lista con los resultados de la prueba sobre cada tabla.
coefinciet_test <- function(tables){
  results <- list()
  for(i in 1:length(tables)){
    chi <- chisq.test(tables[[i]])
    results[[i]] <- unname(sqrt(chi$statistic / (chi$statistic + sum(tables[[i]]))))
    
  }
  return(results)
}

# Se guardan los resultados de la prueba del coeficiente de contingencia sobre 
# las tablas.
coefinciet_results <- coefinciet_test(tables_to_use)


# Se unen los resultados de todos los tests realizados.
results <- cbind(chi_test_results, unname(unlist(cramerv_test_results)), 
                 coefinciet_results)

# Se crea un data frame con los tests realizados y sus resultados sobre cada
# variable.
row_names <- c("Nivel educativo de las mujeres desocupadas según la tenencia de hijos", 
               "Nivel educativo de las mujeres ocupadas según la tenencia de hijos", 
               "Nivel educativo de las mujeres de acuerdo al rango de esdad de los hijos", 
               "Jornada de trabajo de las madres de acuerdo a la tenencia de pareja", 
               "Jornada de trabajo de las madres de acuerdo al rango de edad de los hijos", 
               "Jornada de trabajo de las mujeres ocupadas de acuerdo a la tenencia de hijos", 
               "La edad de las mujeres desocupadas de acuerdo a la tenencia de hijos", 
               "La edad de las mujeres ocupadas de acuerdo a la tenencia de hijos", 
               "La edad de las mujeres de acuerdo con el rango edad de los hijos", 
               "Calificación del grupo ocupacional de acuerdo a la tenencia de hijos", 
               "Calificación del grupo ocupacional de acuerdo al rango de edad de los hijos", 
               "Formalidad del trabajo de acuerdo a la tenencia de hijos", 
               "Formalidad del trabajo de acuerdo al rango de edad de los hijos", 
               "Experiencia laboral de las mujeres desocupadas de acuerdo a la tenencia de hijos")

df <- data.frame(results)
colnames(df) <- c("Chi-cuadrado", "V de cramer", "Coeficiente de contingencia")
rownames(df) <- row_names


# Esta función se encarga de transformar las tablas de contingencia para porder
# generar los gráficos de barras en términos procentuales.
#
# @param tables: Es una lista con las tablas de contingencia.
# @return: Retorna las tablas modificadas.
tables_for_barplots <- function(tables){
  results <- list()
  for(i in 1:length(tables)){
    results[[i]] <- prop.table(tables[[i]], margin = 1) 
  }
  return(results)
}

tables_barplots <- tables_for_barplots(tables_to_use)


#Gráficos de puntos.
for (i in list_of_df2) {
  colnames(i) <- gsub("[.]", " ", colnames(i))
  datos_long <- gather(i, Variable, Valor, -(Año:Total)) 
  cols1 <- c("#FFA07A", "#528B8B", "#8B668B", "#8B0A50") 
  
  plot <- ggplot (data=datos_long, aes(x=factor(Condición), y = Valor, col= factor(Año), shape=factor(Trimestre))) + 
    geom_point() + labs( x = "Condición", y = "Porcentaje de Mujeres", col = "Año", shape = "Trimestre") + 
    scale_color_manual(values = cols1) + 
    facet_wrap(~Variable, scales = "free_y") + 
    theme_minimal() 
  
  print(plot) 
} 


#Gráficos de barras porcentuales.
for (i in 1:length(tables_barplots)) { 
  datos<- tables_barplots[[i]] 
  cols1 <- c("#FFA07A", "#528B8B", "#8B668B", "#8B0A50") 
  cols2 <- c("#9932CC", "#B22222", "#698B22", "#CD6889") 
  colnames(datos) <- gsub("[.]", " ", colnames(datos)) 
  dataframe<- as.data.frame(datos) 
  
  plot <- ggplot(dataframe, aes(x = Var2, y = Freq, fill= Condicion ) ) + 
    labs(x = "Variable", y = "Porcentaje de Mujeres") + 
    geom_bar(width = 0.9, stat = "identity", position = position_dodge()) + 
    scale_fill_manual(values = cols1, "Condición") + 
    theme_minimal()
  print(plot)
} 

#Colores para lo gráficos de bigotes.
colores_personalizados <- c("Sin pareja" = "#FFA07A", "Con pareja" = "#528B8B",  
                            "Sin hijos" = "#FFA07A", "Con hijos" = "#528B8B",  
                            "0 a 5" = "#FFA07A", "6 a 14 " = "#528B8B",  
                            "6 a 14" = "pink", "15 o más" = "#528B8B") 

#Gráficos de caja de bigotes.
for (i in 1:length(list_of_df2)){
  datos<- list_of_df2[[i]] 
  colnames(datos) <- gsub("[.]", " ", colnames(datos)) 
  datos_long5 <- gather(datos, Variable, Valor, -(Año:Total)) 
  
  plot <- ggplot(datos_long5, aes(x = factor(Variable), y = Valor, fill = Condición)) + 
    geom_boxplot() + 
    scale_fill_manual(values = colores_personalizados) + 
    labs(title = "", x = "Variable", y = "Porcentaje  de Mujeres") +
    theme_minimal()
}

#Gráficos de mosaico.
df_to_plot <- list_of_df[[10]]
print(colnames(df_to_plot))
colnames(df_to_plot) <- c("Año","Trimestre", "Condicion", "Alta","Media", "No calificada") 

df_to_plot <- df_to_table(df_to_plot)
df_to_plot<- as.data.frame(df_to_plot)


ggplot(data = df_to_plot) +
  geom_mosaic(aes(weight=Freq, x=product(Condicion), fill=Var2), na.rm=TRUE) +
  labs(x = "Maternindad", y = "Cualificación", fill = "Cualificación") + 
  theme_minimal()
