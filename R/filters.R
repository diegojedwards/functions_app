filters <- function(show, datos){

  if(show == "pub") {
    df <- data.frame()
    for (i in 1:length(params$LS$filters)){
      value <- params$LS$filters[[i]]$condition
      cant <- 0

      if(length(params$LS$filters[[i]][[2]]) > 1 & params$LS$filters[[i]]$value.logic == "Y") {

          df2 <- data.frame(null = matrix(0, nrow = dim(datos)[1]))

          for(j in 1:length(params$LS$filters[[i]]$question)) {

            newcol <- paste(params$LS$filters[[i]]$question[[j]], "bis", sep = "")
            datos[datos[,params$LS$filters[[i]]$question[[j]]] == "Sí", newcol] <- 1
            df2 <- cbind(df2, datos[newcol])
          }
          df2$sum <- apply(df2, 1, sum)
          cant <- table(df2$sum)[[1]]
      } else {

          for(j in 1 : length(params$LS$filters[[i]]$question)) {

            cant <- cant + sum(datos[, params$LS$filters[[i]]$question[[j]]] == params$LS$filters[[i]]$answer[[j]])
          }
        }
        df <- rbind(df, data.frame(
                    Filtro = value,
                    Cantidad = cant))
    }
    df <- rbind(df, data.frame(
                Filtro = "Total",
                Cantidad = sum(df$Cantidad)))
  }

  df
}
  
    
  
