filters <- function(show, datos, params){
  
  if(show == "pub") {
    df <- data.frame()
    for (i in 1:length(params$LS$filters)){
      value <- params$LS$filters[[i]]$condition
      cant <- 0
      #df2 <- data.frame(ncol = dim(params$LS$filters[[i]]$condition)[2])
      for(j in 1:length(params$LS$filters[[i]]$question)) {
        
        #var <- params$LS$filters[[i]]$question[[j]]
        #df2[j] <- c(dlsfull[dlsfull[, params$LS$filters[[i]]$question[[j]]] == params$LS$filters[[i]]$answer[[j]], var])
        cant <- cant + sum(datos[, params$LS$filters[[i]]$question[[j]]] == params$LS$filters[[i]]$answer[[j]])
      }
      df <- rbind(df, data.frame(
        Filtro = value,
        Cantidad = cant
      ))
    }
    df <- rbind(df, data.frame(
      Filtro = "Total",
      Cantidad = sum(df$Cantidad)))
    df
  }
  
  
}
  
  filters <- function(show, datos, params){
  
  if(show == "pub") {
    df <- data.frame()
    for (i in 1:length(params$LS$filters)){
      value <- params$LS$filters[[i]]$condition
      cant <- 0
      #df2 <- data.frame(ncol = dim(params$LS$filters[[i]]$condition)[2])
      for(j in 1:length(params$LS$filters[[i]]$question)) {
        
        #var <- params$LS$filters[[i]]$question[[j]]
        #df2[j] <- c(dlsfull[dlsfull[, params$LS$filters[[i]]$question[[j]]] == params$LS$filters[[i]]$answer[[j]], var])
        cant <- cant + sum(datos[, params$LS$filters[[i]]$question[[j]]] == params$LS$filters[[i]]$answer[[j]])
      }
      df <- rbind(df, data.frame(
        Filtro = value,
        Cantidad = cant
      ))
    }
    df <- rbind(df, data.frame(
      Filtro = "Total",
      Cantidad = sum(df$Cantidad)))
    df
  }
  
  
}
  
  