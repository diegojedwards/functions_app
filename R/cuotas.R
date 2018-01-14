cuotas <- function(show, datos, params) {
  
  if(show == "pub") {
    df <- data.frame()
    for (i in 1:length(params$LS$quotas$quotas.list)){
      vars <- unlist(params$LS$quotas$quotas.list[[i]]$question)
      values <- unlist(params$LS$quotas$quotas.list[[i]]$answers)
      quota <- unlist(params$LS$quotas$quotas.list[[i]]$size)
      cant <- 0
      for (var in vars)
        for (val in values)
          cant <- cant + sum(datos[, var] == val)
      df <- rbind(
        df, data.frame(
          Cuota = names(params$LS$quotas$quotas.list)[i],
          Limite = quota,
          Cantidad = cant,
          Porc = paste0(round(cant/quota*100,0), ' %')))
      
    }
    df <- rbind(df, data.frame(Cuota = "Total", Limite = sum(df$Limite),
                               Cantidad = sum(df$Cantidad), Porc = ""))
    df
  }
}