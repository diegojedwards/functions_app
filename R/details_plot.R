details.plot <- function(show, datos, params) {
    
    if(show == "pub") {
      ymax <- sum(table(datos$date))
      if (ymax < params$LS$quotas$quota_total) ymax <- params$LS$quotas$quota_total
      day_frec <- data.frame(table(datos$date))
      names(day_frec) <- c("Day", "Frecuency")
      day_frec <- data.frame(day_frec, Porcentaje = day_frec$Frecuency / sum(day_frec$Frecuency),
                             Frec_acum = cumsum(day_frec$Frecuency), 
                             Porc_acum = cumsum(day_frec$Frecuency) / params$LS$quotas$quota_total)
      plot_daily <- barplot(day_frec$Frecuency,  width = 1,
                            ylim = c(0, ymax), 
                            ylab = "Cummulative Counts" , cex.axis = 0.8, las=2,
                            names.arg = day_frec$Day, main = "Respuestas validas por dia")
      
      lines(plot_daily, day_frec$Frec_acum, type = "b", cex = 0.7, pch = 19, col="cyan4")
      box(col = "grey62")
      
      axis_y <- day_frec$Frec_acum[1]
      for(i in 1 : (length(day_frec$Frec_acum) - 1)) {
        
        if ((day_frec$Frec_acum[i + 1] - day_frec$Frec_acum[i]) > 6) axis_y <- c(axis_y, day_frec$Frec_acum[i + 1])
      }
      
      axis(side = 4, at = c(0, axis_y), 
           labels = paste(c(0, round(day_frec$Porc_acum * 100)[day_frec$Frec_acum %in% axis_y]) ,"%",sep=""), 
           las = 1, col.axis = "cyan4", col = "cyan4", cex.axis = 0.8)
    }
} 
