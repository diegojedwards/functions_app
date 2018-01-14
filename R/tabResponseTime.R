tabResponseTime <- function(datos, params) {
  
  cant.min <- dim(subset(datos, totaltime < params$LS$ls_min_time, "totaltime"))[1]
  cant.max <- dim(subset(datos, totaltime > params$LS$ls_max_time, "totaltime"))[1]
  df <- data.frame(Time = c(paste("less than", params$LS$ls_min_time, "min", sep = " "),
                            paste("greather than", params$LS$ls_max_time, "min", sep = " ")),
                   Count = c(cant.min, cant.max))
  df
  
}