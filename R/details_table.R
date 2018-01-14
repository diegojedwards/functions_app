details.table <- function(show, SWCE, datos, params) {
  
  if(show == "pub") {
    if (nrow(datos) != 0) {
      varname <- ifelse(SWCE, "estadoLSCE", "estadoLS")
      setTable_2d(datos, row_varname="date", col_varname=varname)
    }
  }
}