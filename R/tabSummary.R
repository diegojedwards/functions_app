tabSummary <- function(show, SWCE, datos) {
  
  if(show == "pub") {
    varname <- ifelse(SWCE, "estadoLSCE", "estadoLS")
    setTable <- setTable_1d(datos, varname = varname)
    setTable_1 <- data.frame( "Total", sum(setTable$Freq), "")
    names(setTable_1) <- colnames(setTable)
    setTable <- rbind(setTable, setTable_1)
    setTable
  }
}