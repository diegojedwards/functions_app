caidas <-  function(show, SWCE, datos, params) {
  
  if(show == "pub") {
    ques <- ls_get_questions(params$LS$idLS)
    ques$nques <- 1:(dim(ques)[1])
    filtervar <- ifelse(params$SWCE$exists.swce, "estadoLSCE", "estadoLS")
    
    a <- datos %>%
      filter_(paste0(filtervar, " == 'LS incompleto'")) %>%
      select(id, lastpage) %>%
      group_by(lastpage) %>%
      summarise(freq=n())
    b <- merge(x=a, y=ques, by.x="lastpage", by.y="nques") %>%
      select(lastpage, freq, question)
    
    b$question <- trimws(b$question)
    b$codetext <- paste(b$qcode, b$question, sep=" - ")
    b <- rbind(b, data.frame(lastpage = "Total", freq = sum(b$freq), 
                             question = "", codetext = ""))
    b
  }
  
  
  
}