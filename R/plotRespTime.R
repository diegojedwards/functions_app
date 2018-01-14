plotRespTime <- function(show, datos, params) {
  
  if(show == "pub") {
    ggplot(datos, aes(totaltime)) +
      geom_histogram(bins = sqrt(dim(datos)[1]), fill="blue", col = "black") +
      ggtitle("Histogram of response times") + xlab("Response times") + ylab("count") +
      theme_update(plot.title = element_text(hjust = 0.5)) +
      theme(title =element_text(size=15)) +
      geom_vline(xintercept = params$LS$ls_min_time, col = "red", size = 1) +
      geom_vline(xintercept = params$LS$ls_max_time, col = "red", size = 1)
  }
}