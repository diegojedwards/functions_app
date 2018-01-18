tabIPAddress <- function(datos) {
  
  datos %>%
    group_by(ipaddr) %>%
    summarise(count = n()) %>%
    filter(count >= 2) %>%
    ungroup() %>%
    arrange(desc(count))
}
