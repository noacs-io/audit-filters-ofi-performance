clean_data <- function(dataset){
  return(dataset %>% filter_all(any_vars(!is.na(.))))
}