#' Select_variables
#'
#' Write a short description of your function here 
#' @param x Describe argument x here 
#' @export 
prepare_data <- function(dataset) {
    ## Replace with the contents of your function
  data <- rofi::import_data(test = TRUE)
  combined.dataset <- rofi::merge_data(data, test = TRUE)
  combined.dataset$ofi <- rofi::create_ofi(combined.dataset)
  return(combined.dataset)
}
