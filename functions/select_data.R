select_data <- function(dataSet) {
    ## Replace with the contents of your function
  data <- rofi::import_data(test = TRUE)
  combinedDataset <- rofi::merge_data(data, test = TRUE)
  combinedDataset$ofi <- rofi::create_ofi(combinedDataset)
  selectedVariables <- combinedDataset %>% select(starts_with("VK"),ofi)
  return(selectedVariables)
}
