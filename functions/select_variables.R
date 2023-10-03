selected_auditfilters <- function(dataset) {
    ## Replace with the contents of your function
  data <- rofi::import_data(test = TRUE)
  combined.dataset <- rofi::merge_data(data, test = TRUE)
  combined.dataset$ofi <- rofi::create_ofi(combined.dataset)
  selected.auditfilters <- combined.dataset %>% select(starts_with("VK"),ofi)
  return(selected.auditfilters)
}
