import_data_ofi <- function(data){
  data <- rofi::import_data()
  #import database
  combinedDataset <- rofi::merge_data(data)
  #Merge data
  combinedDataset$ofi <- rofi::create_ofi(combinedDataset)
  #Add OFI to database
  return(combinedDataset)
}
