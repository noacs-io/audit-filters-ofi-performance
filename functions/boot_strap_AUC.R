boot_strap_AUC <- function(twoVariableData,indices){
  cleanDataSet <- twoVariableData[indices,]
  cleanDataSet <- na.omit(cleanDataSet)
  #ignore NA values
  cleanDataSet[1:2] <- lapply(cleanDataSet[1:2], as.numeric)
  #Convert values to numbers in order for roc to know what to do
  suppressMessages({rocData <- roc(cleanDataSet[,2], cleanDataSet[,1])})
  
  filterAUC <- as.numeric(auc(rocData))
  return(filterAUC)
}
