boot_strap <- function(twoVariableData,indices){
  cleanDataSet <- twoVariableData[indices,]
  cleanDataSet <- na.omit(cleanDataSet)
  #ignore NA values
  sumOfAuditFilterOccurence <- nrow(cleanDataSet)
  TP <- sum(cleanDataSet[1] & cleanDataSet[2])
  TN <- sum(!cleanDataSet[1] & !cleanDataSet[2])
  FP <- sum(!cleanDataSet[1] & cleanDataSet[2])
  FN <- sum(cleanDataSet[1] & !cleanDataSet[2])
  # Calculate Sensitivity (True Positive Rate)
  sensitivityOfAuditFilter <- TP / (TP + FN)
  # Calculate Specificity (True Negative Rate)
  specificityOfAuditFilter <- TN / (TN + FP)
  
  return(c(sensitivityOfAuditFilter,specificityOfAuditFilter))
}
