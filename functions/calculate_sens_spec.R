calculate_sens_spec <- function(twoVariableData){
  twoVariableData <- na.omit(twoVariableData)
  #ignore NA values
  TP <- sum(twoVariableData[1] & twoVariableData[2])
  TN <- sum(!twoVariableData[1] & !twoVariableData[2])
  FP <- sum(!twoVariableData[1] & twoVariableData[2])
  FN <- sum(twoVariableData[1] & !twoVariableData[2])
  # Calculate Sensitivity (True Positive Rate)
  numberOfTrue <- TP + FP
  sensitivityOfAuditFilter <- round((TP / (TP + FN)*100),digits = 1)
  # Calculate Specificity (True Negative Rate)
  specificityOfAuditFilter <- round((TN / (TN + FP)*100), digits = 1)
  
  return(c(sensitivityOfAuditFilter,specificityOfAuditFilter, numberOfTrue))
}