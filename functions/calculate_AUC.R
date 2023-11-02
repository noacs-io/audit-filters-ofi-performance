calculate_AUC <- function(twoVariableData){
  #sum of true positive values
  print(twoVariableData)
  cleanDataSet <- na.omit(twoVariableData)  
  #ignore NA values
  sumOfAuditFilterOccurence <- nrow(cleanDataSet)
  TP <- sum(cleanDataSet[,1] & cleanDataSet[,2])
  TN <- sum(!cleanDataSet[,1] & !cleanDataSet[,2])
  FP <- sum(!cleanDataSet[,1] & cleanDataSet[,2])
  FN <- sum(cleanDataSet[,1] & !cleanDataSet[,2])
  # Calculate Sensitivity (True Positive Rate)
  sensitivityOfAuditFilter <- TP / (TP + FN)
  # Calculate Specificity (True Negative Rate)
  specificityOfAuditFilter <- TN / (TN + FP)
  print(sensitivityOfAuditFilter)
  cleanDataSet[1:2] <- lapply(cleanDataSet[1:2], as.numeric)
  #Convert values to numbers in order for roc to know what to do
  print(cleanDataSet)
  rocData <- roc(cleanDataSet[,2], cleanDataSet[,1])
  AUCCI <- round(as.numeric(ci.auc(rocData, method="bootstrap", conf.level=0.95)), digits = 2)
  #calculate AUC and confidenceintervals for it
  plot(rocData, print.auc=TRUE)
  #plot ROC curves including AUC value
  
  return(AUCCI)
}