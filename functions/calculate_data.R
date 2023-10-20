calculate_data <- function(cleanDataSet, auditfilter){
  #sum of true positive values
  sumOfAuditfilterOccurence <- sum(cleanDataSet[,auditfilter] == "ja" | cleanDataSet[,auditfilter] == "nej")
  TP <- sum(cleanDataSet[,auditfilter] == "ja" & cleanDataSet$ofi == "yes")
  #sum of true negative values
  TN <- sum(cleanDataSet[,auditfilter] == "nej" & cleanDataSet$ofi == "no")
  #sum of false positive values
  FP <- sum(cleanDataSet[,auditfilter] == "nej" & cleanDataSet$ofi == "yes")
  #sum of false negative values
  FN <- sum(cleanDataSet[,auditfilter] == "ja" & cleanDataSet$ofi == "no")
  # Calculate Sensitivity (True Positive Rate)
  sensitivityOfAuditfilter <- TP / (TP + FN)
  # Calculate Specificity (True Negative Rate)
  specificityOfAuditfilter <- TN / (TN + FP)
  calculatedData <- data.frame(Auditfilter = auditfilter, 
                               Number = sumOfAuditfilterOccurence, 
                               Truepositives = TP, 
                               Truenegatives = TN, 
                               Falsepositives = FP, 
                               Truenegatives = TN, 
                               Specificity = specificityOfAuditfilter, 
                               Sensitivity = sensitivityOfAuditfilter)
  
  return(calculatedData)
}