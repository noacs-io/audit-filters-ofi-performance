calculate_data <- function(cleanDataSet, auditFilter){
  #sum of true positive values
  cleanDataSet <- na.omit(cleanDataSet[,c(auditFilter,"ofi")])  
  #ignore NA values
  sumOfAuditFilterOccurence <- nrow(cleanDataSet)
  TP <- sum(cleanDataSet[,auditFilter] & cleanDataSet$ofi)
  TN <- sum(!cleanDataSet[,auditFilter] & !cleanDataSet$ofi)
  FP <- sum(!cleanDataSet[,auditFilter] & cleanDataSet$ofi)
  FN <- sum(cleanDataSet[,auditFilter] & !cleanDataSet$ofi)
  # Calculate Sensitivity (True Positive Rate)
  sensitivityOfAuditFilter <- TP / (TP + FN)
  # Calculate Specificity (True Negative Rate)
  specificityOfAuditFilter <- TN / (TN + FP)
  
  cleanDataSet[,auditFilter]<- as.numeric(cleanDataSet[,auditFilter])
  cleanDataSet$ofi <- as.numeric(cleanDataSet$ofi)
  rocData <- roc(cleanDataSet$ofi, cleanDataSet[,auditFilter])
  rocAUC <- c(auc(rocData))
  AUCCI <- as.numeric(ci.auc(rocData, method="bootstrap", conf.level=0.95))
  SensitivityCI <- ci.se(rocData, method="bootstrap", specificities=specificityOfAuditFilter, conf.level=0.95)
  SpecificityCI <- as.numeric(ci.sp(rocData, method="bootstrap", sensitivities=sensitivityOfAuditFilter, conf.level=0.95))
  
  lowerBoundCIForSensitivity <- SensitivityCI[1]
  upperBoundCIForSensitivity <- SensitivityCI[3]
  
  lowerBoundCIForSpecificity <- SpecificityCI[1]
  upperBoundCIForSpecificity <- SpecificityCI[3]
  
  lowerBoundCIForAUC <- AUCCI[1]
  upperBoundCIForAUC <- AUCCI[3]
  plot(rocData, print.auc=TRUE)
  
  calculatedData <- data.frame(Auditfilter = auditFilter, 
                               Number = sumOfAuditFilterOccurence,
                               Specificity = specificityOfAuditFilter, 
                               Sensitivity = sensitivityOfAuditFilter,
                               AUC = rocAUC,
                               CI_lower_AUC = lowerBoundCIForAUC,
                               CI_upper_AUC = upperBoundCIForAUC,
                               CI_lower_sens = lowerBoundCIForSensitivity,
                               CI_upper_sens = upperBoundCIForSensitivity,
                               CI_lower_spec = lowerBoundCIForSpecificity,
                               CI_upper_spec = upperBoundCIForSpecificity
                               )
  
  return(calculatedData)
}