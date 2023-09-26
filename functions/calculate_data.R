calculate_data <- function(auditfilter){
  two.variable.data <- na.omit(clean.data[,c(auditfilter,"ofi")])
  print(two.variable.data)
  #sum of true positive values
  TP <- sum(two.variable.data[,auditfilter] == "Ja" & two.variable.data$ofi == "Yes")
  #sum of true negative values
  TN <- sum(two.variable.data[,auditfilter] == "Nej" & two.variable.data$ofi == "No")
  #sum of false positive values
  FP <- sum(two.variable.data[,auditfilter] == "Ja" & two.variable.data$ofi == "Yes")
  #sum of false negative values
  FN <- sum(two.variable.data[,auditfilter] == "Nej" & two.variable.data$ofi == "No")
  
  # Calculate Sensitivity (True Positive Rate)
  auditfilter.sensitivity <- TP / (TP + FN)
  # Calculate Specificity (True Negative Rate)
  auditfilter.specificity <- TN / (TN + FP)
  auditfilter.result <- data.frame(
    Auditfilter = c(auditfilter),
    Specificity = c(auditfilter.specificity),
    Sensitivity = c(auditfilter.sensitivity)
  )
  return(auditfilter.result)
}