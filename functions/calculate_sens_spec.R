library(tidymodels)
calculate_sens_spec <- function(twoVariableData){
  truePositives <- sum(twoVariableData[[1]] == TRUE & twoVariableData$ofi == TRUE)
  falsePositives <- sum(twoVariableData[[1]] == TRUE & twoVariableData$ofi == FALSE)
  trueNegatives <- sum(twoVariableData[[1]] == FALSE & twoVariableData$ofi == FALSE)
  falseNegatives <- sum(twoVariableData[[1]] == FALSE & twoVariableData$ofi == TRUE)
  positivePredictiveValue <- truePositives/(truePositives+falsePositives)
  negativePredictiveValue <- trueNegatives/(trueNegatives+falseNegatives)
  sensitivityOfAuditFilter <- sens_vec(as.factor(twoVariableData[[2]]), as.factor(twoVariableData[[1]]), event_level="second")
  specificityOfAuditFilter <- spec_vec(as.factor(twoVariableData[[2]]), as.factor(twoVariableData[[1]]), event_level="second")
  return(c(sensitivityOfAuditFilter,
           specificityOfAuditFilter, 
           positivePredictiveValue, 
           negativePredictiveValue))
}