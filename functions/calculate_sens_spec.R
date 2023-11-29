library(tidymodels)
calculate_sens_spec <- function(twoVariableData){
  sensitivityOfAuditFilter <- sens_vec(as.factor(twoVariableData[[2]]), as.factor(twoVariableData[[1]]), event_level="second")
  specificityOfAuditFilter <- spec_vec(as.factor(twoVariableData[[2]]), as.factor(twoVariableData[[1]]), event_level="second")
  numberOfTrue <- sum(twoVariableData[[1]])
  return(c(sensitivityOfAuditFilter,specificityOfAuditFilter, numberOfTrue))
}