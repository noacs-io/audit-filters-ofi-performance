library(irr)
calculate_cohen_kappa <- function(twoVariableData){
  kappa_result <- kappa2(twoVariableData)
  return(kappa_result$value)
}