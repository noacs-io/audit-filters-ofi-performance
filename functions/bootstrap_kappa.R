bootstrap_kappa <- function(twoVariableData, indices){
  cleanDataSet <- twoVariableData[indices,]
  kappaValue <- calculate_cohen_kappa(cleanDataSet)
  return(kappaValue)
}

