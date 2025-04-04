bootstrap_sens_spec <- function(twoVariableData, indices){
  cleanDataSet <- twoVariableData[indices,]
  sensspec <- calculate_sens_spec(cleanDataSet)
  return(c(sensspec[1],sensspec[2],sensspec[3],sensspec[4]))
}
