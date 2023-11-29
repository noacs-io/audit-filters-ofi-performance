boot_strap <- function(twoVariableData,indices){
  cleanDataSet <- twoVariableData[indices,]
  sensspec <- calculate_sens_spec(cleanDataSet)
  return(c(sensspec[1],sensspec[2]))
}
