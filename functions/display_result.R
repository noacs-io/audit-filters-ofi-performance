display_result <- function(calculatedData){
  results <- data.frame(
    Auditfilter = character(0),
    Specificity = numeric(0),
    Sensitivity = numeric(0)
  )
  results <- rbind(results, calculatedData)
  
return(results)
}