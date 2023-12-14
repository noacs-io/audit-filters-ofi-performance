make_table_two <- function(calculateSensSpec,confidenceInterval, calculateAUC, listOfAuditFiltersClean, counter){
  sensitivityResultFinal <- paste(c(round(calculateSensSpec[1] * 100, digits = 1)," (",confidenceInterval[1],"-",confidenceInterval[2],") "), collapse = "")
  specificityResultFinal <- paste(c(round(calculateSensSpec[2] * 100, digits = 1)," (",confidenceInterval[3],"-",confidenceInterval[4],") "), collapse = "")
  numberOfTrue <- calculateSensSpec[3]
  tableResult <- data.frame(Auditfilter = listOfAuditFiltersClean[counter],
                            Number = numberOfTrue,
                            Specificity = specificityResultFinal,
                            Sensitivity = sensitivityResultFinal)
                            
 
  return(tableResult)
}