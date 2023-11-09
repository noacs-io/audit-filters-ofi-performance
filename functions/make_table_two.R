make_table_two <- function(calculatedSensSpec,confidenceInterval, calculateAUC){
  AUCResult <- round(calculatedAUC[2], digits = 2)
  AUCCIlowerResult <- round(calculatedAUC[1],digits = 2)
  AUCCIupperResult<- round(calculatedAUC[3], digits = 2)
  sensitivityResultFinal <- paste(c(calculateSensSpec[1]," (",confidenceInterval[1],"-",confidenceInterval[2],") "), collapse = "")
  specificityResultFinal <- paste(c(calculateSensSpec[2]," (",confidenceInterval[3],"-",confidenceInterval[4],") "), collapse = "")
  AUCResultFinal <- paste(c(calculatedAUC[2]," (",calculatedAUC[1],"-",calculatedAUC[3],") "), collapse = "")
  numberOfTrue <- calculateSensSpec[3]
  tableResult <- data.frame(Auditfilter = auditFilter,
                            Number = numberOfTrue,
                            Specificity = specificityResultFinal,
                            Sensitivity = sensitivityResultFinal,
                            AUC = AUCResultFinal)
 
  return(tableResult)
}