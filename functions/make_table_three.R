make_table_three <- function(calculateAUC, confidenceInterval, listOfAuditFiltersClean, counter){
  AUCResultFinal <- paste(c(round(calculateAUC, digits = 2)," (",confidenceInterval[5],"-",confidenceInterval[6],") "), collapse = "")
  tableResult <- data.frame(Auditfilter = listOfAuditFiltersClean[counter],
                            AUC = AUCResultFinal,
                            PValue = ifelse(confidenceInterval[7] == 0, "<0.0001", as.character(confidenceInterval[7])) )

  return(tableResult)
}