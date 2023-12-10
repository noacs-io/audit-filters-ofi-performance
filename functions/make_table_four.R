make_table_three <- function(){
  AUCResultFinal <- paste(c(calculateAUC[2]," (",confidenceInterval[5],"-",confidenceInterval[6],") "), collapse = "")
  tableResult <- data.frame(Auditfilter = auditFilter,
                            AUC = AUCResultFinal,
                            PValue = ifelse(confidenceInterval[7] == 0, "<0.0001", as.character(confidenceInterval[7])) )

  return(tableResult)
}