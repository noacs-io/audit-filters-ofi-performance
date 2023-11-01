logistic_regression <- function(dataSet,auditFilter){
  dataSet[,auditFilter]<- as.numeric(dataSet[,auditFilter] == "ja")
  dataSet$ofi <- as.numeric(dataSet$ofi == "yes")
  rocData <- roc(dataSet$ofi, dataSet[,auditFilter])
return(rocData)
}
