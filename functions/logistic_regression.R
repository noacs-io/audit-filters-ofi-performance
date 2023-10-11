logistic_regression <- function(dataSet,auditFilter){
  dataSet[,auditFilter]<- as.numeric(dataSet[,auditFilter] == "ja")
  dataSet$ofi <- as.numeric(dataSet$ofi == "yes")
  roc_data <- roc(dataSet$ofi, dataSet[,auditFilter])
  plot(roc_data, print.auc = TRUE, auc.polygon = TRUE, grid = TRUE, main = as.character(auditFilter))
}
