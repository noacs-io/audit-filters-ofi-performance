logistic_regression <- function(dataset,auditfilters){
  for (auditfilter in auditfilters){
    two.variable <- na.omit(clean.data[,c(auditfilter,"ofi")])
    two.variable[,auditfilter]<- as.numeric(two.variable[,auditfilter] == "Ja")
    two.variable$ofi <- as.numeric(two.variable$ofi == "Yes")
    roc_data <- roc(two.variable$ofi, two.variable[,auditfilter])
    plot(roc_data, print.auc = TRUE, auc.polygon = TRUE, grid = TRUE, main = as.character(auditfilter))
  }
}