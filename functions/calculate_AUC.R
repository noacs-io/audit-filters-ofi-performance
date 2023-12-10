calculate_AUC <- function(twoVariableData, listOfAuditFiltersClean, counter){
  #sum of true positive values
  twoVariableData[1:2] <- lapply(twoVariableData[1:2], as.numeric)
  #Convert values to numbers in order for roc to know what to do
  suppressMessages({rocData <- roc(twoVariableData[,2], twoVariableData[,1])})
  AUCroc <- auc(rocData)
  AUC <- round(as.numeric(AUCroc), digits = 2)
  
  #plot.roc(rocData, col="blue", grid = TRUE, grid.col=c("green", "red"), 
 #          main = listOfAuditFiltersClean[counter], percent=TRUE,
 #          grid.lwd = 0.5, grid.lty=1 )
  return(AUC)
}