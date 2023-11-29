calculate_AUC <- function(twoVariableData){
  #sum of true positive values
  twoVariableData[1:2] <- lapply(twoVariableData[1:2], as.numeric)
  #Convert values to numbers in order for roc to know what to do
  suppressMessages({rocData <- roc(twoVariableData[,2], twoVariableData[,1])})
                   
  AUCCI <- round(as.numeric(ci.auc(rocData, method="bootstrap",boot.n = 1000, conf.level=0.95)), digits = 2)
  
  #n <- 100
  #labels <- sample(0:1, n, replace = TRUE)
  #scores <- runif(n)
  
  rocData1 <- auc(twoVariableData[,2], twoVariableData[,1])
  #rocData2 <- control_ROC()
  # Create a ROC curve with an AUC of 0.5
  #rocData2 <- auc(labels, scores, auc = 0.5)
  print(AUCCI)
  #test_result <- roc.test(rocData1, rocData2, method = "bootstrap",boot.n = 500, boot.stratified = TRUE)
  #print(test_result)
  
  #calculate AUC and confidenceintervals for it
  #plot(rocData, print.auc=TRUE)
  #plot ROC curves including AUC value
  
  return(AUCCI)
}