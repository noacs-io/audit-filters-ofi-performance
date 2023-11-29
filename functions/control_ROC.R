control_ROC <- function(){
  set.seed(123)
  rows <- 10000
  df <- data.frame(
    Predictor = rep(c(1,1,0,0),rows),
    Outcome = rep(c(0,1,0,1),rows)
  )
  
  # Calculate ROC curve and AUC
  roc_curve <- roc(df$Outcome, df$Predictor)
  return(auc(df$Outcome, df$Predictor))
}