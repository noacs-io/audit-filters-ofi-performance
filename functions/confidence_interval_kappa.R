confidence_interval_kappa <- function(twoVariableData){
  set.seed(123) #Setting the seed for bootstrap
  numberOfBootstraps <- 1000 #Determining number of bootstrap iterations
  bootResults <- boot(twoVariableData,statistic = bootstrap_kappa, R = numberOfBootstraps) #bootstrapping 
  kappaConfidenceInterval <- boot.ci(bootResults, type = "basic") #calculating confidence interval from bootstrap
  return(c(round(bootResults$t0, digits = 2), round(kappaConfidenceInterval$basic[4:5], digits = 2))) # returning the original cohen kappa followed by the confidence interval
}