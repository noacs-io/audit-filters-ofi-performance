confidence_interval_sens_spec <- function(twoVariableData){
  set.seed(123) #Setting the seed for bootstrap
  numberOfBootstraps <- 100 #Determining number of bootstrap iterations
  bootResults <- boot(twoVariableData,statistic = bootstrap_sens_spec, R = numberOfBootstraps) #bootstrapping 
  sensitivityCI <- boot.ci(bootResults, type = "basic", index = 1) # Sensitivity CI
  specificityCI <- boot.ci(bootResults, type = "basic", index = 2) #calculating confidence interval from bootstrap
  PPVCI <- boot.ci(bootResults, type = "basic", index = 3)
  NPVCI <- boot.ci(bootResults, type = "basic", index = 4)
  return(c(round(sensitivityCI$t0 * 100, digits = 1), 
           round(sensitivityCI$basic[4:5] * 100, digits = 1), 
           round(specificityCI$t0 * 100, digits = 1), 
           round(specificityCI$basic[4:5] * 100, digits = 1),
           round(PPVCI$t0 * 100, digits = 1),
           round(PPVCI$basic[4:5] * 100, digits = 1),
           round(NPVCI$t0 * 100, digits = 1),
           round(NPVCI$basic[4:5] * 100, digits = 1)))
}