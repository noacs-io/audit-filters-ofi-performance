confidence_interval <- function(twoVariableData){
  set.seed(123)
  numberOfBootstraps <- 1000
  bootResults <- boot(twoVariableData,statistic = boot_strap, R = numberOfBootstraps)
  sensitivityCI <- boot.ci(bootResults, type = "basic", index = 1)  # Sensitivity CI
  specificityCI <- boot.ci(bootResults, type = "basic", index = 2) # Specificity CI
  return(c(round(sensitivityCI$basic[4:5] * 100, digits = 1),specificityCI$basic[4:5] * 100, digits = 1))
}