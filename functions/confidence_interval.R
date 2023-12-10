confidence_interval <- function(twoVariableData){
  set.seed(123)
  numberOfBootstraps <- 100
  bootResults <- boot(twoVariableData,statistic = boot_strap, R = numberOfBootstraps)
  boot2 <- boot(twoVariableData,statistic = boot_strap_AUC, R = numberOfBootstraps)
  AUCCI <- boot.ci(boot2, type = "basic")
  tTest <- t.test(boot2$t - 0.5, alternative = "greater", mu = 0)
  sensitivityCI <- boot.ci(bootResults, type = "basic", index = 1)  # Sensitivity CI
  specificityCI <- boot.ci(bootResults, type = "basic", index = 2) # Specificity CI
  return(c(round(sensitivityCI$basic[4:5] * 100, digits = 1),round(specificityCI$basic[4:5] * 100, digits = 1), round(AUCCI$basic[4:5], digits = 2), round(tTest$p.value, digits = 4) ))
}