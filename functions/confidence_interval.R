confidence_interval <- function(twoVariableData){
  set.seed(123)
  numberOfBootstraps <- 1000
  bootResults <- boot(twoVariableData,statistic = boot_strap, R = numberOfBootstraps)
  boot2 <- boot(twoVariableData,statistic = boot_strap_AUC, R = numberOfBootstraps)
  AUCCI <- boot.ci(boot2, type = "basic")
  ## Implement "bootstrap p-value"
  mu <- 0.5
  mu0.8 <- 0.8
  test.statistic <- abs(boot2$t0 - mu)
  test.distribution <- boot2$t - mu
  centered.test.distribution <- test.distribution - mean(test.distribution)
  p.value <- mean(centered.test.distribution < -test.statistic | centered.test.distribution > test.statistic)
  ## t.test
  test.statistic0.8 <- abs(boot2$t0 - mu0.8)
  test.distribution0.8 <- boot2$t - mu0.8
  centered.test.distribution0.8 <- test.distribution0.8 - mean(test.distribution0.8)
  p.value0.8 <- mean(centered.test.distribution0.8 < -test.statistic0.8 | centered.test.distribution0.8 > test.statistic0.8)
  ## t.test
  tTest <- t.test(boot2$t, alternative = "two.sided", mu = 0.5)
  sensitivityCI <- boot.ci(bootResults, type = "basic", index = 1)  # Sensitivity CI
  specificityCI <- boot.ci(bootResults, type = "basic", index = 2) # Specificity CI
  return(c(round(sensitivityCI$basic[4:5] * 100, digits = 1), round(specificityCI$basic[4:5] * 100, digits = 1), round(AUCCI$basic[4:5], digits = 3), round(p.value, digits = 3), round(p.value0.8, digits = 3)))
}