display_result <- function(result){
  list.auditfilters <- c("VK_sap_less90","VK_mjaltskada", "VK_mass_transf", "VK_gcs_less9_ej_intubTE", "VK_iss_15_ej_iva", "VK_mer_60min_interv","VK_mer_30min_DT","VK_ej_trombrof_TBI_72h","VK_hlr_thorak", "VK_leverskada", "VK_avslutad")
  #print(calculate.data)
  results <- data.frame(
    Auditfilter = character(0),
    Specificity = numeric(0),
    Sensitivity = numeric(0)
  )
  for (individual.auditfilters in list.auditfilters){
    calculated.data <- calculate_data(individual.auditfilters)
    results <- rbind(results, calculated.data)
  }
  return(results)
}