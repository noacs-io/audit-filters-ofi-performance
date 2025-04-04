missing_values <- function(dataWithAuditFilters){
  totalValues <- nrow(dataWithAuditFilters)
  sumMissingValuessl90 <- sum(is.na(dataWithAuditFilters$ed_sbp_value) & is.na(dataWithAuditFilters$ed_sbp_rtscat))
  sumOfAFsl90 <- sum(dataWithAuditFilters$AF_sap_less90)
  sumOfVKsl90 <- sum(dataWithAuditFilters$VK_sap_less90 == "ja", na.rm = TRUE)
  sumDsl90 <- sum(dataWithAuditFilters$AF_sap_less90 & dataWithAuditFilters$VK_sap_less90 == "nej", na.rm = TRUE)
  sumDsl90t <- sum(!dataWithAuditFilters$AF_sap_less90 & dataWithAuditFilters$VK_sap_less90 == "ja", na.rm = TRUE)
  #SAP<90
  
  sumMissingValuesGCSI <- sum(is.na(dataWithAuditFilters$ed_gcs_sum) | is.na(dataWithAuditFilters$ed_intubated))
  sumMissingValuesCT <- sum(is.na(dataWithAuditFilters$dt_ed_first_c))
  sumMissingValuesProcedure <- sum(is.na(dataWithAuditFilters$dt_ed_emerg_proc))
  sumMissingValuesISSint <- sum(is.na(dataWithAuditFilters$ISS) | is.na(dataWithAuditFilters$host_care_level))
  sumMissingValuesISSTN <- sum(is.na(dataWithAuditFilters$Tr_Nivå) | is.na(dataWithAuditFilters$ISS))
  sumMissingValues30dag <- sum(is.na(dataWithAuditFilters$res_survival))
  sumMissingValuesTBI <- sum(is.na(dataWithAuditFilters$VK_ej_trombrof_TBI_72h))
  sumMissinfValuesHLR <- sum(is.na(dataWithAuditFilters$VK_hlr_thorak))
  sumMissingValuesMT <- sum(is.na(dataWithAuditFilters$VK_mass_transf))
  sumMissingValuesL <- sum(is.na(dataWithAuditFilters$VK_leverskada))
  sumMissingValuesM <- sum(is.na(dataWithAuditFilters$VK_mjaltskada))
  sumMissingValuesLM <- sumMissingValuesL + sumMissingValuesM
  
  listOfSums <- c(sumMissingValuessl90,sumMissingValues30dag, 
                  sumMissingValuesISSTN,sumMissingValuesMT,
                  sumMissingValuesGCSI, sumMissingValuesISSint,
                  sumMissingValuesProcedure, sumMissingValuesCT,
                  sumMissinfValuesHLR,sumMissingValuesLM, sumMissingValuesTBI)
  
  missingValues <- sapply(listOfSums, function(x) paste(c(x, " (", round(x/totalValues*100, digits = 1), ")"), collapse = "")) 
   return(missingValues)
}