create_audit_filters <- function(cleanData){
### SAP < 90 ###
  cleanData$AF_sap_less90 <- ifelse(
    is.na(cleanData$ed_sbp_value) & is.na(cleanData$ed_sbp_rtscat), NA, 
    ifelse(is.na(cleanData$ed_sbp_value), cleanData$ed_sbp_rtscat <= 3, 
           ifelse(cleanData$ed_sbp_value < 90 , TRUE,FALSE)))
  #VK_sap_less90
  cleanData$GCS_under_9 <- cleanData$ed_gcs_sum < 9 & !is.na(cleanData$ed_intubated)
  cleanData$GCS_under_9_M <- is.na(cleanData$ed_gcs_sum) | is.na(cleanData$ed_intubated)
  cleanData$AF_gcs_less9_ej_intubTE <- ifelse(
    is.na(cleanData$ed_gcs_sum) | is.na(cleanData$ed_intubated) | cleanData$ed_gcs_sum >= 9, NA, 
    ifelse(cleanData$ed_gcs_sum < 9 & cleanData$ed_intubated != "Yes", TRUE,FALSE))
  #VK_gcs_less9_ej_intubTE
  cleanData$AF_mer_30min_DT <- cleanData$dt_ed_first_ct > 30

  #cleanData$AF_mer_30min_DT[is.na(cleanData$AF_mer_30min_DT)] <- FALSE
  #VK_mer_30min_DT
  cleanData$AF_mer_60_min_interv <- cleanData$dt_ed_emerg_proc > 60
  #cleanData$AF_mer_60_min_interv[is.na(cleanData$AF_mer_60_min_interv)] <- FALSE
  #VK_mer_60_min_interv
  #cleanData$AF_iss_15_ej_iva <- cleanData$ISS >= 15 & cleanData$host_care_level != "Intensive care unit"
  cleanData$ISS_over_15 <- cleanData$ISS >= 15 & !is.na(cleanData$host_care_level)
  cleanData$ISS_over_15_M <- is.na(cleanData$ISS) | is.na(cleanData$host_care_level)
  cleanData$AF_iss_15_ej_iva <- ifelse(
    is.na(cleanData$ISS) | is.na(cleanData$host_care_level) | cleanData$ISS < 15, NA, 
    ifelse(cleanData$ISS >= 15 & cleanData$host_care_level != "Intensive care unit", TRUE,FALSE))
  #VK_iss_15_ej_iva
  cleanData$AF_death_30d <- cleanData$res_survival == "Yes"
  #cleanData$AF_death_30d[is.na(cleanData$AF_death_30d)] <- FALSE
  #death after 30d
  #cleanData$AF_iss_15_ej_TE <- cleanData$ISS >= 15 & cleanData$Tr_Nivå == "No"
  cleanData$ISS_over_15_T <- cleanData$ISS >= 15 & !is.na(cleanData$Tr_Nivå)
  cleanData$ISS_over_15_T_M <- is.na(cleanData$ISS) | is.na(cleanData$Tr_Nivå)
  cleanData$AF_iss_15_ej_TE <- ifelse(
    is.na(cleanData$ISS) | is.na(cleanData$Tr_Nivå) | cleanData$ISS < 15, NA, 
    ifelse(cleanData$ISS >= 15 & cleanData$Tr_Nivå == "No", TRUE,FALSE))
  
  #ISS 15 och ingen trauma larm
  check_starts_with_1 <- function(row) {
    any(grepl("^1", as.character(row)))
  }
  AISCols <- grep("^AISCode_", names(cleanData))
  cleanData$TBI <- apply(cleanData[AISCols], 1, function(row){
    if (check_starts_with_1(row)) {
      return(TRUE)
    } else {
      return(NA)
    }
  })
  
  cleanData$TBI_M <- is.na(cleanData$VK_ej_trombrof_TBI_72h) | is.na(cleanData$AISCode_01)
  cleanData$AF_ej_trombrof_TBI_72h <- ifelse(is.na(cleanData$TBI) | is.na(cleanData$VK_ej_trombrof_TBI_72h), NA, 
                                             ifelse(cleanData$TBI == TRUE & cleanData$VK_ej_trombrof_TBI_72h == "ja", TRUE, FALSE))
  
  cleanData$CPR <- cleanData$VK_hlr_thorak == "ja" | cleanData$Fr1.12 == 1
 
  cleanData$AF_mass_transf <- cleanData$VK_mass_transf == "ja"
 
  
  cleanData$AF_lever_och_mjaltskada <- cleanData$VK_leverskada == "ja" | cleanData$VK_mjaltskada == "ja"
  #combining VK_mjaltskada and VK_leverskada into one auditfilter and turning the values to boolean
 
 
  cleanData$ofi <- cleanData$ofi == "yes"
  
  afCols <- grep("^AF_", names(cleanData))
  
  cleanData$AF_all <- apply(cleanData[afCols], 1, function(row) {
    ifelse(any(row == TRUE, na.rm = TRUE), TRUE, 
           ifelse(all(is.na(row)), NA, FALSE))
  })
  return(cleanData)
}