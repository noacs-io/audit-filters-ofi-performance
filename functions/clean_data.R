clean_data <- function(dataOFI){
  dataOFI[] <- lapply(dataOFI, function(x) if(is.character(x)) tolower(x) else x)
  ##CONVERT TO LOWER
  underFifteen <- dataOFI$pt_age_yrs <= 14
  notScreenedForOFI <- is.na(dataOFI$ofi)
  includedData <- dataOFI[!(underFifteen | notScreenedForOFI),]
  ##ONLY INCLUDED DATA SELECTED
  listOfAuditFilters2 <- c("VK_hlr_thorak","VK_sap_less90","VK_leverskada",
                           "VK_gcs_less9_ej_intubTE","VK_mjaltskada","VK_mer_30min_DT",
                           "VK_mass_transf","VK_mer_60min_interv","VK_iss_15_ej_iva",
                           "VK_ej_trombrof_TBI_72h","VK_iss_15_ej_TE")
  var99 <- c("ed_emergency_pro","TraumaAlarmAtHospital","pt_age_yrs","pre_sbp_value","ed_sbp_value","hosp_vent_days","hosp_los_days","iva_dagar_n","iva_vardtillfallen_n")
  includedData[, -which(names(includedData) %in% var99)][includedData[, -which(names(includedData) %in% var99)] == 99 ] <- NA
  includedData[includedData == 999] <- NA
  includedData[includedData == 9999] <- NA
  
  ##REMOVE 999 AND 9999
  includedData$ed_intubated <- ifelse(includedData$ed_intubated == 1, "Yes","No")
  includedData$Gender <- ifelse(includedData$Gender == "k","Female","Male")
  includedData$res_survival <- ifelse(includedData$res_survival == 1,"Yes","No")
  print(sum(is.na(includedData$res_survival)))
  includedData$Tr_Nivå <- ifelse(includedData$Tr_Nivå == (2 | 4 | 33), "No", "Yes")
  includedData$host_care_level[includedData$host_care_level == 1] <- "Emergency department"
  includedData$host_care_level[includedData$host_care_level == 2] <- "General ward"
  includedData$host_care_level[includedData$host_care_level == 3] <- "Surgical ward"
  includedData$host_care_level[includedData$host_care_level == 4] <- "Specialist ward/Intermediate ward"
  includedData$host_care_level[includedData$host_care_level == 5] <- "Intensive care unit"
  

  includedData[,listOfAuditFilters2][includedData[,listOfAuditFilters2] == "nn"] <- NA
  ##REMOVE "nn"
  return(includedData)
}