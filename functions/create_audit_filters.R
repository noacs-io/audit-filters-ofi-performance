create_audit_filters <- function(cleanData){
  cleanData$AF_sap_less90 <- cleanData$ed_sbp_value < 90 | cleanData$ed_sbp_rtscat <= 3
  cleanData$AF_sap_less90[is.na(cleanData$AF_sap_less90)] <- FALSE
  #VK_sap_less90
  cleanData$AF_gcs_less9_ej_intubTE <- cleanData$ed_gcs_sum < 9 & cleanData$ed_intubated != "Yes"
  cleanData$AF_gcs_less9_ej_intubTE[is.na(cleanData$AF_gcs_less9_ej_intubTE)] <- FALSE
  #VK_gcs_less9_ej_intubTE
  cleanData$AF_mer_30min_DT <- cleanData$dt_ed_first_ct > 30
  cleanData$AF_mer_30min_DT[is.na(cleanData$AF_mer_30min_DT)] <- FALSE
  #VK_mer_30min_DT
  cleanData$AF_mer_60_min_interv <- cleanData$dt_ed_emerg_proc > 60
  cleanData$AF_mer_60_min_interv[is.na(cleanData$AF_mer_60_min_interv)] <- FALSE
  #VK_mer_60_min_interv
  cleanData$AF_iss_15_ej_iva <- cleanData$ISS >= 15 & cleanData$host_care_level != 5
  cleanData$AF_iss_15_ej_iva[is.na(cleanData$AF_iss_15_ej_iva)] <- FALSE
  #VK_iss_15_ej_iva
  cleanData$AF_death_30d <- cleanData$res_survival == "Yes"
  cleanData$AF_death_30d[is.na(cleanData$AF_death_30d)] <- FALSE
  #death after 30d
  cleanData$VK
  cleanData$AF_iss_15_ej_TE <- cleanData$ISS >= 15 & (cleanData$Tr_Nivå == 2 | cleanData$Tr_Nivå == 4 | cleanData$Tr_Nivå == 33)
  cleanData$AF_iss_15_ej_TE[is.na(cleanData$iss_15_ej_TE)] <- FALSE
  #ISS 15 och ingen trauma larm
  cleanData$AF_ej_trombrof_TBI_72h <- cleanData$VK_ej_trombrof_TBI_72h == "ja"
  #turning values into boolean
  cleanData$AF_hlr_thorak <- cleanData$VK_hlr_thorak == "ja"
  #turning values into boolean
  cleanData$AF_mass_transf <- cleanData$VK_mass_transf == "ja"
  #turning values into boolean
  cleanData$AF_lever_och_mjaltskada <- cleanData$VK_leverskada == "ja" | cleanData$VK_mjaltskada == "ja"
  #combining VK_mjaltskada and VK_leverskada into one auditfilter and turning the values to boolean
  #turning values into boolean
  cleanData$ofi <- cleanData$ofi == "yes"
  
  afCols <- grep("^AF_", names(cleanData))
  cleanData[afCols] <- lapply(cleanData[afCols], function(x) replace(x, is.na(x), FALSE))

  return(cleanData)
}