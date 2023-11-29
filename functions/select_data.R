select_data <- function(combinedDataset) {
  combinedDataset$AF_sap_less90 <- combinedDataset$ed_sbp_value < 90 | combinedDataset$ed_sbp_rtscat <= 3
  combinedDataset$AF_sap_less90[is.na(combinedDataset$AF_sap_less90)] <- FALSE
  #VK_sap_less90
  combinedDataset$AF_gcs_less9_ej_intubTE <- combinedDataset$ed_gcs_sum < 9 & combinedDataset$ed_intubated != "Yes"
  combinedDataset$AF_gcs_less9_ej_intubTE[is.na(combinedDataset$AF_gcs_less9_ej_intubTE)] <- FALSE
  #VK_gcs_less9_ej_intubTE
  combinedDataset$AF_mer_30min_DT <- combinedDataset$dt_ed_first_ct > 30
  combinedDataset$AF_mer_30min_DT[is.na(combinedDataset$AF_mer_30min_DT)] <- FALSE
  #VK_mer_30min_DT
  combinedDataset$AF_mer_60_min_interv <- combinedDataset$dt_ed_emerg_proc > 60
  combinedDataset$AF_mer_60_min_interv[is.na(combinedDataset$AF_mer_60_min_interv)] <- FALSE
  #VK_mer_60_min_interv
  combinedDataset$AF_iss_15_ej_iva <- combinedDataset$ISS >= 15 & combinedDataset$host_care_level != 5
  combinedDataset$AF_iss_15_ej_iva[is.na(combinedDataset$AF_iss_15_ej_iva)] <- FALSE
  #VK_iss_15_ej_iva
  combinedDataset$AF_death_30d <- combinedDataset$res_survival == "Yes"
  combinedDataset$AF_death_30d[is.na(combinedDataset$AF_death_30d)] <- FALSE
  #death after 30d
  combinedDataset$AF_iss_15_ej_TE <- combinedDataset$ISS >= 15 & (combinedDataset$Tr_Nivå == 2 | combinedDataset$Tr_Nivå == 4)
  combinedDataset$AF_iss_15_ej_TE[is.na(combinedDataset$iss_15_ej_TE)] <- FALSE
  #ISS 15 och ingen trauma larm
  combinedDataset$AF_ej_trombrof_TBI_72h <- combinedDataset$VK_ej_trombrof_TBI_72h == "ja"
  #turning values into boolean
  combinedDataset$AF_hlr_thorak <- combinedDataset$VK_hlr_thorak == "ja"
  #turning values into boolean
  combinedDataset$AF_mass_transf <- combinedDataset$VK_mass_transf == "ja"
  #turning values into boolean
  combinedDataset$AF_lever_och_mjaltskada <- combinedDataset$VK_leverskada == "ja" | combinedDataset$VK_mjaltskada == "ja"
  #combining VK_mjaltskada and VK_leverskada into one auditfilter and turning the values to boolean
  #turning values into boolean
  combinedDataset$ofi <- combinedDataset$ofi == "yes"
  
  af.cols <- grep("^AF_", names(combinedDataset))
  combinedDataset[af.cols] <- lapply(combinedDataset[af.cols], function(x) replace(x, is.na(x), FALSE))
  
  selectedData <- select(combinedDataset,starts_with("AF_"), ofi,
                         Gender,ISS, pt_age_yrs, ed_sbp_value, ed_gcs_sum,
                         dt_ed_first_ct, ed_intubated,res_survival,dt_ed_emerg_proc )
  
  
  

  #select what i want
  return(selectedData)
}
