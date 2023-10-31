select_data <- function(dataSet) {
  data <- rofi::import_data(test = TRUE)
  #import database
  combinedDataset <- rofi::merge_data(data, test = TRUE)
  #Merge data
  combinedDataset$ofi <- rofi::create_ofi(combinedDataset)
  #Add OFI to database
  
  combinedDataset[] <- lapply(combinedDataset, function(x) if(is.character(x)) tolower(x) else x)
  #Turn everything into lower case
  
  combinedDataset <- combinedDataset[!(is.na(combinedDataset$ofi)), ]
  #Remove rows where no OFI value is assigned
  numberofrows <- nrow(combinedDataset)
  print(numberofrows)
  
  combinedDataset$AF_sap_less90 <- combinedDataset$ed_sbp_value < 90 | combinedDataset$ed_sbp_rtscat <= 3
  combinedDataset$AF_sap_less90[is.na(combinedDataset$AF_sap_less90)] <- FALSE
  #VK_sap_less90
  combinedDataset$AF_gcs_less9_ej_intubTE <- combinedDataset$ed_gcs_sum < 9 & combinedDataset$ed_intubated != 1
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
  combinedDataset$AF_death_30d <- combinedDataset$res_survival == 1
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
  combinedDataset$ofi <- combinedDataset$ofi == "yes"
  combinedDataset$ofi[is.na(combinedDataset$ofi)] <- FALSE
  #turning values into boolean
  combinedDataset$de
  selectedData <- select(combinedDataset,starts_with("AF_"), ofi, 
                         Deceased, Gender,ISS, pt_age_yrs, ed_sbp_value, ed_gcs_sum,
                         dt_ed_first_ct, ed_intubated,res_survival )
  selectedData$ed_intubated <- ifelse(selectedData$ed_intubated == 1, "No","Yes")
  selectedData$Deceased <- ifelse(selectedData$Deceased == "true", "Yes","No")
  selectedData$Gender <- ifelse(selectedData$Gender == "k","Women","Men")
  selectedData$res_survival <- ifelse(selectedData$res_survival == 1,"Dead","Alive")
  #select what i want
  return(selectedData)
}
