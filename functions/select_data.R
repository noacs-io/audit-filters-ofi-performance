select_data <- function(combinedDataset) {
  
  selectedData <- select(combinedDataset,starts_with("AF_"), ofi,
                         Gender,ISS, pt_age_yrs, ed_sbp_value, ed_gcs_sum,
                         dt_ed_first_ct, ed_intubated,res_survival,dt_ed_emerg_proc,
                         host_care_level,Tr_Nivå)
  #select what i want
  return(selectedData)
}
