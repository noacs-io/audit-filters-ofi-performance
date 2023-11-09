library(rofi)
library(dplyr)
library(ggplot2)
library(pROC)
library(gtsummary)
library(gt)
library(boot)
listOfAuditfilters <- c("AF_sap_less90","AF_death_30d","AF_iss_15_ej_TE",
                        "AF_mass_transf", "AF_gcs_less9_ej_intubTE", 
                        "AF_iss_15_ej_iva", "AF_mer_60_min_interv",
                        "AF_mer_30min_DT", "AF_hlr_thorak", "AF_lever_och_mjaltskada")
selectedAuditFilter <- listOfAuditfilters[1:10]

tableOfCalculatedData1 <- data.frame(Auditfilter = character(0),
                                     Number = numeric(0),
                                    Specificity = numeric(0),
                                    Sensitivity = numeric(0),
                                    AUC = numeric(0))
#A table of sensitivity and specificity and AUC

noacsr::source_all_functions()
selectedData <- select_data(data) 
#function that selects data that I am interested in
tableOneData <- select(selectedData, ofi, 
                       Gender,pt_age_yrs,ISS,  
                       ed_sbp_value, ed_gcs_sum,
                       dt_ed_first_ct, ed_intubated, res_survival,dt_ed_emerg_proc
                       )

#data inclusion for my table one
tableOne <- tbl_summary(tableOneData, 
                        by = ofi,
                        missing = "no",
                        type = all_dichotomous() ~ "categorical",
                        label = list(pt_age_yrs ~ "Age", 
                                     ed_sbp_value ~ "ED Systolic Blood Pressure", 
                                     ed_gcs_sum ~ "ED GCS",
                                     dt_ed_first_ct ~ "Time to first CT",
                                     ed_intubated ~ "Intubated at ED",
                                     res_survival ~ "Dead at 30 days",
                                     dt_ed_emerg_proc ~ "Time to definitive treatment")) %>% 
  add_p() %>%
  add_overall(last = TRUE) %>%
  modify_header(label = "**Characteristics**",
                stat_1 = "**No**, (N = {n})",
                stat_2 = "**Yes**, (N = {n})") %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**OFI**")
#plotting my table one


# Import data and select variables
for(auditFilter in selectedAuditFilter){
  twoVariableData <- select(selectedData, all_of(auditFilter), ofi)
  confidenceInterval <- confidence_interval(twoVariableData)
  #Function that calculates confidence interval for sensitivity and specificity
  calculateSensSpec <- calculate_sens_spec(twoVariableData)
  #Function that calculates sensitivity and specificity
  calculatedAUC <- calculate_AUC(twoVariableData)
  #Function that calculates AUC and confidence interval for AUC
  makeTableTwo <- make_table_two(calculatedSensSpec,confidenceInterval, calculateAUC)
  tableOfCalculatedData1 <- rbind(tableOfCalculatedData1,makeTableTwo)
  # bind together the values from calculate_data function into the table defined above
}
tableOfCalculatedData1[tableOfCalculatedData1 == "AF_sap_less90"] <- "SAP < 90"
tableOfCalculatedData1[tableOfCalculatedData1 == "AF_death_30d"] <- "Dead at 30 days"
tableOfCalculatedData1[tableOfCalculatedData1 == "AF_iss_15_ej_TE"] <- "ISS > 15 and no team activation"
tableOfCalculatedData1[tableOfCalculatedData1 == "AF_mass_transf"] <- "Massiv transfusion"
tableOfCalculatedData1[tableOfCalculatedData1 == "AF_gcs_less9_ej_intubTE"] <- "GCS<9 and not intubated"
tableOfCalculatedData1[tableOfCalculatedData1 == "AF_iss_15_ej_iva"] <- "ISS>15 not in intensiv care unit"
tableOfCalculatedData1[tableOfCalculatedData1 == "AF_mer_60_min_interv"] <- ">60 min until first intervention"
tableOfCalculatedData1[tableOfCalculatedData1 == "AF_mer_30min_DT"] <- ">30min until CT"
tableOfCalculatedData1[tableOfCalculatedData1 == "AF_hlr_thorak"] <- "CPR and thoracotomy"
tableOfCalculatedData1[tableOfCalculatedData1 == "AF_lever_och_mjaltskada"] <- "Liver or spleen injury"

tableTwo <- gt(tableOfCalculatedData1) %>% 
  cols_label(Number = "(N)",
             Specificity = "Specificity (%)",
             Sensitivity = "Sensitivity (%)") %>%
  tab_header(
    title = "Performance of auditfilters in predicting OFI") %>%
  cols_align(align = "left")
#design table

  tableTwo
  tableOne