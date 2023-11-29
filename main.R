library(rofi)
library(dplyr)
library(ggplot2)
library(pROC)
library(gtsummary)
library(gt)
library(boot)
library(magrittr)
library(Gmisc, quietly = TRUE)
library(glue)
library(htmlTable)
library(grid)
library(gridExtra)
library(DiagrammeR)

##NA auditfilter = FALSE

listOfAuditFilters <- c("AF_sap_less90","AF_death_30d","AF_iss_15_ej_TE",
                        "AF_mass_transf", "AF_gcs_less9_ej_intubTE", 
                        "AF_iss_15_ej_iva", "AF_mer_60_min_interv",
                        "AF_mer_30min_DT", "AF_hlr_thorak", "AF_lever_och_mjaltskada")
selectedAuditFilter <- listOfAuditFilters[1:10]
listOfAuditFilters2 <- c("VK_hlr_thorak","VK_sap_less90","VK_leverskada",
                  "VK_gcs_less9_ej_intubTE","VK_mjaltskada","VK_mer_30min_DT",
                  "VK_mass_transf","VK_mer_60min_interv","VK_iss_15_ej_iva",
                  "VK_ej_trombrof_TBI_72h","VK_iss_15_ej_TE")

tableOfCalculatedData1 <- data.frame(Auditfilter = character(0),
                                     Number = numeric(0),
                                    Specificity = numeric(0),
                                    Sensitivity = numeric(0),
                                    AUC = numeric(0),
                                    PValue = character(0))

#A table of sensitivity and specificity
tableOfCalculatedData2 <- data.frame(Auditfilter = character(0),
                                     AUC = numeric(0),
                                     PValue = character(0))
#A table of AUC
noacsr::source_all_functions()
importDataOfi <- import_data_ofi(data)
cleanData <- clean_data(importDataOfi,listOfAuditFilters2)

selectedData <- select_data(cleanData)
#function that selects data that I am interested in
createFlowchart <- create_flowchart(importDataOfi)

tableOneData <- select(selectedData, ofi, 
                       Gender,pt_age_yrs,ISS,  
                       ed_sbp_value, ed_gcs_sum,
                       dt_ed_first_ct, ed_intubated, res_survival,dt_ed_emerg_proc
                       )

#data inclusion for my table one
tableOne <- tbl_summary(tableOneData, 
                        by = ofi,
                        missing = "ifany",
                        type = all_dichotomous() ~ "categorical",
                        label = list(pt_age_yrs ~ "Age", 
                                     ed_sbp_value ~ "ED Systolic Blood Pressure", 
                                     ed_gcs_sum ~ "ED GCS",
                                     dt_ed_first_ct ~ "Time to first CT",
                                     ed_intubated ~ "Intubated at ED",
                                     res_survival ~ "Dead at 30 days",
                                     dt_ed_emerg_proc ~ "Time to definitive treatment")) %>% 
  add_overall(last = TRUE, col_label = "**Overall** (N = {N})")%>%
  modify_footnote(everything() ~ NA, abbreviation = FALSE) %>%
  modify_caption("Table 1. Demographic and Clinical Characteristics of patients screened for OFI.") %>%
  modify_header(label = "",
                stat_1 = "**No**, (N = {n})",
                stat_2 = "**Yes**, (N = {n})") %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**OFI**") %>%
  bold_labels()
#plotting my table one
for(auditFilter in selectedAuditFilter){
  cat("VALUES REGARDING:", auditFilter)
  twoVariableData <- select(selectedData, all_of(auditFilter), ofi)
  #twoVariableData[auditFilter][is.na(twoVariableData[auditFilter])] <- FALSE
  confidenceInterval <- confidence_interval(twoVariableData)
  #Function that calculates confidence interval for sensitivity and specificity
  calculateSensSpec <- calculate_sens_spec(twoVariableData)
  #Function that calculates sensitivity and specificity
  aucc <- boot_strap_AUC(twoVariableData)
  #Function that calculates AUC and confidence interval for AUC
  calculateAUC <- calculate_AUC(twoVariableData)
  makeTableTwo <- make_table_two(calculateSensSpec,confidenceInterval, calculateAUC)
  tableOfCalculatedData1 <- rbind(tableOfCalculatedData1,makeTableTwo)
  makeTableThree <- make_table_three(calculateAUC,confidenceInterval)
  tableOfCalculatedData2 <- rbind(tableOfCalculatedData2,makeTableThree)
  # bind together the values from calculate_data function into the table defined above
  print("#######################COMPLETE#########################")
}
tableOfCalculatedData1[tableOfCalculatedData1 == "AF_sap_less90"] <- "SAP < 90"
tableOfCalculatedData1[tableOfCalculatedData1 == "AF_death_30d"] <- "Dead at 30 days"
tableOfCalculatedData1[tableOfCalculatedData1 == "AF_iss_15_ej_TE"] <- "ISS > 15 and no team activation"
tableOfCalculatedData1[tableOfCalculatedData1 == "AF_mass_transf"] <- "Massiv transfusion"
tableOfCalculatedData1[tableOfCalculatedData1 == "AF_gcs_less9_ej_intubTE"] <- "GCS<9 and not intubated"
tableOfCalculatedData1[tableOfCalculatedData1 == "AF_iss_15_ej_iva"] <- "ISS>15 not in intensive care unit"
tableOfCalculatedData1[tableOfCalculatedData1 == "AF_mer_60_min_interv"] <- ">60 min until first intervention"
tableOfCalculatedData1[tableOfCalculatedData1 == "AF_mer_30min_DT"] <- ">30min until CT"
tableOfCalculatedData1[tableOfCalculatedData1 == "AF_hlr_thorak"] <- "CPR and thoracotomy"
tableOfCalculatedData1[tableOfCalculatedData1 == "AF_lever_och_mjaltskada"] <- "Liver or spleen injury"

tableTwo <- gt(tableOfCalculatedData1) %>% 
  cols_label(Number = "(N)",
             Specificity = "Specificity (%)",
             Sensitivity = "Sensitivity (%)") %>%
  cols_align(align = "left") %>%
  tab_caption("Table 2. Performance of auditfilter in predicting OFI") %>%
  tab_source_note("Definition of abbreviations: 
  OFI = Opportunity for Improvement; 
  ED = Emergency Department; 
  GCS = Glascow Coma Scale; 
  ISS = Injury Severity Score; 
  CPR = Cardiopulmonary Resuscitation; 
  AUC = Area under the receiver operating characteristic curve.")
#design table
tableThree <- gt(tableOfCalculatedData2) %>% 
  cols_label(PValue = "p-value") %>%
  cols_align(align = "left") %>%
  tab_caption("Table 3. Area under the Receiver operating characteristic curve of individual auditfilters") %>%
  tab_source_note("Definition of abbreviations: 
  OFI = Opportunity for Improvement; 
  ED = Emergency Department; 
  GCS = Glascow Coma Scale; 
  ISS = Injury Severity Score; 
  CPR = Cardiopulmonary Resuscitation; 
  AUC = Area under the receiver operating characteristic curve.")

  
  tableOne
  tableTwo
  tableThree
  