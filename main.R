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
                        "AF_mer_30min_DT", "AF_hlr_thorak", 
                        "AF_lever_och_mjaltskada", "AF_ej_trombrof_TBI_72h")
listOfAuditFiltersClean <- c("SBP < 90", "Dead at 30 days", "ISS > 15 and no team activation", 
                             "Massive transfusion", "GCS < 9 and not intubated", "ISS > 15 and not in ICU", 
                             "> 60 min until first intervention", "> 30 min until first CT",
                             "CPR and thoracotomy", "Liver or spleen injury", 
                             "No anticoagulantia within 72 hours after TBI")
## viktigt att det ska vara samma ordning ^
selectedAuditFilter <- listOfAuditFilters[1:11]


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
cleanData <- clean_data(importDataOfi)
dataWithAuditFilters <- create_audit_filters(cleanData)
missingValues <- missing_values(dataWithAuditFilters)
selectedData <- select_data(dataWithAuditFilters)
#function that selects data that I am interested in
createFlowchart <- create_flowchart(importDataOfi)
tableFourData <- data.frame(listOfAuditFiltersClean,
                            missingValues)
tableFiveData <- data.frame(Auditfilter = c("SBP > 90", "Dead at 30 days", 
                              "ISS > 15 and no team activation", 
                              "GCS < 9 and not intubated", 
                              "ISS > 15 and not in ICU", 
                              "> 60 min until first intervention", 
                              "> 30 min until first CT",
                              "Massive transfusion", "CPR and thoracotomy", 
                              "Liver or spleen injury", 
                              "No anticoagulantia within 72 hours after TBI"), 
                            Design = c("Manually created","Manually created", "Manually created","Manually created","Manually created", "Manually created", "Manually created", "Original", "Original", "Original", "Original"))
tableOneData <- select(selectedData, ofi, 
                       Gender,pt_age_yrs,ISS,  
                       ed_sbp_value, ed_gcs_sum,
                       dt_ed_first_ct, ed_intubated, 
                       res_survival,dt_ed_emerg_proc,
                       host_care_level,Tr_Nivå
                       )
#data inclusion for my table one

#plotting my table one
counter <- 1
for(auditFilter in selectedAuditFilter){
  twoVariableData <- select(selectedData, all_of(auditFilter), ofi)
  confidenceInterval <- confidence_interval(twoVariableData)
  #Function that calculates confidence interval for sensitivity and specificity
  calculateSensSpec <- calculate_sens_spec(twoVariableData)
  #Function that calculates sensitivity and specificity
  aucc <- boot_strap_AUC(twoVariableData)
  #Function that calculates AUC and confidence interval for AUC
  calculateAUC <- calculate_AUC(twoVariableData, listOfAuditFiltersClean, counter)
  makeTableTwo <- make_table_two(calculateSensSpec,confidenceInterval, calculateAUC, listOfAuditFiltersClean, counter)
  tableOfCalculatedData1 <- rbind(tableOfCalculatedData1,makeTableTwo)
  makeTableThree <- make_table_three(calculateAUC,confidenceInterval, listOfAuditFiltersClean, counter)
  tableOfCalculatedData2 <- rbind(tableOfCalculatedData2,makeTableThree)
  # bind together the values from calculate_data function into the table defined above
  counter <- counter + 1
}
tableOne <- tableOneData %>%
  mutate(host_care_level = factor(host_care_level, levels = c("Emergency department", "General ward", "Surgical ward", "Specialist ward/Intermediate ward", "Intensive care unit")))  %>%
  tbl_summary(by = ofi,
                        missing = "ifany",
                        type = all_dichotomous() ~ "categorical",
                        label = list(pt_age_yrs ~ "Age", 
                                     ed_sbp_value ~ "ED Systolic Blood Pressure", 
                                     ed_gcs_sum ~ "ED GCS",
                                     dt_ed_first_ct ~ "Time to first CT",
                                     ed_intubated ~ "Intubated at ED",
                                     res_survival ~ "Dead at 30 days",
                                     dt_ed_emerg_proc ~ "Time to definitive treatment",
                                     host_care_level ~ "Highest level of care",
                                     Tr_Nivå ~ "Trauma team activation ")) %>% 
  add_overall(last = TRUE, col_label = "**Overall** (N = {N})")%>%
  modify_footnote(everything() ~ NA, abbreviation = FALSE) %>%
  modify_header(label = "",
                stat_1 = "**No**, (N = {n})",
                stat_2 = "**Yes**, (N = {n})") %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**OFI**") %>%
  bold_labels()
############TABLE ONE#######################
tableThree <- tableOfCalculatedData1 %>%
  gt() %>% 
  cols_label(Auditfilter = "Audit filter",
             Number = "(N)",
             Specificity = "Specificity (%)",
             Sensitivity = "Sensitivity (%)") %>%
  cols_align(align = "left") %>%
  tab_source_note("Definition of abbreviations:
  OFI = Opportunity for Improvement;
  SBP = Systolic Blood Pressure;
  ISS = Injury Severity Score;
  GCS = Glascow Coma Scale;
  ICU = Intensive Care Unit;
  CT = Computer Tomopgraphy;
  ED = Emergency Department; 
  CPR = Cardiopulmonary Resuscitation;
  TBI = Traumatic Brain Injury
                  ")
############TABLE TWO#######################
tableFour <- gt(tableOfCalculatedData2) %>% 
  cols_label(PValue = "p-value",
             Auditfilter = "Audit filter") %>%
  cols_align(align = "left") %>%
  tab_source_note("Definition of abbreviations: 
  OFI = Opportunity for Improvement; 
  AUC = Area under the receiver operating characteristic curve;
  SBP = Systolic Blood Pressure;
  ISS = Injury Severity Score;
  GCS = Glascow Coma Scale;
  ICU = Intensive Care Unit;
  CT = Computer Tomopgraphy;
  ED = Emergency Department; 
  CPR = Cardiopulmonary Resuscitation;
  TBI = Traumatic Brain Injury
  ")
############TABLE THREE#######################
tableTwo <- gt(tableFourData) %>%
  cols_label(listOfAuditFiltersClean = "Audit filters",
             missingValues = "Missing values n (%)") %>%
  cols_align(align = "left") %>%
  tab_source_note("Definition of abbreviations: 
  OFI = Opportunity for Improvement; 
  SBP = Systolic Blood Pressure;
  ISS = Injury Severity Score;
  GCS = Glascow Coma Scale;
  ICU = Intensive Care Unit;
  CT = Computer Tomopgraphy;
  ED = Emergency Department; 
  CPR = Cardiopulmonary Resuscitation;
  TBI = Traumatic Brain Injury
  ")
############TABLE FOUR#######################
tableFive <- gt(tableFiveData) %>%
  cols_label(Auditfilter = "Audit filter",
             Design = "Manually created/original") %>%
  cols_align(align = "left") %>%
  tab_source_note("Definition of abbreviations: 
  OFI = Opportunity for Improvement; 
  SBP = Systolic Blood Pressure;
  ISS = Injury Severity Score;
  GCS = Glascow Coma Scale;
  ICU = Intensive Care Unit;
  CT = Computer Tomopgraphy;
  ED = Emergency Department; 
  CPR = Cardiopulmonary Resuscitation;
  TBI = Traumatic Brain Injury
  ")
  
  tableOne 
  tableTwo
  tableThree
  tableFour
  tableFive
  
  