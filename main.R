library(rofi)
library(dplyr)
library(ggplot2)
library(gtsummary)
library(gt)
library(boot)
library(magrittr)
library(knitr)
#library(Gmisc, quietly = TRUE)
library(glue)
library(htmlTable)
library(grid)
library(gridExtra)
library(DiagrammeR)
##NA auditfilter = FALSE

listOfAuditFilters <- c("AF_sap_less90","AF_death_30d","AF_iss_15_ej_TE",
                        "AF_mass_transf", "AF_gcs_less9_ej_intubTE", 
                        "AF_iss_15_ej_iva", "AF_mer_60_min_interv",
                        "AF_mer_30min_DT", 
                        "AF_lever_och_mjaltskada", "AF_ej_trombrof_TBI_72h", "AF_all")
listOfAuditFiltersClean <- c("SBP < 90", "Dead at 30 days", "ISS > 15 and no team activation", 
                             "Massive transfusion", "GCS < 9 and not intubated", "ISS > 15 and not in ICU", 
                             "> 60 min until first intervention", "> 30 min until first CT", "Liver or spleen injury", 
                             "No anticoagulants within 72 hours after TBI", "All")
## viktigt att det ska vara samma ordning ^
DefOfAbbreviations <- "Definition of abbreviations: SBP = Systolic Blood Pressure; ISS = Injury Severity Score; GCS = Glascow Coma Scale; ICU = Intensive Care Unit; CT = Computer Tomography; ED = Emergency Department; CPR = Cardiopulmonary Resuscitation; TBI = Traumatic Brain Injury"

selectedAuditFilter <- listOfAuditFilters[1:11]

tableKappa <- data.frame(Auditfilter = character(0),
                         Sensitivity = numeric(0),
                         Specificity = numeric(0),
                         PPV = numeric(0),
                         NPV = numeric(0))
tableAuditFilter <- data.frame(Auditfilter = character(0),
                               Total = numeric(0),
                               Number = numeric(0),
                               Missing = numeric(0),
                               Manual = character(0))

noacsr::source_all_functions()
importDataOfi <- import_data_ofi(data) #
cleanData <- clean_data(importDataOfi)
dataWithAuditFilters <- create_audit_filters(cleanData)
selectedData <- select_data(dataWithAuditFilters)

createFlowchart <- create_flowchart(importDataOfi)

tableOneData <- select(selectedData, ofi, 
                       Gender,pt_age_yrs,ISS,  
                       ed_sbp_value, ed_gcs_sum,
                       dt_ed_first_ct, ed_intubated, 
                       res_survival,dt_ed_emerg_proc,
                       host_care_level,Tr_Nivå
                       )

#data inclusion for my table one
counter <- 1 #set counter for for loop
manualOrOriginal <- character(0)
for(auditFilter in selectedAuditFilter){
  
  twoVariableData <- select(selectedData, auditFilter, ofi)
  twoVariableDataNaOmit <- na.omit(twoVariableData)
  if(auditFilter == "AF_iss_15_ej_TE"){
    totalValuesIncluded <- sum(selectedData$ISS_over_15_T, na.rm = TRUE)
    missingValue <- sum(selectedData$ISS_over_15_T_M)
  } else if(auditFilter == "AF_gcs_less9_ej_intubTE"){
    totalValuesIncluded <- sum(selectedData$GCS_under_9, na.rm = TRUE)
    missingValue <- sum(selectedData$GCS_under_9_M)
  } else if(auditFilter == "AF_ej_trombrof_TBI_72h"){
    totalValuesIncluded <- sum(selectedData$TBI, na.rm = TRUE)
    missingValue <- sum(selectedData$TBI_M)
  }else if(auditFilter == "AF_iss_15_ej_iva"){
    totalValuesIncluded <- sum(selectedData$ISS_over_15, na.rm = TRUE)
    missingValue <- sum(selectedData$ISS_over_15_M)
  }else{
    totalValuesIncluded <- nrow(twoVariableDataNaOmit)
    missingValue <- sum(is.na(twoVariableData[1]))
  }
  numberOfTrue <- sum(twoVariableDataNaOmit[[1]])
  
  if (counter %in% c(1,2,3,5,6,7,8,10)){
    manualOrOriginal <- "manual"
  } else if (counter %in% c(4,9)){
    manualOrOriginal <- "original"
  } else{
    manualOrOriginal <- " "
  }
  confidenceIntervalKappa <- confidence_interval_kappa(twoVariableDataNaOmit)
  confidenceIntervalSensitivitySpecificity <- confidence_interval_sens_spec(twoVariableDataNaOmit)
  tableAuditFilterResult <- data.frame(Auditfilter = listOfAuditFiltersClean[counter],
                                       Total = paste(c(totalValuesIncluded," (", round(totalValuesIncluded/8309 * 100, digits = 1), "%) "), collapse = ""),
                                       Number = paste(c(numberOfTrue," (", round(numberOfTrue/totalValuesIncluded*100, digits = 1), "%) "), collapse = ""),
                                       Missing = paste(c(missingValue," (", round(missingValue/8309 * 100, digits = 1), "%) "), collapse = ""),
                                       Manual = manualOrOriginal)
  tableAuditFilter <- rbind(tableAuditFilter,tableAuditFilterResult)
  
  tableKappaResult <- data.frame(Auditfilter = listOfAuditFiltersClean[counter],
                                 Sensitivity = paste(c(confidenceIntervalSensitivitySpecificity[1], " (", confidenceIntervalSensitivitySpecificity[2], "-", confidenceIntervalSensitivitySpecificity[3], ") "), collapse = ""),
                                 Specificity = paste(c(confidenceIntervalSensitivitySpecificity[4], " (", confidenceIntervalSensitivitySpecificity[5], "-", confidenceIntervalSensitivitySpecificity[6], ") "), collapse = ""),
                                 PPV = paste(c(confidenceIntervalSensitivitySpecificity[7], " (", confidenceIntervalSensitivitySpecificity[8], "-", confidenceIntervalSensitivitySpecificity[9], ") "), collapse = ""),
                                 NPV = paste(c(confidenceIntervalSensitivitySpecificity[10], " (", confidenceIntervalSensitivitySpecificity[11], "-", confidenceIntervalSensitivitySpecificity[12], ") "), collapse = ""))
  tableKappa <- rbind(tableKappa,tableKappaResult)
  
  counter <- counter + 1
}

tableK <- gt(tableKappa)  %>% 
  cols_label(Auditfilter = "Audit filter",
             Specificity = "Specificity (%)",
             Sensitivity = "Sensitivity (%)",
             PPV = "PPV (%)",
             NPV = "NPV (%)") %>%
  cols_align(align = "left") %>%
  tab_source_note(DefOfAbbreviations)

tableK %>% gtsave(filename = "tabk.html")
tableAF <- gt(tableAuditFilter) %>%
  cols_label(Auditfilter = "Audit filter",
             Total = "Sub population",
             Number = "Flagged",
             Missing = "Missing (%)",
             Manual = "Manual") %>%
  cols_align(align = "left") %>%
  tab_source_note(DefOfAbbreviations)
tableAF %>% gtsave(filename = "tabAF.html")

tableOne <- tableOneData %>%
  mutate(host_care_level = factor(host_care_level, levels = c("Emergency department", 
                                                              "General ward", 
                                                              "Surgical ward", 
                                                              "Specialist ward/Intermediate ward", 
                                                              "Intensive care unit")))  %>%
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
  modify_table_styling(footnote = "Definition of abbreviations:
                         OFI = Opportunity for Improvement;
                         ISS = Injury Severity Score;
                         ED = Emergency Department;
                         SBP = Systolic Blood Pressure;
                         GCS = Glascow Coma Scale;
                        CT = Computer Tomography;") %>%
  modify_header(label = "",
                stat_1 = "**No**, (N = {n})",
                stat_2 = "**Yes**, (N = {n})") %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**OFI**") %>%
  bold_labels() %>%
  add_p(test = list(all_continuous() ~ "wilcox.test")) %>%
  bold_p() 

  