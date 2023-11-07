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
                                    Specificity = numeric(0),
                                    Sensitivity = numeric(0),
                                    AUC = numeric(0))
#A table of sensitivity and specificity and AUC

noacsr::source_all_functions()
selectedData <- select_data(data) 
#function that selects data that I am interested in
tableOneData <- select(selectedData, ofi, 
                       Deceased, Gender,pt_age_yrs,ISS,  
                       ed_sbp_value, ed_gcs_sum,
                       dt_ed_first_ct, ed_intubated, res_survival
                       )

#data inclusion for my table one
tableOne <- tbl_summary(tableOneData, 
                        by = ofi,
                        missing = "no",
                        label = list(pt_age_yrs ~ "Age", 
                                     ed_sbp_value ~ "SBP at ED", 
                                     ed_gcs_sum ~ "GCS at ED",
                                     dt_ed_first_ct ~ "Time to first CT",
                                     ed_intubated ~ "Intubated at ED",
                                     res_survival ~ "30 day mortality")) %>% 
  
  add_n() %>% 
  add_p() %>%
  modify_header(label = "**Characteristics**",
                stat_1 = "**No**",
                stat_2 = "**Yes**") %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**OFI**")
#plotting my table one


# Import data and select variables
for(auditFilter in selectedAuditFilter){
  twoVariableData <- select(selectedData, auditFilter, ofi)
  confidenceInterval <- confidence_interval(twoVariableData)
  #Function that calculates confidence interval for sensitivity and specificity
  calculateSensSpec <- calculate_sens_spec(twoVariableData)
  #Function that calculates sensitivity and specificity
  calculatedAUC <- calculate_AUC(twoVariableData)
  #Function that calculates AUC and confidence interval for AUC
  AUCResult <- round(calculatedAUC[2], digits = 2)
  AUCCIlowerResult <- round(calculatedAUC[1],digits = 2)
  AUCCIupperResult<- round(calculatedAUC[3], digits = 2)
  sensitivityResultFinal <- paste(c(calculateSensSpec[1]," (",confidenceInterval[1],"-",confidenceInterval[2],") "), collapse = "")
  specificityResultFinal <- paste(c(calculateSensSpec[2]," (",confidenceInterval[3],"-",confidenceInterval[4],") "), collapse = "")
  AUCResultFinal <- paste(c(calculatedAUC[2]," (",calculatedAUC[1],"-",calculatedAUC[3],") "), collapse = "")
  tableResult <- data.frame(Auditfilter = auditFilter,
                        Specificity = specificityResultFinal,
                        Sensitivity = sensitivityResultFinal,
                        AUC = AUCResultFinal)
  tableOfCalculatedData1 <- rbind(tableOfCalculatedData1,tableResult)
  # bind together the values from calculate_data function into the table defined above
}
tableTwo <- gt(tableOfCalculatedData1) %>% 
  cols_label(Specificity = "Specificity (%)",
             Sensitivity = "Sensitivity (%)") %>%
  tab_header(
    title = "Performance of auditfilters in predicting OFI") %>%
  cols_align(align = "left")
#missing = "no",
#label = list(AF_iss_15_ej_TE ~ "ISS>15 no trauma larm", 
 #            AF_sap_less90 ~ "SAP<90", 
  #           AF_death_30d ~ "30day mortality",
   #          AF_mass_transf ~ "Massiv transfusion",
    #         AF_gcs_less9_ej_intubTE ~ "GCS<9 and not intubated",
     #        AF_iss_15_ej_iva ~ "ISS>15 not in intensiv care unit",
      #       AF_mer_60_min_interv ~ ">60 min until intervention",
       #      AF_mer_30min_DT ~ ">30min until CT",
        #     AF_hlr_thorak ~ "CPR and thoracotomy",
         #    AF_lever_och_mjaltskada ~ "Liver or spleen injury"))            
