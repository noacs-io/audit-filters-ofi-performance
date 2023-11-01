library(rofi)
library(dplyr)
library(ggplot2)
library(pROC)
library(gtsummary)
listOfAuditfilters <- c("AF_sap_less90","AF_death_30d","AF_iss_15_ej_TE", 
                        "AF_mass_transf", "AF_gcs_less9_ej_intubTE", 
                        "AF_iss_15_ej_iva", "AF_mer_60_min_interv",
                        "AF_mer_30min_DT", "AF_hlr_thorak", "AF_lever_och_mjaltskada")
#  "VK_avslutad" borde inte användas?
# "AF_ej_trombrof_TBI_72h"
tableOfCalculatedData <- data.frame(Auditfilter = character(0),
                                    Number = numeric(0),
                                    Specificity = numeric(0),
                                    Sensitivity = numeric(0),
                                    AUC = numeric(0),
                                    CI_lower_AUC = character(0),
                                    CI_upper_AUC = character(0),
                                    CI_lower_sens = character(0),
                                    CI_upper_sens = character(0),
                                    CI_lower_spec = character (0),
                                    CI_upper_spec = character(0)
)
#A table of sensitivity and specificity

noacsr::source_all_functions()
selectedData <- select_data(data)
tableOneData <- select(selectedData, ofi, 
                       Deceased, Gender,pt_age_yrs,ISS,  
                       ed_sbp_value, ed_gcs_sum,
                       dt_ed_first_ct, ed_intubated, res_survival
                       )

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
tableOne


# Import data and select variables
for(auditFilter in listOfAuditfilters){
  calculatedData <- calculate_data(selectedData, auditFilter)
  # calculate the sensitivity and specificity
  tableOfCalculatedData <- rbind(tableOfCalculatedData,calculatedData)
  # bind together the values from calculate_data function into the table defined above
}
print(tableOfCalculatedData)
show_header_names(tableOne)


             
