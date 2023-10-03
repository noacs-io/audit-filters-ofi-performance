library(rofi)
library(dplyr)
library(ggplot2)
library(pROC)
list.auditfilters <- c("VK_sap_less90","VK_mjaltskada", "VK_mass_transf", "VK_gcs_less9_ej_intubTE", "VK_iss_15_ej_iva", "VK_mer_60min_interv","VK_mer_30min_DT","VK_ej_trombrof_TBI_72h","VK_hlr_thorak", "VK_leverskada", "VK_avslutad")

noacsr::source_all_functions()
selected.auditfilters <- selected_auditfilters(data)
## Import data and select variables
clean.data <- clean_data(selected.auditfilters)
## clean the data from NA values 
display.result <- display_result(clean.data)
print(clean.data)
logistic.regression.of.result <- logistic_regression(clean.data, list.auditfilters)
print(display.result)
