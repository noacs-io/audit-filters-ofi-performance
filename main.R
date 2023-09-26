library(rofi)
library(dplyr)
noacsr::source_all_functions()
selected.auditfilters <- selected_auditfilters(data)
## Import data and select variables
clean.data <- clean_data(selected.auditfilters)
## clean the data from NA values 
#calculate.data <- calculate_data("VK_sap_less90")
list.auditfilters <- c("VK_sap_less90", "VK_gcs_less9_ej_intubTE", "VK_iss_15_ej_iva", "VK_mer_60min_interv","VK_mer_30min_DT","VK_ej_trombrof_TBI_72h","VK_hlr_thorak","VK_mjaltskada", "VK_leverskada", "VK_mass_trans")

#print(calculate.data)

for (individual.auditfilters in list.auditfilters){
  calculated.data <- calculate_data(individual.auditfilters)
  print(calculated.data)
  
}
