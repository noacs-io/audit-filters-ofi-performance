library(rofi)
library(dplyr)
library(ggplot2)
library(pROC)

listOfAuditfilters <- c("VK_sap_less90","VK_mjaltskada", 
                        "VK_mass_transf", "VK_gcs_less9_ej_intubTE", 
                        "VK_iss_15_ej_iva", "VK_mer_60min_interv",
                        "VK_mer_30min_DT", "VK_hlr_thorak", "VK_leverskada")
# "VK_ej_trombrof_TBI_72h" funkar ej för att sensitivitet är 1 och specificitet är 0
#  "VK_avslutad" borde inte användas?

tableOfSensAndSpec <- data.frame(Auditfilter = character(0),
                                 Specificity = numeric(0),
                                 Sensitivity = numeric(0)
)
#A table of sensitivity and specificity

noacsr::source_all_functions()
selectedData <- select_data(data)
## Import data and select variables
for(auditFilter in listOfAuditfilters){
  cleanData <- clean_data(selectedData, auditFilter)
  #clean data by turning values to lower case and remove rows with NA values
  calculatedData <- calculate_data(cleanData, auditFilter)
  #calculate the sensitivity and spe
  tableOfSensAndSpec <- rbind(tableOfSensAndSpec,calculatedData)
  #bind together the values from calculate_data function into the table defined above
  rocOfData <- logistic_regression(cleanData, auditFilter)
  #draw ROC curves
}
print(tableOfSensAndSpec)
