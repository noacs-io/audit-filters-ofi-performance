library(rofi)
library(dplyr)
noacsr::source_all_functions()
selected.auditfilters <- selected_auditfilters(data)
## Import data and select variables
clean.data <- clean_data(selected.auditfilters)
## clean the data from NA values 
display.result <- display_result(clean.data)
print(display.result)
