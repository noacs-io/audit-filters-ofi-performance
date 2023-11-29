clean_data <- function(dataOFI,auditFilter){
  dataOFI[] <- lapply(dataOFI, function(x) if(is.character(x)) tolower(x) else x)
  ##CONVERT TO LOWER
  underFifteen <- dataOFI$pt_age_yrs <= 14
  notScreenedForOFI <- is.na(dataOFI$ofi)
  includedData <- dataOFI[!(underFifteen | notScreenedForOFI),]
  ##ONLY INCLUDED DATA SELECTED
  
  var99 <- c("ed_emergency_pro","TraumaAlarmAtHospital","pt_age_yrs","pre_sbp_value","ed_sbp_value","hosp_vent_days","hosp_los_days","iva_dagar_n","iva_vardtillfallen_n")
  includedData[, -which(names(includedData) %in% var99)][includedData[, -which(names(includedData) %in% var99)] == 99 ] <- NA
  includedData[includedData == 999] <- NA
  includedData[includedData == 9999] <- NA
  ##REMOVE 999 AND 9999
  includedData$ed_intubated <- ifelse(includedData$ed_intubated == 1, "Yes","No")
  includedData$Gender <- ifelse(includedData$Gender == "k","Female","Male")
  includedData$res_survival <- ifelse(includedData$res_survival == 1,"Yes","No")
 
  uniqueAuditFilterValues <- unique(as.vector(as.matrix(includedData[,auditFilter])))
  print(uniqueAuditFilterValues)
 
  includedData[,auditFilter][includedData[,auditFilter] == "nn"] <- NA
  ##REMOVE "nn"
  
  return(includedData)
}