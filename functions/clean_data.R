clean_data <- function(dataSet, auditFilter){
  dataSet[,auditFilter] <- tolower(dataSet[,auditFilter])
  dataSet[,"ofi"] <- tolower(dataSet[,"ofi"])
  filterValues <- function(x) {
    ifelse(x %in% c("yes", "no", "ja", "nej"), x, NA)
  }
  
  # Apply the function to each element of the dataframe
  dataSet[] <- lapply(dataSet, filterValues)
  dataSet <- na.omit(dataSet[,c(auditFilter,"ofi")])
  return(dataSet)
}