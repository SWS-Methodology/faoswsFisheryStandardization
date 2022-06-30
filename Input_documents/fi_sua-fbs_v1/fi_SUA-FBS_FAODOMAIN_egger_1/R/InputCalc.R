#-- Input calculation ----

inputComputation <- function(data){ #, treeNewER){
 
  # Isolate needed elements
  # Previous input available
  input <-  data[ !measuredItemFaostat_L2 %in% primary & measuredElementSuaFbs == '5302', ]
  
  #Er and production needed to compute input if no previous data
  Er <- data[ !measuredItemFaostat_L2 %in% primary & measuredElementSuaFbs == '5423', ]
  Prod <- data[ !measuredItemFaostat_L2 %in% primary & measuredElementSuaFbs == '5510', ]
  
  # Calculate Input
  InputCalc <- merge(Prod, Er, by = c("geographicAreaM49_fi",
                                      "timePointYears", "measuredItemFaostat_L2",
                                      "availability"), suffixes = c("_prod", "_Er"))
  InputCalc <- InputCalc[!is.na(Value_Er)]
  if(nrow(InputCalc[is.na(Value_Er)]) > 0 ){
  message('Missing extraction rates for some Ics groups')
  }
  
  InputCalc[ , input := Value_prod / Value_Er]
  
  data_compute31 <- melt(InputCalc,
                       id.vars = c("geographicAreaM49_fi", "timePointYears",  "measuredItemFaostat_L2", "availability"),
                       measure.vars = "input",
                       value.name = "Value" ,
                       variable.name = "measuredElementSuaFbs", variable.factor = FALSE)

  
  data_compute31[measuredElementSuaFbs=="input",measuredElementSuaFbs:="5302"]
  data_compute31[ , ':='(flagObservationStatus = 'I', flagMethod = 'i', FBSsign = 0)]
  
  # See if any official input
  comp31 <- merge(data_compute31, input, by = c("geographicAreaM49_fi", 
                                "timePointYears",  
                                "measuredItemFaostat_L2", 
                                "availability", 
                                "measuredElementSuaFbs"), all = TRUE,
                  suff = c('', 'Official'))
  
  # If previous data is not NA then it is assigned as input 
  # Note: the Er should have been computed as ratio between Production and Input
  comp31[!is.na(ValueOfficial), c('Value','flagObservationStatus','flagMethod'):= list(ValueOfficial, 
                                                                                       flagObservationStatusOfficial,
                                                                                       flagMethodOfficial)]
  
  comp31 <- comp31[ , c('ValueOfficial', 'flagObservationStatusOfficial',
                       'flagMethodOfficial') := NULL]
  
  # Remove all input data from the original data and add the only data31 part
  # existing data are included as computed with computed extraction rates
  # other input data are computed starting from given extraction rates
  dataNo31 <- data[measuredElementSuaFbs!="5302"]
  SUAinput <- rbind(dataNo31, comp31[,.(geographicAreaM49_fi, timePointYears,  measuredItemFaostat_L2, availability, measuredElementSuaFbs, Value, flagObservationStatus, flagMethod)]) #rbind(data, data_compute31) #
  return(SUAinput)
  
}