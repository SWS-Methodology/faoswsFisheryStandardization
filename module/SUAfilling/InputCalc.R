#-- Input calculation ----

inputComputation <- function(data){ #, treeNewER){
  # ### Compute the input (element 31) for each children
  # data1 <- copy(data)
  # # keep only the availability which is important to compute the ShareDownUp
  # setkey(data1)
  # data1 <- unique(data1[,.(geographicAreaM49_fi, timePointYears, measuredItemFaostat_L2, measuredElement,Value)])
  # setnames(data1, c("Value", "measuredElement", "measuredItemFaostat_L2"), 
  #          c("Value_child", "measuredElement_child" ,"child"))
  # 
  # ## I consider that the only commodity that may be play the role of parent are the PRIMARY
  # treePrimary <- treeNewER[parent %in%  primary]
  # treePrimary[, child:=as.character(child)]
  # 
  # # calculate input
  # data1 <- merge(data1, treePrimary, by=c("child"), allow.cartesian = TRUE)
  # data1[measuredElement_child=="5510", input:=Value_child/extraction_rate]
  # 
  # # isolate the input element
  # data_compute31= melt(data1,
  #                      id.vars = c("geographicAreaM49_fi", "timePointYears", "child"),
  #                      measure.vars = "input",
  #                      value.name = "Value" ,
  #                      variable.name = "measuredElement", variable.factor = FALSE)
  # 
  # 
  # # set input == '5302'
  # data_compute31[measuredElement=="input",measuredElement:="5302"]
  # setnames(data_compute31, "child", "measuredItemFaostat_L2")
  # data_compute31 <- data_compute31[!is.na(Value)]
  # data_compute31[ , ':='(availability = NA, flagObservationStatus = 'E', flagMethod = 'i', FBSsign = 0)]
  # 
  # # Remove all input data from the original data and add the only data31 part
  # # existing data are included as computed with computed extraction rates
  # # other input data are computed starting from given extraction rates
  # dataNo31 <- data[measuredElement!="5302"]
  # SUAinput <- rbind(dataNo31, data_compute31[,.(geographicAreaM49_fi, timePointYears,  measuredItemFaostat_L2, availability, measuredElement, Value)]) #rbind(data, data_compute31) #
  # return(SUAinput)
  input <-  data[ !measuredItemFaostat_L2 %in% primary & measuredElementSuaFbs == '5302', ]
  Er <- data[ !measuredItemFaostat_L2 %in% primary & measuredElementSuaFbs == '5423', ]
  Prod <- data[ !measuredItemFaostat_L2 %in% primary & measuredElementSuaFbs == '5510', ]
  
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


  # set input == '5302'
  data_compute31[measuredElementSuaFbs=="input",measuredElementSuaFbs:="5302"]
  data_compute31[ , ':='(flagObservationStatus = 'E', flagMethod = 'i', FBSsign = 0)]
  
  
  comp31 <- merge(data_compute31, input, by = c("geographicAreaM49_fi", 
                                "timePointYears",  
                                "measuredItemFaostat_L2", 
                                "availability", 
                                "measuredElementSuaFbs"), all = TRUE,
                  suff = c('', 'Official'))
  
  comp31[!is.na(ValueOfficial), c('Value','flagObservationStatus','flagMethod'):= list(ValueOfficial, 
                                                                                       flagObservationStatusOfficial,
                                                                                       flagMethodOfficial) ]
  
  comp31 <- comp31[ , c('ValueOfficial', 'flagObservationStatusOfficial',
                       'flagMethodOfficial') := NULL]
  
  # Remove all input data from the original data and add the only data31 part
  # existing data are included as computed with computed extraction rates
  # other input data are computed starting from given extraction rates
  dataNo31 <- data[measuredElementSuaFbs!="5302"]
  SUAinput <- rbind(dataNo31, comp31[,.(geographicAreaM49_fi, timePointYears,  measuredItemFaostat_L2, availability, measuredElementSuaFbs, Value, flagObservationStatus, flagMethod)]) #rbind(data, data_compute31) #
  return(SUAinput)
  
}