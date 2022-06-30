#-- Extraction rates calculation ----

eRcomputation <- function(data,  tree, 
                          oldEr = NULL, years){
  
  newYear <- max(as.numeric(years))
  ## the principal ingredient to compute the foodProcessing is the extraction rates
  ## for those items that already have a protected figures for 31 the default extractio rate 
  ## in the commodity tree has to be updated:
  
  dataProd <- data[measuredElementSuaFbs=="5510"]
  dataInput <- data[measuredElementSuaFbs=="5302"]
  
  # Take all secondary products
  Er0 <- unique(data[ !measuredItemFaostat_L2 %in% primary, .(geographicAreaM49_fi, timePointYears,  measuredItemFaostat_L2, availability)])
  Er0[ , measuredElementSuaFbs := '5423']
  
  if(nrow(dataInput) > 0){
    computeExtractionRate <- merge(dataProd, dataInput, 
                                   by = c("geographicAreaM49_fi", "timePointYears", 
                                          "measuredItemFaostat_L2", "availability"), 
                                   suffixes = c("_prod","_input"))
    computeExtractionRate[ , extraction_rate_C := Value_prod/Value_input]
     
    setnames(computeExtractionRate,c("extraction_rate_C"), "Value")
    computeExtractionRate <- computeExtractionRate[, .(geographicAreaM49_fi, timePointYears, measuredItemFaostat_L2, availability, Value)]
    computeExtractionRate[ , ':=' (flagObservationStatus = "I", flagMethod = "i" ) ] 
    
    input2Er <- merge(Er0, computeExtractionRate, by = c("geographicAreaM49_fi", "timePointYears",
                                                          "measuredItemFaostat_L2", "availability"), all.x = TRUE)
  } else {
    # If no Input then nothing happens
    input2Er <- Er0[ , ':=' (Value = 0, flagObservationStatus = "M", flagMethod = "u" ) ] 
  }
  
  # Historical data
  if(!is.null(oldEr)){
    
    if(nrow(oldEr) > 0){
    
    old2newEr <- merge(input2Er, oldEr, by = c('geographicAreaM49_fi', 'measuredElementSuaFbs',
                                            'measuredItemFaostat_L2', 'timePointYears'), 
                       all.x = TRUE, suffixes = c('', 'Series'))
    
    old2newEr[is.na(Value) | Value == 0, c('Value', 
                                           'flagObservationStatus',
                                           'flagMethod') := list(ValueSeries,
                                                                 flagObservationStatusSeries,
                                                                 flagMethodSeries)]
    
    old2newEr[ , c("ValueSeries", "flagObservationStatusSeries",
                 "flagMethodSeries") := NULL]
    
    # Carry forward are flagged as (E, t)
    carryforward <- data.table()
    for(i in 1:length(years)){
      previous <- oldEr[timePointYears == as.character(as.numeric(years[i])-1)]
      missing <- old2newEr[timePointYears == years[i]][is.na(Value) | Value == 0]
      
      # Only previous available values taken
      carryforward2append <- merge(previous, missing, by = c('geographicAreaM49_fi',
                                                             'measuredElementSuaFbs',
                                                             'measuredItemFaostat_L2'),
                                   suffixes = c('Prev', ''))
      carryforward2append[ , c('Value', 
                               'flagObservationStatus',
                               'flagMethod') := list(ValuePrev,
                                                     flagObservationStatusPrev,
                                                     flagMethodPrev)]
      carryforward2append[ , names(carryforward2append)[grepl('Prev', names(carryforward2append))] := NULL]
      
      carryforward <- rbind(carryforward, carryforward2append)
      }
    
    ErFromData <- merge(old2newEr, carryforward, by = c('geographicAreaM49_fi',
                                                        'measuredElementSuaFbs',
                                                        'measuredItemFaostat_L2',
                                                        'timePointYears',
                                                        'availability'),
                        all.x = TRUE, suffixes = c('', 'CF'))
    
    ErFromData[is.na(Value) | Value == 0, c('Value', 
                                            'flagObservationStatus',
                                            'flagMethod') := list(ValueCF,
                                                                  'E', 't')]
    ErFromData[ , names(ErFromData)[grepl('CF', names(ErFromData))] := NULL]
    
  
    } else {
    ErFromData <- input2Er
  }
    
  } else{
    ErFromData <- input2Er
  }
  
  # Add default Er if still any missing
  DefEr <- merge(ErFromData, tree[!is.na(extraction_rate) , .(child, extraction_rate)], 
                       by.x = c("measuredItemFaostat_L2"), by.y = c("child"), all.x = TRUE)
  # Flag for default extraction rate is (I, c) = Imputed, Copied from elsewhere in the working system
  DefEr[is.na(Value) | Value == 0, c('Value', 
                                         'flagObservationStatus',
                                         'flagMethod') := list(extraction_rate,
                                                               'I', 'c')]
  DefEr[ , extraction_rate := NULL]
  
  eRadded <- DefEr[!is.na(Value)]
  setkey(eRadded)
  eRadded <- unique(eRadded)
  eRadded <- eRadded[, Value := round(Value, 6)]
  dataEr <- rbind(data[measuredElementSuaFbs != '5423'], eRadded)
  setkey(dataEr)
  dataEr <- unique(dataEr)
}
