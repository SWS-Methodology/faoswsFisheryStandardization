#-- Extraction rates calculation ----

eRcomputation <- function(data,  tree, 
                          oldEr = NULL, years){
  
  newYear <- max(as.numeric(years))
  ## the principal ingredient to compute the foodProcessing is the extraction rates
  ## for those items that already have a protected figures for 31 the default extractio rate 
  ## in the commodity tree has to be updated:
  
  dataProd <- data[measuredElementSuaFbs=="5510"]
  dataInput <- data[measuredElementSuaFbs=="5302"]
  
  # # get flags from input data with correspondent rows
  # flagsInput <- combineFlag(data = dataInput, flagObservationStatusVar = flagObservationStatus, flagMethodVar = flagMethod)
  # flagsInput <- data.table(flag = flagsInput, row = 1:nrow(flagsInput))
  # # Only keep protected flags
  # flagsProt <- flagsInput[flagsInput %in% getProtectedFlag() ]
  # 
  # # only keep input data with protected data otherwise take tree extraction rates
  # dataInput <- dataInput[as.vector(flagsProt$row), ]
  
  if(nrow(dataInput) > 0){
    computeExtractionRate <- merge(dataProd, dataInput, 
                                   by = c("geographicAreaM49_fi", "timePointYears", "measuredItemFaostat_L2", "availability"), 
                                   suffixes = c("_prod","_input"))
    computeExtractionRate[ , extraction_rate_C := Value_prod/Value_input]
    # computeExtractionRate <- computeExtractionRate[, .(measuredItemFaostat_L2, extraction_rate_C)]
    # setnames(computeExtractionRate,"measuredItemFaostat_L2","child")
    # computeExtractionRate <- unique(computeExtractionRate[ , extraction_rate_C := mean(extraction_rate_C), by = list(child)])
    # 
    setnames(computeExtractionRate,c("extraction_rate_C"), "Value")
    computeExtractionRate <- computeExtractionRate[, .(geographicAreaM49_fi, timePointYears, measuredItemFaostat_L2, availability, Value)]
    computeExtractionRate[ , ':=' (flagObservationStatus = "E", flagMethod = "i" ) ] 
    
    Er2add <- unique(data[ !measuredItemFaostat_L2 %in% primary, .(geographicAreaM49_fi, timePointYears,  measuredItemFaostat_L2, availability)])
    Er2add <- Er2add[ , measuredElementSuaFbs := '5423']
    Er2add <- merge(Er2add, computeExtractionRate, by = c("geographicAreaM49_fi", "timePointYears",
                                                          "measuredItemFaostat_L2", "availability"), all.x = TRUE)
    
    # if no official input check previous years
    if(!is.null(oldEr)){
    useOldEr <- merge(oldEr[timePointYears == as.character(newYear - 1), .(geographicAreaM49_fi, 
                                                                           measuredItemFaostat_L2, 
                                                                           Value, flagObservationStatus, 
                                                                           flagMethod)], 
                      Er2add[ timePointYears == as.character(newYear)],
                      suffixes = c('Previous', 'Last'),
                      by = c("geographicAreaM49_fi", "measuredItemFaostat_L2"),
                      all = TRUE)
    # If no value for last year then previous year value is selected
    useOldEr[is.na(ValueLast), c('ValueLast', 
                                 'flagObservationStatusLast', 
                                 'flagMethodLast') := list(ValuePrevious,
                                                           flagObservationStatusPrevious, 
                                                           flagMethodPrevious)]
    
    useOldEr[ , c("ValuePrevious", "flagObservationStatusPrevious",
                  "flagMethodPrevious") := NULL]
    
    setnames(useOldEr, c('ValueLast', 
                         'flagObservationStatusLast', 
                         'flagMethodLast'),
             c('Value', 
               'flagObservationStatus',
               'flagMethod'))
    
    # Put together last year and other years
    oldErAll <- rbind(useOldEr[ , availability:= NULL], oldEr[timePointYears != as.character(newYear)])
    
    # Insert default Er if needed
    Er2addAllEr <- merge(Er2add, tree[!is.na(extraction_rate) , .(child, extraction_rate)], 
                         by.x = c("measuredItemFaostat_L2"), by.y = c("child"), all.x = TRUE)
    Er2addAllEr[ , Value := ifelse(is.na(Value) | Value == 0, extraction_rate, Value)]
    Er2addAllEr[ , extraction_rate := NULL]
    Er2addAllEr[ , ':=' (flagObservationStatus = "E", flagMethod = "i" ) ]
    
    eRadded <- merge(Er2addAllEr, oldErAll, by = c("geographicAreaM49_fi", "measuredElementSuaFbs",
                                                   "measuredItemFaostat_L2", "timePointYears"),
                     all.x = TRUE, suffixes = c('Default', 'Series'))
    
    eRadded[!is.na(ValueSeries) & ValueSeries > 0, c('ValueDefault', 
                                   'flagObservationStatusDefault',
                                   'flagMethodDefault') := list(ValueSeries,
                                                                flagObservationStatusSeries,
                                                                flagMethodSeries)]
    
    eRadded[ , c("ValueSeries", "flagObservationStatusSeries",
                 "flagMethodSeries") := NULL]
    
    setnames(eRadded, c('ValueDefault', 
                        'flagObservationStatusDefault',
                        'flagMethodDefault'),
             c('Value', 
               'flagObservationStatus',
               'flagMethod'))
    
  } else {
    
    eRadded <- Er2add
    
  }
    setkey(eRadded)
    eRadded <- unique(eRadded)
    dataEr <- rbind(data, eRadded[!is.na(Value)])
    setkey(dataEr)
    dataEr <- unique(dataEr)
    # # compare eR substitute calculated one only if parent is primary 
    # # when secondary parent different as input expressed in primary equivalent
    # # If the extraction rate is calculated from protected data then this one is kept in the tree
    # treeNewER <- merge(tree ,computeExtractionRate, by=c("child"), all.x = TRUE) # by=c("timePointYears",  "child" ) from Francesca
    # treeNewER[!is.na(extraction_rate_C) & parent %in% primary, extraction_rate:=extraction_rate_C]
    # treeNewER[,c("extraction_rate_C"):=NULL]
    # treeNewER <- treeNewER[!is.na(child)]
  } else {
    
    # treeNewER <- tree
    
    Er2add <- unique(data[ !measuredItemFaostat_L2 %in% primary, .(geographicAreaM49_fi, timePointYears,  measuredItemFaostat_L2, availability)])
    Er2add[ , measuredElementSuaFbs := '5423']
    
    # if no official input check previous years
    if(!is.null(oldEr)){
    useOldEr <- merge(oldEr[timePointYears == as.character(newYear - 1), .(measuredItemFaostat_L2, Value, flagObservationStatus, flagMethod)], 
                      Er2add[ timePointYears == as.character(newYear)],
                      by = c("measuredItemFaostat_L2"),
                      all.y = TRUE)
    oldErAll <- rbind(useOldEr[ , availability:= NULL], oldEr[timePointYears != as.character(newYear - 1)])
    
    Er2addAllEr <- merge(Er2add, tree[!is.na(extraction_rate) , .(child, extraction_rate)], 
                         by.x = c("measuredItemFaostat_L2"), by.y = c("child"), all.x = TRUE)
    setnames(Er2addAllEr, "extraction_rate", "Value")
    Er2addAllEr[ , ':=' (flagObservationStatus = "E", flagMethod = "i" ) ]
    
    eRadded <- merge(Er2addAllEr, oldErAll, by = c("geographicAreaM49_fi", "measuredElementSuaFbs",
                                                   "measuredItemFaostat_L2", "timePointYears"),
                     all.x = TRUE, suffixes = c('Default', 'Series'))
    
    eRadded[!is.na(ValueSeries) & ValueSeries > 0, c('ValueDefault', 
                                                     'flagObservationStatusDefault',
                                                     'flagMethodDefault') := list(ValueSeries,
                                                                                  flagObservationStatusSeries,
                                                                                  flagMethodSeries)]
    
    eRadded[ , c("ValueSeries", "flagObservationStatusSeries",
                 "flagMethodSeries") := NULL]
    
    setnames(eRadded, c('ValueDefault', 
                        'flagObservationStatusDefault',
                        'flagMethodDefault'),
             c('Value', 
               'flagObservationStatus',
               'flagMethod'))
    } else {
      Er2addAllEr <- merge(Er2add, tree[!is.na(extraction_rate) , .(child, extraction_rate)], 
                           by.x = c("measuredItemFaostat_L2"), by.y = c("child"), all.x = TRUE)
      setnames(Er2addAllEr, "extraction_rate", "Value")
      Er2addAllEr[ , ':=' (flagObservationStatus = "E", flagMethod = "i" ) ]
      
      eRadded <- Er2addAllEr
      
    }
    setkey(eRadded)
    eRadded <- unique(eRadded)
    dataEr <- rbind(data, eRadded[!is.na(Value)])
    setkey(dataEr)
    dataEr <- unique(dataEr)
  }
  # return(treeNewER)
  return(dataEr)
}