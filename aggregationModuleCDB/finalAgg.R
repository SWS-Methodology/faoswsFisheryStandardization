## load the library
library(faosws)
library(faoswsUtil)
library(data.table)

#-- Token QA ----
if(CheckDebug()){
  
  library(faoswsModules)
  SETTINGS = ReadSettings("sws.yml")
  
  ## If you're not on the system, your settings will overwrite any others
  R_SWS_SHARE_PATH = SETTINGS[["share"]]
  
  ## Define where your certificates are stored
  SetClientFiles(SETTINGS[["certdir"]])
  
  ## Get session information from SWS. Token must be obtained from web interface
  GetTestEnvironment(baseUrl = SETTINGS[["server"]],
                     token = '27ded447-71ec-413b-bcd4-87669ac20c70')
  
}

message('CDB aggregation : getting parameters')

sessionKey_CDB = swsContext.datasets[[1]]

sessionCountry0 <- swsContext.datasets[[1]]@dimensions$geographicAreaM49_fi@keys 
sessionISSCFC0 <- swsContext.datasets[[1]]@dimensions$measuredItemISSCFC@keys 
sessionElement <- swsContext.datasets[[1]]@dimensions$measuredElement@keys 

region_level <- swsContext.computationParams$region_level # World, continent, regions

group_level <- swsContext.computationParams$group_level # lev 3, lev 2, lev 1, grandtotal

min_year <- swsContext.computationParams$min_year
max_year <- swsContext.computationParams$max_year
sel_years <- as.character(min_year:max_year)

message('CDB aggregation : loading datatables')

country2cont <- ReadDatatable('fi_commodity_aggregates_geo')

if(region_level %in% c('World', 'All')){
  
  M49 <- GetCodeList("FisheriesCommodities", "commodities_total","geographicAreaM49_fi" )[type == 'country',code]
  sessionCountry <- M49
  
} else if(region_level == 'Continent'){
  
  continents <- unique(country2cont[un_codecountry %in% sessionCountry0]$un_code)
  sessionCountry <- country2cont[un_code %in% continents]$un_codecountry
  
} else if(region_level == 'Region'){
  
  regions <- unique(country2cont[un_codecountry %in% sessionCountry0]$un_codereg)
  sessionCountry <- country2cont[un_codereg %in% regions]$un_codecountry
  
} else if(region_level == 'None'){
  
  sessionCountry <- sessionCountry0
}

isscfc2fao <- ReadDatatable('fi_commodity_aggregates_comm')

if(group_level %in% c('Grandtotal', 'All')){
  
  sessionISSCFC <- GetCodeList("FisheriesCommodities", "commodities_total","measuredItemISSCFC" )[,code]
  
} else if(group_level == 'Level 1'){
  
  l1 <- unique(isscfc2fao[code %in% sessionISSCFC0]$fao_code)
  sessionISSCFC <- isscfc2fao[fao_code %in% l1]$code
  
} else if(group_level == 'Level 2'){
  
  l2 <- unique(isscfc2fao[code %in% sessionISSCFC0]$fao_codel2)
  sessionISSCFC <- isscfc2fao[fao_codel2 %in% l2]$code
  
} else if(group_level == 'Level 3'){
  
  l3 <- unique(isscfc2fao[code %in% sessionISSCFC0]$fao_codel3)
  sessionISSCFC <- isscfc2fao[fao_codel3 %in% l3]$code
  
} else if(group_level == 'None'){
  
  sessionISSCFC <- sessionISSCFC0
  
}

sessionCountry <- sessionCountry[sessionCountry != 680 ]
print(sessionCountry)
print(sessionISSCFC)

#-- Pulling data from Commodites dataset") ----
message('CDB aggregation : loading dataset')

  commodityDB0 <- data.table()
  
  for(i in 1:length(sessionCountry)){
    KeyComm <- DatasetKey(domain = "Fisheries Commodities", dataset = "commodities_total", dimensions = list(
      geographicAreaM49_fi = Dimension(name = "geographicAreaM49_fi", keys = sessionCountry[i]),
      measuredItemISSCFC = Dimension(name = "measuredItemISSCFC", keys = sessionISSCFC),
      measuredElement = Dimension(name = "measuredElement", keys = sessionElement),
      timePointYears = Dimension(name = "timePointYears", keys = sel_years)))
    
    commodityDBchunk <- GetData(KeyComm)
    commodityDB0 <- rbind(commodityDB0, commodityDBchunk)
    print(i)
  }
  
  commodityDB <- copy(commodityDB0)
  commodityDB$flagObservationStatus <- factor(commodityDB$flagObservationStatus,
                                              levels = c('M', 'O', 'N', '', 'X', 'T', 'E', 'I'), 
                                              ordered = TRUE)

  
  #-- Geographical aggregates ----
  
  message('CDB aggregation : geographical aggregation.')
  
  if(region_level == 'World'){
    
    commodityDBW <- copy(commodityDB)
    commodityGeo <- commodityDBW[ , c('geo', 'Value', 
                                         'flagObservationStatus',
                                         'flagMethod') := list('1', sum(Value, na.rm = TRUE),
                                                   max(flagObservationStatus), 
                                                   'x'),
                                     by = c('measuredItemISSCFC', 
                                            'measuredElement', 
                                            'timePointYears')]
    
    commodityGeo[ , geographicAreaM49_fi := NULL]
    setnames(commodityGeo, 'geo', 'geographicAreaM49_fi')
    
    
  } else if(region_level == 'Continent'){
    commodityDBC <- copy(commodityDB)
    commodityGeo <- merge(commodityDBC, country2cont[start_year < max_year & end_year > min_year, 
                                    .(un_code, un_codecountry)], 
          by.x = 'geographicAreaM49_fi', by.y = 'un_codecountry')
    
   
    commodityGeo <- commodityGeo[ , c('Value', 'flagObservationStatus',
                                        'flagMethod') := list(sum(Value, na.rm = TRUE),
                                                              max(flagObservationStatus), 
                                                              'x'),
                                    by = c('un_code', 'measuredItemISSCFC', 
                                           'measuredElement', 
                                           'timePointYears')]
    
    commodityGeo[ , geographicAreaM49_fi := NULL]
    setnames(commodityGeo, 'un_code', 'geographicAreaM49_fi')
    
  } else if(region_level == 'Region'){
    commodityDBR <- copy(commodityDB)
    
    commodityGeo <- merge(commodityDBR, country2cont[start_year < max_year & end_year > min_year, 
                                                  .(un_codereg, un_codecountry)], 
                        by.x = 'geographicAreaM49_fi', by.y = 'un_codecountry')
    
    
    commodityGeo <- commodityGeo[ , c('Value', 'flagObservationStatus',
                                  'flagMethod') := list(sum(Value, na.rm = TRUE),
                                                        max(flagObservationStatus), 
                                                        'x'),
                              by = c('un_codereg', 'measuredItemISSCFC', 
                                     'measuredElement', 
                                     'timePointYears')]
    
    commodityGeo[ , geographicAreaM49_fi := NULL]
    setnames(commodityGeo, 'un_codereg', 'geographicAreaM49_fi')
    
  } else if(region_level == 'All') {
    
    # World
    commodityDBW <- copy(commodityDB)
    commodityGeo1 <- commodityDBW[ , c('geo', 'Value', 
                                     'flagObservationStatus',
                                     'flagMethod') := list('1', sum(Value, na.rm = TRUE),
                                                           max(flagObservationStatus), 
                                                           'x'),
                                 by = c('measuredItemISSCFC', 
                                        'measuredElement', 
                                        'timePointYears')]
    
    commodityGeo1[ , geographicAreaM49_fi := NULL]
    setnames(commodityGeo1, 'geo', 'geographicAreaM49_fi')
    
    # Continent
    commodityDBC <- copy(commodityDB)
    commodityGeo2 <- merge(commodityDBC, country2cont[start_year < max_year & end_year > min_year, 
                                                    .(un_code, un_codecountry)], 
                          by.x = 'geographicAreaM49_fi', by.y = 'un_codecountry')
    
    
    commodityGeo2 <- commodityGeo2[ , c('Value', 'flagObservationStatus',
                                      'flagMethod') := list(sum(Value, na.rm = TRUE),
                                                            max(flagObservationStatus), 
                                                            'x'),
                                  by = c('un_code', 'measuredItemISSCFC', 
                                         'measuredElement', 
                                         'timePointYears')]
    
    commodityGeo2[ , geographicAreaM49_fi := NULL]
    setnames(commodityGeo2, 'un_code', 'geographicAreaM49_fi')
    
    # Region
    commodityDBR <- copy(commodityDB)
    commodityGeo3 <- merge(commodityDBR, country2cont[start_year < max_year & end_year > min_year, 
                                                    .(un_codereg, un_codecountry)], 
                          by.x = 'geographicAreaM49_fi', by.y = 'un_codecountry')
    
    
    commodityGeo3 <- commodityGeo3[ , c('Value', 'flagObservationStatus',
                                      'flagMethod') := list(sum(Value, na.rm = TRUE),
                                                            max(flagObservationStatus), 
                                                            'x'),
                                  by = c('un_codereg', 'measuredItemISSCFC', 
                                         'measuredElement', 
                                         'timePointYears')]
    
    commodityGeo3[ , geographicAreaM49_fi := NULL]
    setnames(commodityGeo3, 'un_codereg', 'geographicAreaM49_fi')
    
    # All
    
    commodityGeo <- rbind(commodityGeo1, commodityGeo2)
    commodityGeo <- rbind(commodityGeo, commodityGeo3)
    
  } else if(region_level == 'None'){
      
    commodityGeo <- commodityDB
    
    }

  setkey(commodityGeo)
  commodityGeo <- unique(commodityGeo)
  
  # if(any(sessionCountry0 != sessionCountry)){
  #   
  #   sepGeo <- commodityDB[geographicAreaM49_fi %in% sessionCountry0 ]
  #   commodityGeo <- rbind(commodityGeo, sepGeo)
  #   
  # }

  print(commodityGeo)
  
  #-- Commodities aggregates ----

  message('CDB aggregation : commodity aggregation.')
  
if(group_level == 'Grandtotal'){
  
  commodityGeoG <- copy(commodityGeo)
  commodityAgg <-  commodityGeoG[ , c('isscfc', 'Value', 
                                     'flagObservationStatus',
                                     'flagMethod') := list('FAOCLASSIFICATION',
                                                           sum(Value, na.rm = TRUE),
                                                           max(flagObservationStatus), 
                                                           'x'),
                                 by = c('geographicAreaM49_fi',
                                        'measuredElement', 
                                        'timePointYears')]
  
  commodityAgg[ , measuredItemISSCFC := NULL]
  setnames(commodityAgg, 'isscfc', 'measuredItemISSCFC')
  
} else if(group_level == 'Level 1'){
  commodityGeoL1 <- copy(commodityGeo)
  commodityAgg <-  merge(commodityGeoL1, isscfc2fao[ , .(fao_code, code)],
                         by.x = 'measuredItemISSCFC', by.y = 'code')
  
  commodityAgg <- commodityAgg[ , c('Value', 'flagObservationStatus',
                                    'flagMethod') := list(sum(Value, na.rm = TRUE),
                                                          max(flagObservationStatus), 
                                                          'x'),
                                by = c('geographicAreaM49_fi',
                                       'fao_code', 
                                       'measuredElement', 
                                       'timePointYears')]
  
  commodityAgg[ , measuredItemISSCFC := NULL]
  setnames(commodityAgg, 'fao_code', 'measuredItemISSCFC')
  
  
} else if(group_level == 'Level 2'){
  commodityGeoL2 <- copy(commodityGeo)
  commodityAgg <-  merge(commodityGeoL2, isscfc2fao[ , .(fao_codel2, code)],
                         by.x = 'measuredItemISSCFC', by.y = 'code')
  
  commodityAgg <- commodityAgg[ , c('Value', 'flagObservationStatus',
                                    'flagMethod') := list(sum(Value, na.rm = TRUE),
                                                          max(flagObservationStatus), 
                                                          'x'),
                                by = c('geographicAreaM49_fi',
                                       'fao_codel2',
                                       'measuredElement', 
                                       'timePointYears')]
  
  commodityAgg[ , measuredItemISSCFC := NULL]
  setnames(commodityAgg, 'fao_codel2', 'measuredItemISSCFC')
  
  
} else if(group_level == 'Level 3'){
  commodityGeoL3 <- copy(commodityGeo)
  commodityAgg <-  merge(commodityGeoL3, isscfc2fao[ , .(fao_codel3, code)],
                         by.x = 'measuredItemISSCFC', by.y = 'code')
  
  commodityAgg <- commodityAgg[ , c('Value', 'flagObservationStatus',
                                    'flagMethod') := list(sum(Value, na.rm = TRUE),
                                                          max(flagObservationStatus), 
                                                          'x'),
                                by = c('geographicAreaM49_fi',
                                       'fao_codel3',
                                       'measuredElement', 
                                       'timePointYears')]
  
  commodityAgg[ , measuredItemISSCFC := NULL]
  setnames(commodityAgg, 'fao_codel3', 'measuredItemISSCFC')
  
} else if(group_level == 'All') {
  
  # Grandtotal
  commodityGeoG <- copy(commodityGeo)
  commodityAgg1 <-  commodityGeoG[ , c('isscfc', 'Value', 
                                     'flagObservationStatus',
                                     'flagMethod') := list('FAOCLASSIFICATION',
                                                           sum(Value, na.rm = TRUE),
                                                           max(flagObservationStatus), 
                                                           'x'),
                                 by = c('geographicAreaM49_fi',
                                        'measuredElement', 
                                        'timePointYears')]
  
  commodityAgg1[ , measuredItemISSCFC := NULL]
  setnames(commodityAgg1, 'isscfc', 'measuredItemISSCFC')
  # Level 1
  commodityGeoL1 <- copy(commodityGeo)
  commodityAgg2 <-  merge(commodityGeoL1, isscfc2fao[ , .(fao_code, code)],
                         by.x = 'measuredItemISSCFC', by.y = 'code')
  
  commodityAgg2 <- commodityAgg2[ , c('Value', 'flagObservationStatus',
                                    'flagMethod') := list(sum(Value, na.rm = TRUE),
                                                          max(flagObservationStatus), 
                                                          'x'),
                                by = c('geographicAreaM49_fi',
                                       'fao_code', 
                                       'measuredElement', 
                                       'timePointYears')]
  
  commodityAgg2[ , measuredItemISSCFC := NULL]
  setnames(commodityAgg2, 'fao_code', 'measuredItemISSCFC')
  
  # Level 2
  commodityGeoL2 <- copy(commodityGeo)
  commodityAgg3 <-  merge(commodityGeoL2, isscfc2fao[ , .(fao_codel2, code)],
                         by.x = 'measuredItemISSCFC', by.y = 'code')
  
  commodityAgg3 <- commodityAgg3[ , c('Value', 'flagObservationStatus',
                                    'flagMethod') := list(sum(Value, na.rm = TRUE),
                                                          max(flagObservationStatus), 
                                                          'x'),
                                by = c('geographicAreaM49_fi',
                                       'fao_codel2',
                                       'measuredElement', 
                                       'timePointYears')]
  
  commodityAgg3[ , measuredItemISSCFC := NULL]
  setnames(commodityAgg3, 'fao_codel2', 'measuredItemISSCFC')
  
  # Level 3
  commodityGeoL3 <- copy(commodityGeo)
  commodityAgg4 <-  merge(commodityGeoL3, isscfc2fao[ , .(fao_codel3, code)],
                         by.x = 'measuredItemISSCFC', by.y = 'code')
  
  commodityAgg4 <- commodityAgg4[ , c('Value', 'flagObservationStatus',
                                    'flagMethod') := list(sum(Value, na.rm = TRUE),
                                                          max(flagObservationStatus), 
                                                          'x'),
                                by = c('geographicAreaM49_fi',
                                       'fao_codel3',
                                       'measuredElement', 
                                       'timePointYears')]
  
  commodityAgg4[ , measuredItemISSCFC := NULL]
  setnames(commodityAgg4, 'fao_codel3', 'measuredItemISSCFC')
  
  # All
  commodityAgg <- rbind(commodityAgg1, commodityAgg2)
  commodityAgg <- rbind(commodityAgg, commodityAgg3)
  commodityAgg <- rbind(commodityAgg, commodityAgg4)
  
  
} else if(group_level == 'None'){
  
  commodityAgg <- commodityGeo 
  
}
  
  setkey(commodityAgg)
  commodityAgg <- unique(commodityAgg)
  
  print(commodityAgg)
  
  # if(any(sessionISSCFC0 != sessionISSCFC)){
  #   
  #   sepComm <- commodityGeo[measuredItemISSCFC %in% sessionISSCFC0]
  #   
  #   commodityAgg <- rbind(commodityAgg, sepComm)
  #   
  # }
  
  message('CDB aggregation : saving data.')
  
  CONFIG <- GetDatasetConfig(sessionKey_CDB@domain, sessionKey_CDB@dataset)
  
  stats <- SaveData(domain = CONFIG$domain,
                    dataset = CONFIG$dataset,
                    data = commodityAgg,
                    waitTimeout = Inf)
  
  ##-- send Email with notification of correct execution ----
  
  ## Initiate email
  from = "sws@fao.org"
  to = swsContext.userEmail
  subject = "Aggregation plug-in has correctly run"
  body = paste("The plug-in has saved the data in your sessions. 
               Please note these results cannot be saved in the dataset as they are aggregates linked to virtual SWS codes.
               Best regards.")
  
  sendmailR::sendmail(from = from, to = to, subject = subject, msg = body)
  paste0("Email sent to ", swsContext.userEmail)
  
  msg <- paste0("Aggregation process completed successfully! ",
                       stats$inserted, " observations written, ",
                       stats$ignored, " weren't updated, ",
                       stats$discarded, " had problems.")

