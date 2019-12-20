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

#-- Parameters ----
message("fi_SUA-FBS: Getting parameters")

sessionKey_fbsFias = swsContext.datasets[[1]]
sessionKey_suaUnb = swsContext.datasets[[2]]
sessionKey_suabal = swsContext.datasets[[3]]
sessionKey_fbsFaostat = swsContext.datasets[[4]]

# start and end year for standardization come from user parameters
sessionCountry <- '604' #swsContext.datasets[[1]]@dimensions$geographicAreaM49_fi@keys 
yearVals <- as.character(1995:2005) #swsContext.datasets[[1]]@dimensions$timePointYears@keys
year <- max(as.numeric(yearVals))
#sessionCountry <- '250' #as.character(c(36, 116, 250, 364, 372, 586, 604, 724, 840, 862, 894))

# yearVals <- as.character(2013:2017)
#-- Needed datasets ----

## Get global production (from Production environment)

message("fi_SUA-FBS: Pulling data from Global production")

keyDim <- c("geographicAreaM49_fi", "fisheriesAsfis", "measuredElement", "timePointYears")

KeyGlobal <- DatasetKey(domain = "Fisheries", dataset = "fi_global_production", dimensions = list(
  geographicAreaM49_fi = Dimension(name = "geographicAreaM49_fi", keys = sessionCountry),
  fisheriesAsfis = Dimension(name = "fisheriesAsfis", keys = GetCodeList("Fisheries", "fi_global_production","fisheriesAsfis" )[,code]),
  fisheriesCatchArea = Dimension(name = "fisheriesCatchArea", keys = GetCodeList("Fisheries", "fi_global_production","fisheriesCatchArea" )[,code]),
  measuredElement = Dimension(name = "measuredElement", keys = c("FI_001")),
  timePointYears = Dimension(name = "timePointYears", keys = yearVals )))

globalProduction <- GetData(KeyGlobal)

# Aggregate by fisheriesCatchArea
# Convert flags into ordinal factor so that simple aggregation is possible
# The function aggregateObservationFlag is too slow so flag are transformed into factors

globalProduction$flagObservationStatus <- factor(globalProduction$flagObservationStatus, 
                                                 levels = c('M', 'O', 'N', '', 'X', 'T', 'E', 'I'), 
                                                 ordered = TRUE)

globalProduction <- globalProduction[ , list(ValueAggr = sum(Value, na.rm = TRUE), 
                                             flagObservationStatusAggr = max(flagObservationStatus),
                                             flagMethodAggr = "s"),
                                      by=c("geographicAreaM49_fi",
                                           "fisheriesAsfis",
                                           "measuredElement",
                                           "timePointYears")]

setnames(globalProduction, names(globalProduction), c("geographicAreaM49_fi", "fisheriesAsfis",
                                                      "measuredElement", "timePointYears",
                                                      "Value", "flagObservationStatus",
                                                      "flagMethod"))

# Hard code change from FI_001 to 5510, both are Production in tonnes.
globalProduction$measuredElement <- ifelse(globalProduction$measuredElement == "FI_001", "5510", globalProduction$measuredElement)
if(any(globalProduction$measuredElement != "5510") ){
  message("Not all the elements in Global production dataset are FI_001")
}

## Get Commodities data (from QA environment)

message("fi_SUA-FBS: Pulling data from Commodites dataset")

commodityDB <- data.table()

for(i in 1:length(sessionCountry)){
KeyComm <- DatasetKey(domain = "Fisheries Commodities", dataset = "commodities_total", dimensions = list(
  geographicAreaM49_fi = Dimension(name = "geographicAreaM49_fi", keys = sessionCountry[i]),
  measuredItemISSCFC = Dimension(name = "measuredItemISSCFC", keys = GetCodeList("FisheriesCommodities", "commodities_total","measuredItemISSCFC" )[,code]),
  measuredElement = Dimension(name = "measuredElement", keys = GetCodeList("FisheriesCommodities", "commodities_total","measuredElement" )[,code]),
  timePointYears = Dimension(name = "timePointYears", keys = yearVals )))


## It should be the commodity DB validated
# KeyComm <- DatasetKey(domain = "Fisheries Commodities", dataset = "commodities_total_validated", dimensions = list(
#   geographicAreaM49_fi = Dimension(name = "geographicAreaM49_fi", keys = GetCodeList("FisheriesCommodities", "commodities_total","geographicAreaM49_fi" )[,code]),
#   measuredItemISSCFC = Dimension(name = "measuredItemISSCFC", keys = GetCodeList("FisheriesCommodities", "commodities_total","measuredItemISSCFC" )[,code]),
#   measuredElement = Dimension(name = "measuredElement", keys = GetCodeList("FisheriesCommodities", "commodities_total","measuredElement" )[,code]),
#   timePointYears = Dimension(name = "timePointYears", keys = GetCodeList("FisheriesCommodities", "commodities_total","timePointYears" )[,code])))

commodityDBchunk <- GetData(KeyComm)
commodityDB <- rbind(commodityDB, commodityDBchunk)
print(i)
}



commodityDB$flagObservationStatus <- factor(commodityDB$flagObservationStatus,
                                            levels = c('M', 'O', 'N', '', 'X', 'T', 'E', 'I'), 
                                            ordered = TRUE)

# No need of value elements "5622" and "5922"
commodityDB <- commodityDB[!measuredElement %in% c("5622", "5922", "5930", 
                                                   "5937", "5923", "5931",
                                                   "5940", "5630", "5637")]

# Re-export in Export
commodityDB <- commodityDB[measuredElement %in% c("5912", "5907",
                                                  "5906"), measuredElement := '5910']

# Other import
commodityDB <- commodityDB[measuredElement == "5607", measuredElement := '5610']


commodityDB <- commodityDB[ , c("Value", 
                                "flagObservationStatus", 
                                "flagMethod") := list(sum(Value, na.rm = TRUE),
                                                      max(flagObservationStatus),
                                                      '-'),
                            by = c('geographicAreaM49_fi',
                                   'timePointYears',
                                   'measuredItemISSCFC',
                                   'measuredElement')]

# #-- Connect to EBX for updated mapping ----
# 
# # user = Fishery-SOAP
# # password = W8EqZpDM4YBMKsrq 
# message("Connecting to EBX5 and creating mappings")
# 
# SetEBXCredentials(username = 'Fishery-SOAP',  password = 'W8EqZpDM4YBMKsrq',  new = T)
# 
# #-- Commodity mapping ----
# # Get ISSCFC codes
# isscfcList <- ReadEBXCodeList(cl_name = "ISSCFC")
# isscfc <- isscfcList[, .(Identifier, Code)]
# 
# # Get ICS codes
# icsList <- ReadEBXCodeList(cl_name = "FAOSTAT_Level2")
# ics <- icsList[ , .(Identifier, FAOSTAT_Code)]
# 
# # Get mapping ICS-ISSCFC
# ics2isscfc <- ReadEBXGroup(gr_name = "Group_FAOSTATL2_ISSCFC")
# 
# map_isscfc <- merge(ics2isscfc, ics, by.x = "Group", by.y = "Identifier")
# map_isscfc <- merge(map_isscfc, isscfc, by.x = "Member", by.y = "Identifier")
# 
# # Keep only needed dimensions and put right names
# map_isscfc <- map_isscfc[,.(FAOSTAT_Code, Code)]
# setnames(map_isscfc, names(map_isscfc), c("ics", "measuredItemISSCFC"))
# 
# #-- Species mapping ----
# # Get Asfis alphacodes
# asfisList <- ReadEBXCodeList(cl_name = "SpeciesItem")
# asfis <- asfisList[, .(Identifier, Alpha.Code, NAME_en)]
# 
# # Get ISSCAAP groups codes
# isscaapList <- ReadEBXCodeList(cl_name ="SpeciesIsscaapGroup")
# isscaap <- isscaapList[, .(Identifier, ISSCAAP_Code )]
# 
# # Get mapping ASFIS-ISSCAAP
# isscaap2asfis <- ReadEBXGroup(gr_name = "Group_IsscaapGrp_Item")
# 
# #Get ICS groups for Species
# ics4asfisList <- ReadEBXCodeList(cl_name = "SpeciesFaostat")
# ics4asfis <- ics4asfisList[, .(Identifier, Faostat.ID)]
# # Get mapping ICS-ISSCAAP
# ics2isscaap <- ReadEBXGroup(gr_name = "Group_Faostat_IsscaapGrp")
# setnames(ics2isscaap, names(ics2isscaap), c("ics", "isscaap"))
# 
# # Merge ASFIS-ISSCAAP
# map_asfis <- merge(isscaap2asfis, isscaap, by.x = "Group", by.y = "Identifier")
# map_asfis <- merge(map_asfis, asfis, by.x = "Member", by.y = "Identifier")
# # Add ICS
# map_asfis <- merge(map_asfis, ics2isscaap, by.x = "Group", by.y = "isscaap")
# map_asfis <- merge(map_asfis, ics4asfis, by.x = "ics", by.y = "Identifier")
# 
# # Keep only needed dimensions and put right names
# map_asfis <- map_asfis[, .(Faostat.ID, ISSCAAP_Code, Alpha.Code, NAME_en)]
# setnames(map_asfis, names(map_asfis), c("ics", "isscaap", "fisheriesAsfis", "description"))

# Get datatables that correspond to the EBX code
map_isscfc <- ReadDatatable('map_isscfc')
setnames(map_isscfc, "measured_item_isscfc", "measuredItemISSCFC")

map_asfis <- ReadDatatable('map_asfis')
setnames(map_asfis, "asfis", "fisheriesAsfis")

#-- Start processing global production ----
message('fi_SUA-FBS: Start processing Global Production')
# Map to ICS
globalProductionMapping <- merge(globalProduction, map_asfis, by = c("fisheriesAsfis"), all.x = TRUE)

if(any(is.na(globalProductionMapping$ics))){
  
  notMappedSpecies <- unique(globalProductionMapping[is.na(ics)]$fisheriesAsfis)
  message(paste("These species are not mapped to any ICS group:", 
                paste(notMappedSpecies, collapse = ", ")))
  msg2email1 <- paste("These species are not mapped to any ICS group:", 
                      paste(notMappedSpecies, collapse = ", "))
}
msg2email1 <- ifelse(any(is.na(globalProductionMapping$ics)), msg2email1, "")
# sum by ics, no species anymore

message('Species - ICS mapping table for exceptions.')
# Table of deviated species

new_map_asfis <- data.table()
for(i in seq(sessionCountry)){
new_country <-  ReadDatatable('gp_mapping', where = paste("country = '", sessionCountry[i], "'", sep = ''))
new_map_asfis <- rbind(new_map_asfis, new_country)
}

#################
if(nrow(new_map_asfis) > 0){
  
  new_map_asfis[ end_year == 'LAST', end_year := year]
  newMapping <- merge(globalProductionMapping, new_map_asfis, 
                      by.x = c('geographicAreaM49_fi', 'fisheriesAsfis'),
                      by.y = c('country', 'asfis'), all.x = TRUE)
  
  unchanged <- newMapping[is.na(from_code)]
  tochange <- newMapping[!is.na(from_code)]
  
  tochange[ timePointYears < end_year & timePointYears > start_year & ratio == 1, ics := to_code]
  tochange[ timePointYears < end_year & timePointYears > start_year & ratio != 1, Value := Value * (1-ratio)]
  
  duplicate <- tochange[ timePointYears < end_year & timePointYears > start_year & ratio != 1]
  if(nrow(duplicate) > 0){
    duplicate[ , c('ics', 'Value') := list(to_code, Value * ratio)]
  }
  
  changed <- rbind(tochange, duplicate)
  
  gpMap_new <- rbind(unchanged, changed) 
  gpMap_new[ , c('from_code', 'to_code', 'start_year', 'end_year', 'ratio'):= NULL]
  
} else {
  gpMap_new  <- globalProductionMapping
}

globalProductionAggr <- gpMap_new[, list(Value = sum(Value, na.rm = TRUE),
                                                       flagObservationStatus = max(flagObservationStatus),
                                                       flagMethod = "s"), by = list(geographicAreaM49_fi,
                                                                                    timePointYears,
                                                                                    measuredElement,
                                                                                    ics)]

# Remove ISSCAAP/ASFIS not mapped to any ICS (Sponges, Corals, Pearls): 
globalProductionAggr <- globalProductionAggr[!is.na(ics), ]

#-- Start processing commodity DB ----
message('fi_SUA-FBS: Start processing commodity DB')
# Map to ICS
commodityDBIcs <- merge(commodityDB, map_isscfc, by = "measuredItemISSCFC")
commodityDBIcs$measuredItemISSCFC <- as.character(commodityDBIcs$measuredItemISSCFC)

# Special commodities changes (China export for "034.4.1.5.2.45" goes to ICS 1543 instead of 1517,
# Greece export "034.1.5.9.90" goes to ICS 1514 instead of 1540) and 
# Some commodities are imported but not for food purposes, main example "ornamental fish". 
# These flows are deviated directly to "other utilizations"

# These hard-coded changes should be removed or put in a data.table if they increase
# commodityDBIcs[ geographicAreaM49_fi == "156" & 
#                   measuredItemISSCFC == "034.4.1.5.2.45" &
#                   measuredElement == "5910",]$ics <- "1543" # instead of 1517
# 
# commodityDBIcs[ geographicAreaM49_fi == "300" & 
#                   measuredItemISSCFC == "034.1.5.9.90" &
#                   measuredElement == "5910",]$ics <- "1514" # instead of 1540
message('Commodity - ICS mapping table for exceptions.')
# Account for commodity deviation

new_map_isscfc <- data.table()

for(i in seq(sessionCountry)){
 new_country_isscfc <-  ReadDatatable('cdb_mapping', where = paste("country = '", sessionCountry[i], "'", sep = ''))
 new_map_isscfc <- rbind(new_map_isscfc, new_country_isscfc)
 }

if(nrow(new_map_isscfc) > 0){
  new_map_isscfc[ end_year == 'LAST', end_year := year]
  
  newMappingCDB <- merge(commodityDBIcs, new_map_isscfc,
                         by.x = c('geographicAreaM49_fi', 'measuredElement','measuredItemISSCFC'),
                         by.y = c('country', 'element','isscfc'), all.x = TRUE)
  
  
  unchangedCDB <- newMappingCDB[is.na(from_code)]
  tochangeCDB <- newMappingCDB[!is.na(from_code)]
  
  tochangeCDB[ timePointYears < end_year & timePointYears > start_year & ratio == 1, ics := to_code]
  tochangeCDB[ timePointYears < end_year & timePointYears > start_year & ratio != 1, Value := Value * (1-as.numeric(ratio))]
  
  duplicateCDB <- tochangeCDB[!is.na(Value) & timePointYears < end_year & timePointYears > start_year & ratio != 1]
  duplicateCDB[ , c('ics', 'Value') := list(to_code, Value * as.numeric(ratio))]
  
  changedCDB <- rbind(tochangeCDB, duplicateCDB)
  
  cdbMap_new <- rbind(unchangedCDB, changedCDB) 
  cdbMap_new <- rbind(unchangedCDB, changedCDB[ics != '9999']) 
  
  cdbMap_new[ , c('from_code', 'to_code', 'start_year', 'end_year', 'ratio'):= NULL]
  # Sum by ICS, no commodities anymore
} else {
  cdbMap_new <- commodityDBIcs
}



#-- Link table ----
message('fi_SUA-FBS: Link table and other uses deviations')
# Link table for special period ICS group changes
link_table <- ReadDatatable("link_table")

## Checks on link table
# quantity different from 100% allocated
link_table[ , check := sum(percentage), by=c("geographic_area_m49","flow","start_year","end_year","from_code")]

if(any(link_table$check!=1)){
  message(paste0("Not 100% of the original quantity is allocated for link:" , paste0(link_table[link_table$check!=1,], collapse = " ")))
  msg2email2 <- paste0("Not 100% of the original quantity is allocated for link:" , paste0(link_table[link_table$check!=1,], collapse = " "))

  }
msg2email2 <- ifelse(any(link_table$check!=1), msg2email2, "")
# Link table expressed in terms of "PRD", "TRD", "EXP", "IMP", "ALL"
# they translate into standard measuredElement through linkCorrespondence
linkCorrespondence <- data.table(flow = c("PRD", "TRD", "TRD", "EXP", "IMP", "ALL", "ALL", "ALL"), 
                                 measuredElement = c("5510", "5910", "5610",  "5910", "5610", "5510", "5910", "5610"))

message('From flow to code')
link_table2 <- merge(link_table, linkCorrespondence, by = "flow", allow.cartesian = TRUE)

link_table2$end_year <- ifelse(link_table2$end_year == "LAST", max(as.numeric(cdbMap_new$timePointYears)),
                               link_table2$end_year)

# Change ICS codes
message('From table to CDB')
commodityDBLink <- merge(cdbMap_new, link_table2, 
                         by.x = c("geographicAreaM49_fi", "measuredElement", "ics"),
                         by.y = c("geographic_area_m49", "measuredElement", "from_code"), 
                         all.x = TRUE, allow.cartesian = TRUE)



setkey(commodityDBLink)
commodityDBLink <- unique(commodityDBLink)
# Avoid NAs for periods
commodityDBLink$start_year <- ifelse(is.na(commodityDBLink$start_year), "1900", commodityDBLink$start_year)
commodityDBLink$end_year <- ifelse(is.na(commodityDBLink$end_year), "9999", commodityDBLink$end_year)

commodityDBLink <- commodityDBLink[timePointYears >= start_year, ]
commodityDBLink <- commodityDBLink[timePointYears <= end_year]

# Change ICS for defined periods

commodityDBLink[!is.na(to_code) & 
                  as.numeric(timePointYears) >= as.numeric(start_year) &
                  as.numeric(timePointYears) <= as.numeric(end_year), ics := to_code]

commodityDBLink[!is.na(percentage) , Value := Value*percentage]

# remove unnecessary dimensions
commodityDBLink <- commodityDBLink[ , c("flow", "start_year", "end_year", "percentage", "to_code", "check") := NULL]


##-- Other uses introduction ----

# Some commodities are not imported for food purpouses (e.g. "ornamental fish").
# Those flow are deviated to "other utilizations"

otherUses <- ReadDatatable('other_uses')

message('Other uses merge')
commodityDBotherUses <- merge(commodityDBLink, otherUses, 
                              by.x = c( "measuredItemISSCFC", "measuredElement", "ics"),
                              by.y = c("isscfc", "measured_element_orig", "ics"))

commodityDBotherUses$measuredElement <- ifelse(is.na(commodityDBotherUses$measured_element_dest),
                                               commodityDBotherUses$measuredElement,
                                               commodityDBotherUses$measured_element_dest)

commodityDBotherUses <- commodityDBotherUses[ , c("label", "measured_element_dest", "fias_code") := NULL]

commodityDBdeviated <- rbind(commodityDBLink, commodityDBotherUses)

# Sum by ICS, no commodities anymore

commodityDBAggr <- commodityDBdeviated[ , list(Value = sum(Value, na.rm = TRUE),
                                                flagObservationStatus = max(flagObservationStatus),
                                                flagMethod = "s"),
                                         by = list(geographicAreaM49_fi,
                                                   timePointYears,
                                                   measuredElement,
                                                   ics)]

#-- SUA ----

SUA <- rbind(globalProductionAggr, commodityDBAggr)
setnames(SUA, "ics", "measuredItemFaostat_L2")

SUA <- SUA[ , list(Value = sum(Value, na.rm = TRUE),
                   flagObservationStatus = max(flagObservationStatus),
                   flagMethod = "s"), by = list(geographicAreaM49_fi,
                                                timePointYears,
                                                measuredElement,
                                                measuredItemFaostat_L2)]
setnames(SUA, 'measuredElement', 'measuredElementSuaFbs')
SUA <- SUA[!is.na(Value)]
message("fi_SUA-FBS: Saving SUA unbalanced data")

CONFIG <- GetDatasetConfig(sessionKey_suaUnb@domain, sessionKey_suaUnb@dataset)

stats <- SaveData(domain = CONFIG$domain,
                  dataset = CONFIG$dataset,
                  data = SUA, waitTimeout = Inf)


msg2email3 <- paste0("Pull data to SUA_unbalanced process completed successfully! ",
       stats$inserted, " observations written, ",
       stats$ignored, " weren't updated, ",
       stats$discarded, " had problems.")

#-- Filling SUA ----

elementSignTable <- ReadDatatable('element_sign_table')
setnames(elementSignTable, 'measured_element', 'measuredElementSuaFbs')

# Now only considering production, import and export to compute availability
# Actually not expanded
SUAexpanded <- merge(SUA, elementSignTable[ , .(measuredElementSuaFbs, sign)], by = "measuredElementSuaFbs", all.x = TRUE)

if(any(is.na(SUAexpanded$sign))){
  stop('There is an element in the SUA not included in the availability calculation.')
}

message("fi_SUA-FBS: Calculating availability")

SUAexpanded[, availability := sum(Value * sign, na.rm = TRUE), 
            by = list(geographicAreaM49_fi, timePointYears, measuredItemFaostat_L2)]

# Check no negative primary availability. 
# Now many production data are missing in the commodity DB in SWS so 
# there are negative primary availabilities
map_asfis <- ReadDatatable('map_asfis')
setnames(map_asfis, c("asfis", "ics"), c("fisheriesAsfis", "measuredItemFaostat_L2"))
setkey(map_asfis)
primary <- unique(map_asfis$measuredItemFaostat_L2)
primaryneg <- SUAexpanded[availability < 0 & measuredItemFaostat_L2 %in% primary]

if(nrow(primaryneg) > 0){
  countriesneg <- unique(primaryneg$geographicAreaM49_fi)
  measuredItemFaostat_L2neg <- unique(primaryneg$measuredItemFaostat_L2)
  msg2email4 <- paste0('There are negative primary availabilities. Check country(ies): ',
                    paste0(countriesneg, collapse = ", "), ' and measuredItemFaostat_L2 groups: ',
                    paste0(measuredItemFaostat_L2neg, collapse = ", "))
  message(msg2email4)
}
msg2email4 <- ifelse(nrow(primaryneg) > 0, msg2email4, "")

message("fi_SUA-FBS: Start processing negative availability")

secondaryneg <- SUAexpanded[availability < 0 & !measuredItemFaostat_L2 %in% primary]
setkey(secondaryneg)
secondaryneg <- secondaryneg[!duplicated(secondaryneg)]

# Delete old imbalances stored
imbalance_store <- ReadDatatable('imbalance_tab', readOnly = FALSE)
if(nrow(imbalance_store[ geographicaream49_fi %in% unique(secondaryneg$geographicAreaM49_fi), ]) > 0){
changeset <- Changeset('imbalance_tab')
AddDeletions(changeset, imbalance_store[ geographicaream49_fi %in% unique(secondaryneg$geographicAreaM49_fi), ])
Finalise(changeset)
}
# Add new imbalances

secondarynegCompliant <- copy(secondaryneg)
secondarynegCompliant <- secondarynegCompliant[ , .(geographicAreaM49_fi, measuredItemFaostat_L2,
                                                    timePointYears, availability)]
setkey(secondarynegCompliant)
secondarynegCompliant <- unique(secondarynegCompliant)

setnames(secondarynegCompliant,
         c('geographicAreaM49_fi', 'timePointYears',
           'measuredItemFaostat_L2'),
         c('geographicaream49_fi', 'timepointyears',
           'measureditemfaostat_l2'))

changeset <- Changeset('imbalance_tab')
AddInsertions(changeset, secondarynegCompliant)
Finalise(changeset)


if(nrow(secondaryneg) > 0){
  countriessecneg <- unique(secondaryneg$geographicAreaM49_fi)
  measuredItemFaostat_L2secneg <- unique(secondaryneg$measuredItemFaostat_L2)
  msg2email5 <- paste0('There are negative secondary availabilities for country(ies): ',
                              paste0(countriessecneg, collapse = ", "), ' and measuredItemFaostat_L2 groups: ',
                              paste0(measuredItemFaostat_L2secneg, collapse = ", "))
  message(msg2email5)
}
msg2email5 <- ifelse(nrow(secondaryneg) > 0, msg2email5, "")

if(nrow(secondaryneg) > 0){

  # Make sure all production (5510) values have been imputed

  icsneg <- unique(secondaryneg$measuredItemFaostat_L2)
  # ics2add <- icsneg[!(icsneg %in% unique(secondaryneg[measuredElementSuaFbs == "5510"]$measuredItemFaostat_L2)) ]
  setkey(secondaryneg, geographicAreaM49_fi, timePointYears,  measuredItemFaostat_L2, availability)
  prod2add <- unique(secondaryneg[ , .(geographicAreaM49_fi, timePointYears,  measuredItemFaostat_L2, availability) ])

  # add production element with NA values and flags then estimate as in Francesca code with estimation flags
  prod2add[ , ':=' (measuredElementSuaFbs = '5510', Value = - availability,
                    flagObservationStatus = as.factor('I'), flagMethod = 'i', sign = 1)]

  # SUA with all production values
  SUAwithProdupd <- merge(secondaryneg, prod2add, by = c('geographicAreaM49_fi',
                                                      'timePointYears',
                                                      'measuredItemFaostat_L2',
                                                      'availability',
                                                      'measuredElementSuaFbs'),
                       suffixes = c('', '_added'), all = TRUE)
  SUAwithProdupd$sign_added <- as.integer(SUAwithProdupd$sign_added)
  
  
  SUAwithProdupd[measuredElementSuaFbs == '5510' , c("Value", "sign", "flagObservationStatus",
                   "flagMethod") := list(ifelse(is.na(Value), Value_added, 
                                                Value+Value_added),
                                               sign_added,
                                         flagObservationStatus_added,
                                         flagMethod_added)]

  
  
  # Putting together values with negative and positive availability which had been separated before
  SUAwithProd <- rbind(SUAwithProdupd[ , .(geographicAreaM49_fi, timePointYears,
                                        measuredItemFaostat_L2, availability,
                                        measuredElementSuaFbs, Value,
                                        flagObservationStatus, flagMethod)],
                       SUAexpanded[availability >= 0, .(geographicAreaM49_fi, timePointYears,
                                                       measuredItemFaostat_L2, availability,
                                                       measuredElementSuaFbs, Value,
                                                       flagObservationStatus, flagMethod)])

} else {

  SUAwithProd <- SUAexpanded
  SUAwithProd[ , sign := NULL ]

}
# Add input value if present
KeySUAinput <- DatasetKey(domain = "Fisheries Commodities", dataset = "fi_sua_balanced_control",
                        dimensions = list(
                          geographicAreaM49_fi = Dimension(name = "geographicAreaM49_fi", keys = GetCodeList("FisheriesCommodities", "fi_sua_balanced_control","geographicAreaM49_fi" )[,code]),
                          measuredItemFaostat_L2 = Dimension(name = "measuredItemFaostat_L2", keys = GetCodeList("FisheriesCommodities", "fi_sua_balanced_control","measuredItemFaostat_L2" )[,code]),
                          measuredElementSuaFbs = Dimension(name = "measuredElementSuaFbs", keys = c('5302', '5510', '5141', '5423')), # GetCodeList("FisheriesCommodities", "fi_sua_balanced_control","measuredElementSuaFbs" )[,code]),
                          timePointYears = Dimension(name = "timePointYears", keys = GetCodeList("FisheriesCommodities", "fi_sua_balanced_control","timePointYears" )[,code] )))

SUAstored <- GetData(KeySUAinput)
# SUAstoredInput <- SUAstored[measuredElementSuaFbs == '5302' & Value != 0]
# SUAstoredInput <- merge(SUAstoredInput, unique(SUAwithProd[ , .(geographicAreaM49_fi,
#                                                         timePointYears,
#                                                         measuredItemFaostat_L2,
#                                                         availability)]),
#                        by = c('geographicAreaM49_fi',
#                               'timePointYears',
#                               'measuredItemFaostat_L2'))
# setkey(SUAstoredInput)
# SUAstoredInput <- unique(SUAstoredInput)

SUAwithInput <- SUAwithProd #rbind(SUAwithProd, SUAstoredInput)
setkey(SUAwithInput)
SUAwithInput <- unique(SUAwithInput)

  
SUAvalProd <- SUAstored[measuredItemFaostat_L2 %in% primary & measuredElementSuaFbs == '5510' & timePointYears == as.character(max(as.numeric(yearVals)) - 1)]
SUAvalFood <- SUAstored[measuredItemFaostat_L2 %in% primary & measuredElementSuaFbs == '5141' & timePointYears == as.character(max(as.numeric(yearVals)) - 1)]
# SUAvalEr <- SUAstored[measuredElementSuaFbs == '5423' & timePointYears != max(yearVals)] #== as.character(max(as.numeric(yearVals)) - 1)]

# Making sure we are dealing with old extraction rate, i.e. in 10 thousands and not decimals (can be removed after first new validation)
# SUAvalEr[Value > 20 , Value := Value/10000]

foodShare <- merge(SUAvalProd, SUAvalFood, by = c('geographicAreaM49_fi', 
                                                  'measuredItemFaostat_L2', 
                                                  'timePointYears'),
                   suffixes = c('Prod', 'Food'), all = TRUE)

foodShare[ , perc := ((ValueFood/ValueProd) - 0.1), by = c('geographicAreaM49_fi', 
                                                 'measuredItemFaostat_L2', 
                                                 'timePointYears')]

foodShare[ , perc := ifelse(perc < 0.03, 0.03, perc)]

balancingElements <- ReadDatatable('balancing_elements')

setnames(balancingElements, names(balancingElements), c("geographicAreaM49_fi", 
                                                        "measuredItemFaostat_L2",
                                                        "measuredElementSuaFbs",
                                                        "start_year", "end_year", "share"))

# For secondary groups at least 3% is dedicated to Food (for ICS whose balancing element is food for the country)
onlyfood <- balancingElements[!measuredItemFaostat_L2 %in% primary & geographicAreaM49_fi %in% sessionCountry & measuredElementSuaFbs == '5141']

onlyfood[ , perc := 0.03]
SUAvalFoodSec <- onlyfood[ , .(geographicAreaM49_fi, measuredItemFaostat_L2, perc)]

# Take primary food
foodShare2apply1 <- foodShare[!is.na(perc) & perc != Inf & perc < 1 ]

foodShare2apply1 <- foodShare2apply1[ , .(geographicAreaM49_fi, measuredItemFaostat_L2, perc)]

# Put together primary and secondary food
foodShare2apply <- rbind(foodShare2apply1, SUAvalFoodSec)
# foodShare2apply <- foodShare2apply1

tree <- ReadDatatable('commodity_tree_fi_provisional')

message("fi_SUA-FBS: Calculating extraction rates")
SUAwithEr <- eRcomputation(data = SUAwithInput, tree = tree[parent %in% primary ], 
                            years = yearVals) #oldEr = SUAvalEr,
message("fi_SUA-FBS: Calculating input element")
# If input available then eR is calculated dividing prod/input => input = prod/eR 
# so the result is the same, input are all calculated
SUAinput <- inputComputation(data = SUAwithEr)

newTree <- merge(tree, unique(SUAinput[ measuredElementSuaFbs == '5423' & !is.na(Value),
                                        .(geographicAreaM49_fi, timePointYears, measuredItemFaostat_L2, Value)]), 
                 by.x = 'child', by.y = 'measuredItemFaostat_L2', all.x = TRUE, allow.cartesian = TRUE)
newTree[ , extraction_rate := Value ]
newTree[ , Value:= NULL]

#-- Recalculate availability using food imputation for primary products ----
food1 <- merge(SUAinput[availability > 0 & measuredElementSuaFbs == '5510'], 
                     foodShare2apply, by = c('geographicAreaM49_fi', 'measuredItemFaostat_L2'), 
                     all.x = TRUE )


food1[ , c('measuredElementSuaFbs', 
                 'flagObservationStatus',
                 'flagMethod'):= list('5141',
                         'E',
                         'b')]

food2 <- food1[!is.na(Value) & !is.na(perc), ValueFood := Value * perc]
food2 <- food2[ , c('Value','perc') := NULL]
food2 <- food2[!is.na(ValueFood)]
food <- food2[ availability < ValueFood, ValueFood := availability]
setnames(food, 'ValueFood', 'Value')

SUAFood <- rbind(SUAinput, food)

SUAFood <- merge(SUAFood, elementSignTable[ , .(measuredElementSuaFbs, sign)], by = "measuredElementSuaFbs", all.x = TRUE)

SUAFood[, availability := sum(Value * sign, na.rm = TRUE), 
            by = list(geographicAreaM49_fi, timePointYears, measuredItemFaostat_L2)]

SUAFood[ , sign := NULL ]
#--Food processing ----
message("fi_SUA-FBS: Calculating food processing")
FPdata_all <- foodProcessingComputation(SUAinput = SUAFood, treeNewER = newTree)
FPdata <- FPdata_all$result
FPdata <- FPdata[Value != 0]
FPproblems <- FPdata_all$problems
FPdata <- FPdata[ , availability := NULL ]
FPdata[ , c("flagObservationStatus", "flagMethod") := list("E", "i")]

SUAFood[ , availability := NULL]
SUAunbal <- rbind(SUAFood[!is.na(Value), ], FPdata)
SUAunbal$flagObservationStatus <- as.character(SUAunbal$flagObservationStatus)

# message("fi_SUA-FBS: Saving data SUA filled unbalanced")

# CONFIG1 <- GetDatasetConfig(sessionKey_suaUnb@domain, sessionKey_suaUnb@dataset)
# 
# stats1 <- SaveData(domain = CONFIG1$domain, 
#                   dataset = CONFIG1$dataset, 
#                   data = SUAunbal, waitTimeout = Inf)
# 
# 
# msg2email6 <- paste0("SUA unbalanced filling process completed successfully! ",
#        stats1$inserted, " observations written, ",
#        stats1$ignored, " weren't updated, ",
#        stats1$discarded, " had problems. Food processing calculation returned the following messages: ", paste(FPmessages, collapse = " ") )

#-- Balancing ----

SUAunbal <-  merge(SUAunbal, elementSignTable[, .(measuredElementSuaFbs, sign)], by = "measuredElementSuaFbs", all.x = TRUE)

if(any(is.na(SUAunbal$sign))){
  stop('There is an element in the SUA not included in the availability calculation.')
}

SUAunbal[ , availability := sum(Value * sign, na.rm = TRUE), 
         by = list(geographicAreaM49_fi, timePointYears, measuredItemFaostat_L2)]

#----------------
message('fi_SUA-FBS: Pulling balancing elements')

currentYear <- as.numeric(gsub("\\-[0-9]*", "", Sys.Date()))
balancingElements[ end_year == "LAST"]$end_year <- as.character(currentYear)

balancingValues <- unique(SUAunbal[ , .(geographicAreaM49_fi, timePointYears , measuredItemFaostat_L2, availability) ])

balancing <- merge(balancingElements, 
                   balancingValues, by = c("geographicAreaM49_fi","measuredItemFaostat_L2"),
                   all.y = TRUE)
setnames(balancing, c("availability"), c("Value"))

if(any(is.na(balancing$measuredElementSuaFbs)) & any(balancing[is.na(measuredElementSuaFbs)]$availability != 0)){
  message('Balancing elements missing!')
  message(balancing[is.na(measuredElementSuaFbs) & availability != 0])
}

balancing2merge <- balancing[ as.numeric(timePointYears) >= as.numeric(start_year) & as.numeric(timePointYears) <= as.numeric(end_year), Value := Value*share]
balancing2merge[ , c('start_year', 'end_year', 'share') := NULL]
balancing2merge[ , c('flagObservationStatus', 'flagMethod') := list('E','b')]
# Balancing cannot be negative
balancingproblems <- balancing2merge[round(Value,3) < 0,]

balancingproblems_store <- ReadDatatable('balancing_problems_tab', readOnly = FALSE)
if(nrow(balancingproblems_store[ geographicaream49_fi %in% unique(secondaryneg$geographicAreaM49_fi), ]) > 0){
  changeset <- Changeset('balancing_problems_tab')
  AddDeletions(changeset, balancingproblems_store[ geographicaream49_fi %in% unique(secondaryneg$geographicAreaM49_fi), ])
  Finalise(changeset)
}
# Add new imbalances

if(nrow(FPproblems$NotCovered) > 0){
  toupload <- copy(FPproblems$NotCovered)
  toupload[ , measuredElementSuaFbs := '5023']
  setnames(toupload, c('parent_primary', 'UncoveredQuantity'),
           c('measuredItemFaostat_L2', 'Value'))
  balancingproblemsCompliant <- rbind(toupload, balancingproblems[ , .(geographicAreaM49_fi, measuredItemFaostat_L2,
                                                                       timePointYears, measuredElementSuaFbs, Value)])
} else {
  balancingproblemsCompliant <- rbind(balancingproblems[ , .(geographicAreaM49_fi, measuredItemFaostat_L2,
                                                                       timePointYears, measuredElementSuaFbs, Value)])
}

setkey(balancingproblemsCompliant)
balancingproblemsCompliant <- unique(balancingproblemsCompliant)

setnames(balancingproblemsCompliant,
         c('geographicAreaM49_fi', 'timePointYears',
           'measuredItemFaostat_L2', 'measuredElementSuaFbs', 'Value'),
         c('geographicaream49_fi', 'timepointyears',
           'measureditemfaostat_l2', 'measuredelementsuafbs', 'value'))

changeset <- Changeset('balancing_problems_tab')
AddInsertions(changeset, balancingproblemsCompliant)
Finalise(changeset)

# if negative balancing not included uncomment lines
# balancing2merge[Value < 0, Value := 0]
# balancing2merge <- balancing2merge[ Value != 0, ]

SUAbal <- merge(SUAunbal[ , .(geographicAreaM49_fi, timePointYears , 
                              measuredItemFaostat_L2, measuredElementSuaFbs, 
                              Value, flagObservationStatus, flagMethod)], balancing2merge,
                by = c('geographicAreaM49_fi', 'timePointYears', 
                       'measuredItemFaostat_L2', 'measuredElementSuaFbs'),
                suffixes = c('','Bal'), 
                all = TRUE)

SUAbal[is.na(ValueBal), ValueBal := 0 ]
SUAbal[is.na(Value), Value := 0 ]
SUAbal[ , Value := Value + ValueBal ]
SUAbal$flagObservationStatus <- as.character(SUAbal$flagObservationStatus)
SUAbal[is.na(flagObservationStatus) , flagObservationStatus := 'E']
SUAbal[is.na(flagMethod) , flagMethod := 'b']
SUAbal <- SUAbal[ , c('ValueBal', 
                      'flagObservationStatusBal',
                      'flagMethodBal') := NULL]

# # Sum if there is a balancing elements that was already present there
# SUAbal1 <- SUAbal[ , list(ValueAggr = sum(Value, na.rm = TRUE), 
#                flagObservationStatusAggr = max(flagObservationStatus),
#                flagMethodAggr = "i"),
#         by=c("geographicAreaM49_fi",
#              "measuredItemFaostat_L2",
#              "measuredElementSuaFbs",
#              "timePointYears")]


SUAbalAvail <- merge(SUAbal, elementSignTable[, .(measuredElementSuaFbs, sign)], by = "measuredElementSuaFbs", all.x = TRUE)

SUAbalAvail[, availability := sum(Value * sign, na.rm = TRUE), 
            by = list(geographicAreaM49_fi, timePointYears, measuredItemFaostat_L2)]

if(any(round(SUAbalAvail$availability) < 0)){
  message("fi_SUA-FBS: Negative availability for some products.")
  msg2email7 <- paste0('Negative availability for products:', 
                       paste0(unique(SUAbalAvail[round(availability) != 0, ]$measuredItemFaostat_L2), collapse = ", "),
                       'for country', paste0(unique(SUAbalAvail[round(availability) != 0, ]$geographicAreaM49_fi), collapse = ", "))

}

msg2email7 <- ifelse(any(round(SUAbalAvail$availability) < 0), msg2email7, "")

SUAbalAvail[, c("sign", "availability"):=NULL]

#-- SUA with nutrient ----
# ## Add NutrientFactors
nutrientFactors <- ReadDatatable("fishery_nutrient")
nutrientFactors$calories <- as.numeric(nutrientFactors$calories)
nutrientFactors$proteins <- as.numeric(nutrientFactors$proteins)
nutrientFactors$fats <- as.numeric(nutrientFactors$fats)
nutrientFactors[is.na(proteins), proteins := 0]

SUA_with_nutrient <- merge(SUAbalAvail, nutrientFactors, by.x = "measuredItemFaostat_L2", by.y = "ics", all.x = TRUE)

SUA_with_nutrient[measuredElementSuaFbs=="5141", calories:=Value*calories/100]
SUA_with_nutrient[measuredElementSuaFbs=="5141", proteins:=Value*proteins/100]
SUA_with_nutrient[measuredElementSuaFbs=="5141", fats:=Value*fats/100]
SUA_with_nutrient[measuredElementSuaFbs!="5141",`:=`(c("calories", "proteins", "fats"),list(0,0,0) )]

# Get "calories", "proteins" and "fats" and make them in the dataset format
SUAnutrients <-  melt.data.table(SUA_with_nutrient[measuredElementSuaFbs=="5141", ],
                                 id.vars = c('geographicAreaM49_fi', 'measuredItemFaostat_L2', 'timePointYears'),
                                 measure.vars = c('calories', 'proteins','fats'),
                                 variable.name = 'measuredElementSuaFbs', value.name = 'Value')
SUAnutrients$measuredElementSuaFbs <- as.character(SUAnutrients$measuredElementSuaFbs)
SUAnutrients$measuredElementSuaFbs <- ifelse(SUAnutrients$measuredElementSuaFbs == 'calories', '261',
                                       ifelse(SUAnutrients$measuredElementSuaFbs == 'proteins', '271',
                                              ifelse(SUAnutrients$measuredElementSuaFbs == 'fats', '281', SUAnutrients$measuredElementSuaFbs)))

SUAnutrients[ , c('flagObservationStatus', 'flagMethod') := list('E','i')]
SUAnutrients <- unique(SUAnutrients)
foodOnly <- SUA_with_nutrient[measuredElementSuaFbs=="5141", .(measuredItemFaostat_L2, measuredElementSuaFbs,
                                                   geographicAreaM49_fi, timePointYears, Value,
                                                   flagObservationStatus, flagMethod)]

SUAnutrients <- rbind(SUAnutrients, foodOnly)

#-- Get population data ----
elemKeys <- "511"

keyPop <- DatasetKey(domain = "population", dataset = "population_unpd", dimensions = list(
  geographicAreaM49 = Dimension(name = "geographicAreaM49", keys = sessionCountry),
  measuredElement = Dimension(name = "measuredElement", keys = elemKeys),
  timePointYears = Dimension(name = "timePointYears", keys = yearVals)
))

popSWS <- GetData(keyPop)
setnames(popSWS,c("geographicAreaM49", "measuredElement"),c("geographicAreaM49_fi", "measuredElementSuaFbs"))

sourceMetaData0 <- GetMetadata(keyPop)
sourceMetaData <- sourceMetaData0[grepl("WPP", Metadata_Value),]
sourceMetaData <- sourceMetaData[Metadata == 'GENERAL', ]
setnames(sourceMetaData, c("geographicAreaM49", "measuredElement"),c("geographicAreaM49_fi", "measuredElementSuaFbs"))
#----


SUAnutrientCapita <- merge(SUAnutrients, popSWS, by=c("geographicAreaM49_fi","timePointYears"), suffixes = c("","_pop"))  
SUAnutrientCapita[measuredElementSuaFbs !="5141" , Value := (Value*1000)/(Value_pop*365)]
SUAnutrientCapita[measuredElementSuaFbs =="5141" , Value := Value/Value_pop]
SUAnutrientCapita <- SUAnutrientCapita[ , .(geographicAreaM49_fi,
                                            timePointYears,
                                            measuredItemFaostat_L2,
                                            measuredElementSuaFbs,
                                            Value, flagObservationStatus,
                                            flagMethod)]

SUAnutrientCapita[measuredElementSuaFbs=="261",measuredElementSuaFbs:="264"]
SUAnutrientCapita[measuredElementSuaFbs=="281",measuredElementSuaFbs:="284"]
SUAnutrientCapita[measuredElementSuaFbs=="271",measuredElementSuaFbs:="274"]
SUAnutrientCapita[measuredElementSuaFbs=="5141",measuredElementSuaFbs:="5038"]

SUA_with_nutrient[ , c('calories', 'proteins','fats') := NULL] 


# bind SUA with "calories", "proteins" and "fats" elements
SUAallNutr <- rbind(SUAnutrients[measuredElementSuaFbs!="5141"], SUAnutrientCapita)
SUANoPop <- rbind(SUA_with_nutrient, SUAallNutr)
Pop2include <- merge(unique(SUANoPop[ , .(measuredItemFaostat_L2,
                                          geographicAreaM49_fi,
                                          timePointYears)]), popSWS, by = c('geographicAreaM49_fi', 
                                                                            'timePointYears'))
Metadata2include <- merge(unique(SUANoPop[ , .(measuredItemFaostat_L2,
                                          geographicAreaM49_fi,
                                          timePointYears)]), sourceMetaData, by = c('geographicAreaM49_fi', 
                                                                                    'timePointYears'),
                          allow.cartesian = TRUE)

SUA2save <- rbind(SUANoPop, Pop2include)
SUA2save <- SUA2save[!is.na(Value)]
message("fi_SUA-FBS: Saving data SUA balanced data")
CONFIG2 <- GetDatasetConfig(sessionKey_suabal@domain, sessionKey_suabal@dataset)

stats2 <- SaveData(domain = CONFIG2$domain,
                   dataset = CONFIG2$dataset,
                   data = SUA2save[measuredElementSuaFbs!= "5038" ],
                   metadata = Metadata2include, waitTimeout = Inf)

msg2email8 <- paste0("SUA food processing, balancing and nutrient inclusion processes completed successfully! ",
                  stats2$inserted, " observations written, ",
                  stats2$ignored, " weren't updated, ",
                  stats2$discarded, " had problems.")

#-- Standardisation & FBS ----
# get all conversion factors (or extration rates) from commodity tree
message('Get commodity tree')
tree <- ReadDatatable('commodity_tree_fi_provisional')

extrRates <- unique(SUA2save[ measuredElementSuaFbs == '5423', .(measuredItemFaostat_L2, geographicAreaM49_fi, timePointYears, Value)])

compareEr <- merge(unique(tree[parent %in% primary , .(child, extraction_rate)]), extrRates, by.x = 'child', by.y = 'measuredItemFaostat_L2', all.y = TRUE)

compareEr[is.na(Value), Value := extraction_rate]

updatedEr <- compareEr[ , .(child, Value, geographicAreaM49_fi, timePointYears)]

updatedtree <- merge(unique(tree[parent %in% primary , .(parent, child, weight)]), updatedEr, by= 'child', all.y = TRUE, allow.cartesian = TRUE)

primary <- unique(updatedtree[!parent %in% child]$parent)
setkey(updatedtree)
convFact <- unique(updatedtree[weight == TRUE & !is.na(Value) , .(geographicAreaM49_fi, timePointYears, parent, child, Value)])

# add primary ICS with Er equal 1
primaryTree1 <- data.table(geographicAreaM49_fi = rep(unique(convFact$geographicAreaM49_fi), each = length(unique(convFact$timePointYears))), 
                           timePointYears = rep(unique(convFact$timePointYears), length(unique(convFact$geographicAreaM49_fi))) )

primaryTree2 <- data.table(geographicAreaM49_fi = rep(unique(convFact$geographicAreaM49_fi), each = length(unique(primary))), parent = primary)

primaryTree <- merge(primaryTree1, primaryTree2, by = 'geographicAreaM49_fi', allow.cartesian = TRUE)  
primaryTree[ , `:=` (child= parent, Value = 1)]

convFact <- rbind(convFact, primaryTree)
setnames(convFact, new = 'extraction_rate', old = 'Value')
# SUA with standardized element, no zero weight elements

SUAstand_prep <- merge(SUA2save, convFact, by.x = c('geographicAreaM49_fi', 'timePointYears', 'measuredItemFaostat_L2'),
                  by.y = c('geographicAreaM49_fi', 'timePointYears', 'child'))
setkey(SUAstand_prep)
SUAstand_prep <- unique(SUAstand_prep)
# Standardised value is Value/eR except from input values which are already in primary equivalent
SUAstand_prep[! measuredElementSuaFbs %in% c('5302', '261', '264', '271', '274', '281', '284', '511'), Value_stand := Value/extraction_rate]
SUAstand_prep[measuredElementSuaFbs %in% c('5302', '261', '264', '271', '274', '281', '284', '511'), Value_stand := Value]

# take only all primary elements and for secondary exclude production and input
SUAstand <-  rbind(SUAstand_prep[measuredItemFaostat_L2 %in% primary, ], 
                   SUAstand_prep[!measuredElementSuaFbs %in% c('5510', '5302') & !measuredItemFaostat_L2 %in% primary, ])



##-- FAOSTAT ----
# Aggregate SUA by parent meals included for FAOSTAT
 
 SUAstandAggr0 <- SUAstand[ , .(measuredElementSuaFbs, geographicAreaM49_fi, timePointYears,
                               flagObservationStatus, flagMethod, parent, Value_stand)]
 
 
 
 SUAstandAggr0$flagObservationStatus <- factor(SUAstandAggr0$flagObservationStatus, levels = c('M', 'O', 'N', '', 'X', 'T', 'E', 'I'), ordered = TRUE)
 
 SUAstandAggr1 <- SUAstandAggr0[measuredElementSuaFbs != '511' , list(Value = sum(Value_stand, na.rm = TRUE),
                  flagObservationStatusAggr = max(flagObservationStatus),
                  flagMethodAggr = 's'), by = c("measuredElementSuaFbs", "geographicAreaM49_fi",
                                             "timePointYears", "parent")]

 setnames(SUAstandAggr1, c('parent', 'flagObservationStatusAggr', 'flagMethodAggr'), 
          c('measuredItemFaostat_L2', 'flagObservationStatus', 'flagMethod'))
 Pop2SUAaggr <- SUAstandAggr0[measuredElementSuaFbs == '511']
 setnames(Pop2SUAaggr, c('Value_stand', 'parent'), c('Value', 'measuredItemFaostat_L2'))
 SUAstandAggr <- rbind(SUAstandAggr1, Pop2SUAaggr)
 
 IcsGroups <- unique(SUAstandAggr[ , .(geographicAreaM49_fi, timePointYears, measuredItemFaostat_L2)])
 
 faostat_pop2merge <- merge(IcsGroups, popSWS, by=c("geographicAreaM49_fi","timePointYears"), all = TRUE) 
 
 faostatfbsPOP <- rbind(SUAstandAggr, faostat_pop2merge)
 
 # mapp for FBS groups
 sua_fbs_mapping <- data.table(measuredItemFaostat_L2 = primary[primary!='1579'],
                               fbs = c(  "10",  "20",  "30",  "40",  "50",  "60",  "70",  "90"))
 
 fbsFaostatL1faostat <- merge(faostatfbsPOP, sua_fbs_mapping, by = "measuredItemFaostat_L2")
 fbsFaostatL1faostat[ , measuredItemFaostat_L2 := NULL]
 setnames(fbsFaostatL1faostat, "fbs", "measuredItemFaostat_L2")

 
# faostatOutmain[ measuredElementSuaFbs %in% c("261","271","281"), Value_caput_day:= (Value/Value_pop)/365]  
# nutrient_caput_day <- faostatOutmain[measuredElementSuaFbs %in% c("261","271","281")
#                          ,.(measuredItemFaostat_L2,geographicAreaM49_fi,timePointYears,measuredElementSuaFbs,Value_caput_day)]
# 
# 
# nutrient_caput_day[measuredElementSuaFbs=="261",measuredElementSuaFbs:="calories_caput_day"]
# nutrient_caput_day[measuredElementSuaFbs=="281",measuredElementSuaFbs:="fats_caput_day"]
# nutrient_caput_day[measuredElementSuaFbs=="271",measuredElementSuaFbs:="proteins_caput_day"]
# 
# setnames(nutrient_caput_day,"Value_caput_day", "Value")
# 
# out=faostatOutmain[,.(measuredItemFaostat_L2,geographicAreaM49_fi,timePointYears,measuredElementSuaFbs, Value)]
# 
# out=rbind(out, nutrient_caput_day)
# 
# 
# 
#-- FAOSTAT FBS standardization ----
 
 message("Starting Faostat standardization")
faostatGroups <- data.table(measuredElementSuaFbs = c("5510", "5610", "5910", "5912", "5520",
                                                "5525", "5166", "5071", "5016", "5141", "5038", "264",
                                                "511"),
                            faostat = c('Production', 'Imports', 'Exports', 'Exports', 'Feed', 'Bait',
                                        'Other net uses', 'Other net uses', 'Other net uses',
                                        'Total food consumption', 'Food quantity/day/capita (kg)',
                                        'Food consumption (kcal)', 'Population'),
                            measuredElementFaostat = c("5510", "5610", "5910", "5910", "5520",
                                                       "5525", "5153", "5153", "5153", "5141", "294", 
                                                       "664", "511"))

FBSfaostat0 <- merge(fbsFaostatL1faostat[!measuredElementSuaFbs %in% c('271', '281')],
                    faostatGroups, by = "measuredElementSuaFbs")
FBSfaostat1 <- FBSfaostat0[measuredElementFaostat != '511' , list(Value = sum(Value, na.rm = TRUE),
                                 flagObservationStatus = max(flagObservationStatus),
                                 flagMethod = 's'), by = c("geographicAreaM49_fi",
                                                               "timePointYears", "measuredItemFaostat_L2",
                                                               "faostat", "measuredElementFaostat")]

FBSfaostat <- rbind(FBSfaostat1, FBSfaostat0[measuredElementFaostat == '511', -'measuredElementSuaFbs', with = FALSE])
setnames(FBSfaostat, c("faostat", "measuredElementFaostat"), c("element_description", "measuredElementSuaFbs"))
# FBSfaostat <- FBSfaostat[ , Value := ifelse(measuredElementFaostat %in% c("264"), Value, Value/1000)]


faostatFBS2save <- FBSfaostat[ , .(geographicAreaM49_fi, timePointYears, measuredItemFaostat_L2,
                                   measuredElementSuaFbs, Value, flagObservationStatus, flagMethod)]
faostatFBS2save <- faostatFBS2save[!is.na(Value)]

message("fi_SUA-FBS: Saving FAOSTAT standardized data")
CONFIG4 <- GetDatasetConfig(sessionKey_fbsFaostat@domain, sessionKey_fbsFaostat@dataset)

statsFaostat <- SaveData(domain = CONFIG4$domain,
                      dataset = CONFIG4$dataset,
                      data = faostatFBS2save, waitTimeout = Inf)

msg2email10 <- paste0("Standardization process completed successfully! ",
                     statsFaostat$inserted, " observations written, ",
                     statsFaostat$ignored, " weren't updated, ",
                     statsFaostat$discarded, " had problems.")

#-- FIAS ----
# Aggregate SUA by parent code take away meals
mealCodes <- c('1508', '1521', '1534', '1547', '1558', '1566', '1575', '1589')

# SUA with elements and groups to aggregates
SUAstandAggrFias0 <- SUAstand[ !measuredItemFaostat_L2 %in% mealCodes , .(measuredElementSuaFbs, geographicAreaM49_fi, timePointYears,
                                                                         flagObservationStatus, flagMethod, parent, Value_stand)]

# take only input element 5302
mealsInput0 <- SUA2save[measuredElementSuaFbs == '5302' & measuredItemFaostat_L2 %in% mealCodes , .(measuredElementSuaFbs, geographicAreaM49_fi, timePointYears,
                                                                  flagObservationStatus, flagMethod, 
                                                                  measuredItemFaostat_L2, Value)]

mealsInput <- merge(mealsInput0, unique(tree[parent %in% primary, .(parent, child)]), 
                    by.x = 'measuredItemFaostat_L2', by.y = 'child', all.x = TRUE)

mealsInput[ , measuredItemFaostat_L2 := NULL]
setnames(mealsInput, c("parent"), c("measuredItemFaostat_L2"))

SUAstandAggrFias0$flagObservationStatus <- factor(SUAstandAggrFias0$flagObservationStatus, levels = c('M', 'O', 'N', '', 'X', 'T', 'E', 'I'), ordered = TRUE)

SUAstandAggrFias1 <- SUAstandAggrFias0[measuredElementSuaFbs != '511' , list(Value = sum(Value_stand, na.rm = TRUE),
                                             flagObservationStatusAggr = max(flagObservationStatus),
                                             flagMethodAggr = 's'), by = c("measuredElementSuaFbs", "geographicAreaM49_fi",
                                                                           "timePointYears", "parent")]

setnames(SUAstandAggrFias1, c('parent', 'flagObservationStatusAggr', 'flagMethodAggr'), 
         c('measuredItemFaostat_L2', 'flagObservationStatus', 'flagMethod'))

Pop2SUAaggFias <- SUAstandAggrFias0[measuredElementSuaFbs == '511']
setnames(Pop2SUAaggFias,c('Value_stand', 'parent'), c('Value', 'measuredItemFaostat_L2'))
SUAstandAggrFias <- rbind(SUAstandAggrFias1, Pop2SUAaggFias)



# Include population data

# fias_pop <- merge(SUAstandAggrFias, popSWS, by=c("geographicAreaM49_fi","timePointYears"), suffixes = c("","_pop"))  
# fias_pop[ measuredElementSuaFbs %in% c("261","271","281"), Value_caput_day:= (Value/Value_pop)]  
# nutrient_caput_year_FIAS <- fias_pop[measuredElementSuaFbs %in% c("261","271","281")
#                                      ,.(measuredItemFaostat_L2,geographicAreaM49_fi,timePointYears,
#                                         measuredElementSuaFbs,Value_caput_day,flagObservationStatus, flagMethod)]
# 
# 
# nutrient_caput_year_FIAS[measuredElementSuaFbs=="261",measuredElementSuaFbs:="264"]
# nutrient_caput_year_FIAS[measuredElementSuaFbs=="281",measuredElementSuaFbs:="284"]
# nutrient_caput_year_FIAS[measuredElementSuaFbs=="271",measuredElementSuaFbs:="274"]
# nutrient_caput_year_FIAS[ , Value_caput_day := Value_caput_day/365]
# setnames(nutrient_caput_year_FIAS,"Value_caput_day", "Value")

# fiasFbs <- fias_pop[,.(measuredItemFaostat_L2,geographicAreaM49_fi,timePointYears,measuredElementSuaFbs, 
#                        Value, flagObservationStatus, flagMethod)]
# 
# fiasFbsTot1 <- rbind(fiasFbs, nutrient_caput_year_FIAS)
fiasFbsTot <- rbind(SUAstandAggrFias, mealsInput)
IcsGroups <- unique(fiasFbsTot[ , .(geographicAreaM49_fi, timePointYears, measuredItemFaostat_L2)])

# Introduce population data
pop2merge <- merge(popSWS, IcsGroups, by = c("geographicAreaM49_fi", "timePointYears"), all = TRUE)

fiasfbsPOP <- rbind(fiasFbsTot, pop2merge)

# mapp for FBS groups
sua_fbs_mapping <- data.table(measuredItemFaostat_L2 = primary[primary!='1579'],
                              fbs = c(  "10",  "20",  "30",  "40",  "50",  "60",  "70",  "90"))

fbsFaostatL1 <- merge(fiasfbsPOP, sua_fbs_mapping, by = "measuredItemFaostat_L2")
fbsFaostatL1[ , measuredItemFaostat_L2 := NULL]
setnames(fbsFaostatL1, "fbs", "measuredItemFaostat_L2")

#-- FIAS FBS standardization ---- 
message("fi_SUA-FBS: Starting Fias standardization")
fiasGroups <- data.table(measuredElementSuaFbs = c("5510", "5302", "5610", "5910", "5912", "5520", 
                                                   "5525", "5166", "5016", "5071", "5141", "511", "5038", "264",
                                                   "274", "284"),
                         fias = c('Production', 'Meals input', 'Imports', 'Exports', 'Exports', 'Other non-food uses', 
                                  'Other non-food uses', 'Other non-food uses', 'Other non-food uses', 'Stock variations',
                                  'Total food supply', 'Population', 'Per capita food',
                                  'Calories', 'Proteins', 'Fats'),
                         measuredElementFias = c("5510", "5302", "5610", "5910", "5910", "5153", 
                                                 "5153", "5153", "5153", "5071", "5141", "511", "5038", # or "5036" instead of 261 
                                                 "264", "274", "284"))

# # Ics groups regarding meals
# MealsIcs <- c('1508', '1521', '1534', '1547', '1558', '1566', '1575', '1589')
# # select input values only for meals
# Mealsinput <- fiasFbsTot[ measuredElementSuaFbs == "5302" & measuredItemFaostat_L2 %in% MealsIcs]
# 
# MealsinputParent <- merge(Mealsinput,tree[parent %in% primary , .(parent, child)], by.x = c('measuredItemFaostat_L2'), by.y = c('child'))
# 
# # Meal inputs have to be taken away from production
# mealsProdAway <- merge(fiasFbsTot[measuredElementSuaFbs == "5510" & ! measuredItemFaostat_L2 %in% MealsIcs], MealsinputParent, 
#       by.x = c("geographicAreaM49_fi", "timePointYears", "measuredItemFaostat_L2"),
#       by.y = c("geographicAreaM49_fi", "timePointYears", "parent"),
#       suffixes = c("_prod", "_meals"), all.x = TRUE)
# 
# mealsProdAway$Value_meals <- as.numeric(mealsProdAway$Value_meals)
# mealsProdAway[ , Value := ifelse(is.na(Value_meals), Value_prod, (Value_prod - Value_meals)) ]
# mealsProdAway <- mealsProdAway[ , .(geographicAreaM49_fi, timePointYears, measuredItemFaostat_L2,
#                                     measuredElementSuaFbs_prod, Value, flagObservationStatus_prod,
#                                            flagMethod_prod)]
# setnames(mealsProdAway, c('measuredElementSuaFbs_prod', 'flagObservationStatus_prod', 'flagMethod_prod'), 
#          c('measuredElementSuaFbs', 'flagObservationStatus','flagMethod'))
# 
# SUAstandFias <- rbind(SUAstandAggr[ measuredElementSuaFbs != '5510' & ! measuredItemFaostat_L2 %in% MealsIcs], 
#                       Mealsinput, mealsProdAway)


# FBS
FBSfias0 <- merge(fbsFaostatL1[!measuredElementSuaFbs %in% c('261', '271', '281')], 
                 fiasGroups, by = "measuredElementSuaFbs")
FBSfias1 <- FBSfias0[measuredElementFias != '511' , list(Value = sum(Value, na.rm = TRUE),
                           flagObservationStatus = max(flagObservationStatus),
                           flagMethod = 's'), by = c("geographicAreaM49_fi",
                                                     "timePointYears", "measuredItemFaostat_L2",
                                                     "fias", "measuredElementFias")]

FBSfias <- rbind(FBSfias1, FBSfias0[measuredElementFias == '511', -'measuredElementSuaFbs', with = FALSE])
fiasFBS2save <-  FBSfias[, .(geographicAreaM49_fi, timePointYears, measuredItemFaostat_L2, 
                             measuredElementFias, Value, flagObservationStatus, flagMethod)]

setnames(fiasFBS2save, "measuredElementFias", "measuredElementSuaFbs")

metadataGroups <- unique(fiasFBS2save[ , .(geographicAreaM49_fi, timePointYears, measuredItemFaostat_L2)])
Metadata2includeFias <- merge(sourceMetaData, metadataGroups, by = c("geographicAreaM49_fi", "timePointYears"), 
                              all = TRUE, allow.cartesian = TRUE)


message("fi_SUA-FBS: Saving FIAS standardized data")
CONFIG3 <- GetDatasetConfig(sessionKey_fbsFias@domain, sessionKey_fbsFias@dataset)

statsFias <- SaveData(domain = CONFIG3$domain,
                   dataset = CONFIG3$dataset,
                   data = fiasFBS2save,
                   metadata = Metadata2includeFias,
                   waitTimeout = Inf)

msg2email9 <- paste0("Standardization process completed successfully! ",
       statsFias$inserted, " observations written, ",
       statsFias$ignored, " weren't updated, ",
       statsFias$discarded, " had problems.")

R_SWS_SHARE_PATH <- Sys.getenv("R_SWS_SHARE_PATH")

saveRDS(FPproblems,
  file.path(R_SWS_SHARE_PATH, "shiny-app", "shinyFisheriesSUAFBS", "FoodProcessingFeedback.rds")
)
##-- send Email with notification of correct execution ----

## Initiate email
from = "sws@fao.org"
to = swsContext.userEmail
subject = "fi_SUA-FBS plug-in has correctly run"
body = paste("The plug-in has saved the data in your sessions. The plugin returned the following messages:", 
             msg2email1, 
             msg2email2, 
             msg2email4, 
             msg2email5, 
             # msg2email6,
             msg2email7, 
             msg2email8, 
             msg2email9, 
             msg2email10,
           #  FPproblems,
             sep = "\n")

sendmailR::sendmail(from = from, to = to, subject = subject, msg = body)
paste0("Email sent to ", swsContext.userEmail)



