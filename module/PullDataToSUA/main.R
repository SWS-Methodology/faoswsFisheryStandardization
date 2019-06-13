##' # Pull data from Global Production and Commodtity dataset to SUA
##'
##' **Author: Charlotte Taglioni**
##'
##' **Description:**
##'
##' This module is designed to harvest the data from Global Production and Commodtity dataset
##' and pull all relevant FBS data into the SUA unbalanced dataset.  
##' It pulls from the following
##' 
##' **Inputs:**
##'
##' * Global Production (production from capture and aquaculture sources)
##' * Commodities (validated) (production, import, export)
##' 
##' * Datatables for other uses...

##'
##' **Flag assignment:**
##'
##' | Observation Status Flag | Method Flag|
##' | --- | --- | --- |


## load the library
library(faosws)
<<<<<<< HEAD
=======
library(faoswsModules)
>>>>>>> 2b5bd370ef208233830f02597b0fb0f518addcb1
library(data.table)
library(faoebx5)
library(readxl)
library(faoswsStandardization)


#-- Needed datasets ----

## Get global production (from Production environment)

message("Pulling data from Global production")

if(CheckDebug()){
  
  SETTINGS = ReadSettings("module/pullDataToSUA/sws1.yml")
  
  ## If you're not on the system, your settings will overwrite any others
  R_SWS_SHARE_PATH = SETTINGS[["share"]]
  
  ## Define where your certificates are stored
  SetClientFiles(SETTINGS[["certdir"]])
  
  ## Get session information from SWS. Token must be obtained from web interface
  GetTestEnvironment(baseUrl = SETTINGS[["server"]],
                     token = SETTINGS[["token"]])
  
  startYear = 2000
  endYear   = 2016
  
}

# TEST COUNTRIES
currentCountry <- c("156", "300", "203", "40")

keyDim <- c("geographicAreaM49_fi", "fisheriesAsfis", "measuredElement", "timePointYears")

KeyGlobal <- DatasetKey(domain = "Fisheries", dataset = "fi_global_production", dimensions = list(
  geographicAreaM49_fi = Dimension(name = "geographicAreaM49_fi", keys = currentCountry),
  fisheriesAsfis = Dimension(name = "fisheriesAsfis", keys = GetCodeList("Fisheries", "fi_global_production","fisheriesAsfis" )[,code]),
  fisheriesCatchArea = Dimension(name = "fisheriesCatchArea", keys = GetCodeList("Fisgheries", "fi_global_production","fisheriesCatchArea" )[,code]),
  measuredElement = Dimension(name = "measuredElement", keys = c("FI_001")),
  timePointYears = Dimension(name = "timePointYears", keys = as.character(c(startYear:endYear) ))
))

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

message("Pulling data from Commodites dataset")
if(CheckDebug()){
  
  SETTINGS = ReadSettings("sws.yml")
  
  ## If you're not on the system, your settings will overwrite any others
  R_SWS_SHARE_PATH = SETTINGS[["share"]]
  
  ## Define where your certificates are stored
  SetClientFiles(SETTINGS[["certdir"]])
  
  ## Get session information from SWS. Token must be obtained from web interface
  GetTestEnvironment(baseUrl = SETTINGS[["server"]],
                     token = SETTINGS[["token"]])
  
  startYear = 2000
  endYear   = 2016
  
}


KeyComm <- DatasetKey(domain = "Fisheries Commodities", dataset = "commodities_total_validated_test", dimensions = list(
  geographicAreaM49_fi = Dimension(name = "geographicAreaM49_fi", keys = GetCodeList("FisheriesCommodities", "commodities_total","geographicAreaM49_fi" )[,code]),
  measuredItemISSCFC = Dimension(name = "measuredItemISSCFC", keys = GetCodeList("FisheriesCommodities", "commodities_total","measuredItemISSCFC" )[,code]),
  measuredElement = Dimension(name = "measuredElement", keys = GetCodeList("FisheriesCommodities", "commodities_total","measuredElement" )[,code]),
  timePointYears = Dimension(name = "timePointYears", keys = GetCodeList("FisheriesCommodities", "commodities_total","timePointYears" )[,code])))


## It should be the commodity DB validated
# KeyComm <- DatasetKey(domain = "Fisheries Commodities", dataset = "commodities_total_validated", dimensions = list(
#   geographicAreaM49_fi = Dimension(name = "geographicAreaM49_fi", keys = GetCodeList("FisheriesCommodities", "commodities_total","geographicAreaM49_fi" )[,code]),
#   measuredItemISSCFC = Dimension(name = "measuredItemISSCFC", keys = GetCodeList("FisheriesCommodities", "commodities_total","measuredItemISSCFC" )[,code]),
#   measuredElement = Dimension(name = "measuredElement", keys = GetCodeList("FisheriesCommodities", "commodities_total","measuredElement" )[,code]),
#   timePointYears = Dimension(name = "timePointYears", keys = GetCodeList("FisheriesCommodities", "commodities_total","timePointYears" )[,code])))

commodityDB <- GetData(KeyComm)
commodityDB$flagObservationStatus <- factor(commodityDB$flagObservationStatus,
                                            levels = c('M', 'O', 'N', '', 'X', 'T', 'E', 'I'), 
                                            ordered = TRUE)

# No need of value elements "5622" and "5922"
commodityDB <- commodityDB[!measuredElement %in% c("5622", "5922")]

#-- Connoect to EBX for updated mapping ----

# user = Fishery-SOAP
# password = W8EqZpDM4YBMKsrq 
message("Connecting to EBX5 and creating mappings")

SetEBXCredentials(username = 'Fishery-SOAP',  password = 'W8EqZpDM4YBMKsrq',  new = T)

#-- Commodity mapping ----
# Get ISSCFC codes
isscfcList <- ReadEBXCodeList(cl_name = "ISSCFC")
isscfc <- isscfcList[, .(Identifier, Code)]

# Get ICS codes
icsList <- ReadEBXCodeList(cl_name = "FAOSTAT_Level2")
ics <- icsList[ , .(Identifier, FAOSTAT_Code)]

# Get mapping ICS-ISSCFC
ics2isscfc <- ReadEBXGroup(gr_name = "Group_FAOSTATL2_ISSCFC")

map_isscfc <- merge(ics2isscfc, ics, by.x = "Group", by.y = "Identifier")
map_isscfc <- merge(map_isscfc, isscfc, by.x = "Member", by.y = "Identifier")

# Keep only needed dimensions and put right names
map_isscfc <- map_isscfc[,.(FAOSTAT_Code, Code)]
setnames(map_isscfc, names(map_isscfc), c("ics", "measuredItemISSCFC"))

#-- Species mapping ----
# Get Asfis alphacodes
asfisList <- ReadEBXCodeList(cl_name = "SpeciesItem")
asfis <- asfisList[, .(Identifier, Alpha.Code, NAME_en)]

# Get ISSCAAP groups codes
isscaapList <- ReadEBXCodeList(cl_name ="SpeciesIsscaapGroup")
isscaap <- isscaapList[, .(Identifier, ISSCAAP_Code )]

# Get mapping ASFIS-ISSCAAP
isscaap2asfis <- ReadEBXGroup(gr_name = "Group_IsscaapGrp_Item")

#Get ICS groups for Species
ics4asfisList <- ReadEBXCodeList(cl_name = "SpeciesFaostat")
ics4asfis <- ics4asfisList[, .(Identifier, Faostat.ID)]
# Get mapping ICS-ISSCAAP
ics2isscaap <- ReadEBXGroup(gr_name = "Group_Faostat_IsscaapGrp")
setnames(ics2isscaap, names(ics2isscaap), c("ics", "isscaap"))

# Merge ASFIS-ISSCAAP
map_asfis <- merge(isscaap2asfis, isscaap, by.x = "Group", by.y = "Identifier")
map_asfis <- merge(map_asfis, asfis, by.x = "Member", by.y = "Identifier")
# Add ICS
map_asfis <- merge(map_asfis, ics2isscaap, by.x = "Group", by.y = "isscaap")
map_asfis <- merge(map_asfis, ics4asfis, by.x = "ics", by.y = "Identifier")

# Keep only needed dimensions and put right names
map_asfis <- map_asfis[, .(Faostat.ID, ISSCAAP_Code, Alpha.Code, NAME_en)]
setnames(map_asfis, names(map_asfis), c("ics", "isscaap", "fisheriesAsfis", "description"))

#-- Start processing global production ----

# Map to ICS
globalProductionMapping <- merge(globalProduction, map_asfis, by = c("fisheriesAsfis"), all.x = TRUE)

if(any(is.na(globalProductionMapping$ics))){
  
  notMappedSpecies <- unique(globalProductionMapping[is.na(ics)]$fisheriesAsfis)
  message(paste("These species are not mapped to any ICS group:", 
                paste(notMappedSpecies, collapse = ", ")))
}

# sum by ics, no species anymore

globalProductionAggr <- globalProductionMapping[, list(Value = sum(Value, na.rm = TRUE),
                                                       flagObservationStatus = max(flagObservationStatus),
                                                       flagMethod = "s"), by = list(geographicAreaM49_fi,
                                                                                    timePointYears,
                                                                                    measuredElement,
                                                                                    ics)]

# remove ISSCAAP not mapped to any ICS: Pearls 81, Coral 82, Sponges 83
# which correspond to Asfis: 
# 81: FSH, OSH (China)
# 82: CDE, CEL, COJ, COK (China) and COL (Greece)
# 83: SPO (Greece)

globalProductionAggr <- globalProductionAggr[!is.na(ics), ]

#-- Start processing commodity DB ----

# Map to ICS
commodityDBIcs <- merge(commodityDB, map_isscfc, by = "measuredItemISSCFC")
commodityDBIcs$measuredItemISSCFC <- as.character(commodityDBIcs$measuredItemISSCFC)

# Special commodities changes (China export for "034.4.1.5.2.45" goes to ICS 1543 instead of 1517,
# Greece export "034.1.5.9.90" goes to ICS 1514 instead of 1540) and 
# Some commodities are imported but not for food purposes, main example "ornamental fish". 
# These flows are deviated directly to "other utilizations"

# These hard-coded changes should be removed or put in a data.table if they increase
commodityDBIcs[ geographicAreaM49_fi == "156" & 
                  measuredItemISSCFC == "034.4.1.5.2.45" &
                  measuredElement == "5910",]$ics <- "1543"

commodityDBIcs[ geographicAreaM49_fi == "300" & 
                  measuredItemISSCFC == "034.1.5.9.90" &
                  measuredElement == "5910",]$ics <- "1514"


#-- Link table ----
# Link table for special period ICS group changes
link_table <- ReadDatatable("link_table")

## Checks on link table
# quantity different from 100% allocated
link_table[ , check := sum(percentage), by=c("geographic_area_m49","flow","start_year","end_year","from_code")]

if(any(link_table$check!=1)){
  message(paste0("Not 100% of the original quantity is allocated for link:" , paste0(link_table[link_table$check!=1,], collapse = " ")))
}

# Link table expressed in terms of "PRD", "TRD", "EXP", "IMP", "ALL"
# they translate into standard measuredElement through linkCorrespondence
linkCorrespondence <- data.table(flow = c("PRD", "TRD", "TRD", "EXP", "IMP", "ALL", "ALL", "ALL"), 
                                 measuredElement = c("5510", "5910", "5610",  "5910", "5610", "5510", "5910", "5610"))

link_table2 <- merge(link_table, linkCorrespondence, by = "flow", allow.cartesian = TRUE)

link_table2$end_year <- ifelse(link_table2$end_year == "LAST", max(as.numeric(commodityDBIcs$timePointYears)),
                               link_table2$end_year)

# Change ICS codes

commodityDBLink <- merge(commodityDBIcs, link_table2, 
                         by.x = c("geographicAreaM49_fi", "measuredElement", "ics"),
                         by.y = c("geographic_area_m49", "measuredElement", "from_code"), all.x = TRUE)

# Avoid NAs for periods
commodityDBLink$start_year <- ifelse(is.na(commodityDBLink$start_year), "0", commodityDBLink$start_year)
commodityDBLink$end_year <- ifelse(is.na(commodityDBLink$end_year), "0", commodityDBLink$end_year)

# Change ICS for defined periods
commodityDBLink$ics <- ifelse(!is.na(commodityDBLink$start_year) & 
                                as.numeric(commodityDBLink$timePointYears) >= as.numeric(commodityDBLink$start_year) &
                                as.numeric(commodityDBLink$timePointYears) <= as.numeric(commodityDBLink$end_year),
                              commodityDBLink$to_code, commodityDBLink$ics)

commodityDBLink[!is.na(percentage) , Value := Value*percentage]

# remove unnecessary dimensions
commodityDBLink <- commodityDBLink[ , c("flow", "start_year", "end_year", "percentage", "to_code", "check") := NULL]


##-- Other uses introduction ----

# Some commodities are not imported for food porpuses (e.g. "ornamental fish").
# Those flow are deviated to "other utilizations"

otherUses <- ReadDatatable('other_uses')

commodityDBotherUses <- merge(commodityDBLink, otherUses, 
                              by.x = c( "measuredItemISSCFC", "measuredElement", "ics"),
                              by.y = c("isscfc", "measured_element_orig", "ics"), all.x = TRUE)

commodityDBotherUses$measuredElement <- ifelse(is.na(commodityDBotherUses$measured_element_dest),
                                               commodityDBotherUses$measuredElement,
                                               commodityDBotherUses$measured_element_dest)

commodityDBotherUses <- commodityDBotherUses[ , c("label", "measured_element_dest", "fias_code") := NULL]

# Sum by ICS, no commodities anymore

commodityDBAggr <- commodityDBotherUses[ , list(Value = sum(Value, na.rm = TRUE),
                                                flagObservationStatus = max(flagObservationStatus),
                                                flagMethod = "s"),
                                         by = list(geographicAreaM49_fi,
                                                   timePointYears,
                                                   measuredElement,
                                                   ics)]

#-- SUA ----

SUA <- rbind(globalProductionAggr, commodityDBAggr)
SUA <- SUA[ , list(Value = sum(Value, na.rm = TRUE),
                   flagObservationStatus = max(flagObservationStatus),
                   flagMethod = "s"), by = list(geographicAreaM49_fi,
                                                timePointYears,
                                                measuredElement,
                                                ics)]


SUA <- SaveData(domain = "FisheriesCommodities", dataset = "fi_sua_unbalanced", data = SUA, waitTimeout = 2000000)

paste0(stats$inserted, " observations written, ",
       stats$ignored, " weren't updated, ",
       stats$discarded, " had problems.")

##-- send Email with notification of correct execution ----

## Initiate email
from = "sws@fao.org"
to = swsContext.userEmail
subject = "fi_PullDataToSua plug-in has correctly run"
body = "The plug-in has saved the SUAs in your session"

sendmailR::sendmail(from = from, to = to, subject = subject, msg = body)
paste0("Email sent to ", swsContext.userEmail)

