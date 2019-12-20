##' # National processed production aggregation process for the Fishery commodity database
##'
##' **Author: Charlotte Taglioni**
##'
##' **Description:**
##'
##' This module is designed to aggregate processed production
##' products classified at national level into the ISSCFC.
##' 
##' **Inputs:**
##'
##' * Trade datable: FT: mirrored (fishtrade_mirrored)


## Load the library
suppressMessages({
  library(faosws)
  #  library(faoswsUtil)
  library(data.table)
})

# Token QA
if(CheckDebug()){
  
  library(faoswsModules)
  SETTINGS = ReadSettings("sws.yml")
  
  ## If you're not on the system, your settings will overwrite any others
  R_SWS_SHARE_PATH = SETTINGS[["share"]]
  
  ## Define where your certificates are stored
  SetClientFiles(SETTINGS[["certdir"]])
  
  ## Get session information from SWS. Token must be obtained from web interface
  GetTestEnvironment(baseUrl = SETTINGS[["server"]],
                     token = SETTINGS[["token"]])
  
}

#-- Parameters ----

dataInterest <- swsContext.computationParams$dataInterest
yearInterest <- swsContext.computationParams$yearInterest



if(is.null(dataInterest) | is.na(dataInterest)){
  stop('Please choose the data of interest.')
}

if(is.null(yearInterest) | is.na(yearInterest)){
  stop('Please choose the year(s) of interest.')
}

#-- Processed production loading ----

if(dataInterest == 'Selected country/ies and year(s)' & yearInterest == 'session'){
  # country <- swsContext.computationParams$country
  # start and end year for standardization come from user parameters
  country <- paste("'", swsContext.datasets[[1]]@dimensions$geographicAreaM49_fi@keys[14], collapse = ",", "'")
  country <- gsub(" ", "", country)
  yearVals <- swsContext.datasets[[1]]@dimensions$timePointYears@keys[2]

  where <- paste("rep in (", country, ")", sep = "")
  trade0 <- ReadDatatable('fishtrade_mirrored', where = where)
  trade0 <- trade0[year %in% as.character(yearVals), ]
  
} else if(dataInterest == 'All countries' & yearInterest == 'session'){
  
  yearVals <- swsContext.datasets[[1]]@dimensions$timePointYears@keys
  trade0 <- ReadDatatable('fishtrade_mirrored')
  trade0 <- trade0[year %in% as.character(yearVals), ]
  
}else if(dataInterest == 'Selected country and years' & yearInterest == 'all'){
  
  country <- paste("'",swsContext.datasets[[1]]@dimensions$geographicAreaM49_fi@keys, collapse = ",", "'")
  country <- gsub(" ", "", country)
  where <- paste("rep in (", country, ")", sep = "")
  trade0 <- ReadDatatable('fishtrade_mirrored', where = where)
  
} else {
  
  trade0 <- ReadDatatable('fishtrade_mirrored')
  
}

trade0$flow <- as.character(trade0$flow)
trade0$year <- as.character(trade0$year)


# Select only needed columns 
# Adjust structure for value and weight and element values
# flow: 1 = import, 2 = export, 3 = re-export

tradeValue <- trade0[ , .(rep, flow, year,
                         isscfc_code, value_correction,
                         value_method, imputation_method)]


# replece elements with SWS value elements

tradeValue[flow == '1' , flow := '5622']
tradeValue[flow == '2' , flow := '5922']
tradeValue[flow == '3' , flow := '5923']


# set names according to the commodity dataset
setnames(tradeValue, c("rep", "flow", "year",
                  "isscfc_code", "value_correction",
                  "value_method", "imputation_method"),
         c("geographicAreaM49_fi", "measuredElement", "timePointYears",
           "measuredItemISSCFC", "Value", 
           "flag_method", "imputation_method"))


tradeWeight <- trade0[ , .(rep, flow, year,
                          isscfc_code, weight_correction,
                          weight_method, imputation_method)]

tradeWeight[flow == '1' , flow := '5610']
tradeWeight[flow == '2' , flow := '5910']
tradeWeight[flow == '3' , flow := '5912']

# set names according to the commodity dataset
setnames(tradeWeight, c("rep", "flow", "year",
                  "isscfc_code", "weight_correction",
                  "weight_method", "imputation_method"), 
         c("geographicAreaM49_fi", "measuredElement", "timePointYears",
           "measuredItemISSCFC", "Value",
           "flag_method", "imputation_method"))

# Delete unnecessary columns
trade <- rbind(tradeValue, tradeWeight)

# convert kg in tonnes and $ in 1000$
trade[ , Value := Value/1000]
# Change flag
trade[ is.na(flag_method), c('flagObservationStatus', 'flagMethod') :=
         list(' ', '-')]

trade[!is.na(flag_method), c('flagObservationStatus', 'flagMethod') :=
         list('I', 'e')]

trade[flag_method == 'Expert estimate', c('flagObservationStatus', 'flagMethod') :=
         list('E', 'f')]


trade[ , flag_method := NULL]

message('fi_trade2CommodityDB: Trade data loaded')

# Take columns of interest
dataFile <- trade[ , .(geographicAreaM49_fi, measuredElement, timePointYears, 
                          measuredItemISSCFC, Value, flagObservationStatus, flagMethod)]

# Get the dimension 
isscfc <- GetCodeList(domain = 'FisheriesCommodities', dataset = 'commodities_total',
                      dimension = 'measuredItemISSCFC')[ , code]

# Check all ISSCFC codes belong to the dimension 'measuredItemISSCFC'
if(!all(dataFile$measuredItemISSCFC %in% isscfc)){
  stop(paste0('Codes:', unique(dataFile[!measuredItemISSCFC %in% isscfc]$measuredItemISSCFC), ' do not belong to the measuredItemISSCFC dimension.'))
}

dataFile$Value <- as.numeric(dataFile$Value)

# Take all values by flag to impute the right observetion flag
dataFilebyflag <- dataFile[ , list(Value = sum(Value, na.rm = TRUE)), by = c("geographicAreaM49_fi", "measuredElement", 
                                                                             "timePointYears", "measuredItemISSCFC", 
                                                                             "flagObservationStatus")]

dataFile$flagObservationStatus <- ifelse(is.na(dataFile$flagObservationStatus), ' ', dataFile$flagObservationStatus)
dataFile$flagObservationStatus <- factor(dataFile$flagObservationStatus, 
                                         levels = c('M', 'O', 'N', ' ', 'X', 'T', 'E', 'I'), 
                                         ordered = TRUE)

# Calculate total value by ISSCFC
dataFilebyISSCFC <- dataFile[ , list(Value = sum(Value, na.rm = TRUE),
                                     flagObservationStatus = max(flagObservationStatus)), by = c("geographicAreaM49_fi", "measuredElement", 
                                                                                                 "timePointYears", "measuredItemISSCFC")]
dataFilebyISSCFC[is.na(flagObservationStatus) , flagObservationStatus := ' ']

# Compare data by ISSCFC and Flags with data only by ISSCFC
mergedFile <- merge(dataFilebyflag, dataFilebyISSCFC, by = c("geographicAreaM49_fi", "measuredElement", 
                                                             "timePointYears", "measuredItemISSCFC"),
                    suffixes = c("_byflag", "_total"), all=TRUE)

# Calculate ratio
mergedFile[ , ratio := Value_byflag/Value_total]
mergedFile[ , ratiocheck := sum(ratio, na.rm = TRUE), by = c("geographicAreaM49_fi", "measuredElement", 
                                                             "timePointYears", "measuredItemISSCFC")]
all(mergedFile$ratiocheck %in% c(0,1))

# If ratio over 0.5 then flag is the flag having more than 50% of data, other wise is the normal hierarchy that wins
mergedFile[!is.na(ratio) & ratio > 0.5 & flagObservationStatus_total != flagObservationStatus_byflag, flagObservationStatus_total := flagObservationStatus_byflag]

# flag method assigned consequently, i.e. flag combinations are: ( , -), (E, f), (O, -) or (M, -)
mergedFile[ , flagMethod := '-']
mergedFile[flagObservationStatus_total == ' ', flagMethod := 's']
mergedFile[flagObservationStatus_total == 'E', flagMethod := 'f']
mergedFile[flagObservationStatus_total %in% c('O', 'M'), flagMethod := '-']

# Data reshape
mergedFile[ , c("flagObservationStatus_byflag", "Value_byflag", "ratio", "ratiocheck") := NULL]
setnames(mergedFile, c("Value_total", "flagObservationStatus_total"), c("Value", "flagObservationStatus"))
mergedFile$flagObservationStatus <- as.character(mergedFile$flagObservationStatus)
mergedFile[flagObservationStatus == " ", flagObservationStatus := ""]
setkey(mergedFile, geographicAreaM49_fi, measuredElement, timePointYears, measuredItemISSCFC, Value, flagObservationStatus)

# File to save ready
data2save <- mergedFile[!duplicated(mergedFile)]
message('fi_trade2CommodityDB: Data ready.')
#-- Metadata ----

# Approach used goes in the metadata
metadataFile <- trade[ , .(geographicAreaM49_fi, measuredElement, timePointYears, 
                              measuredItemISSCFC, imputation_method)]
setkey(metadataFile)
metadataFile <- unique(metadataFile)

# Message
metadataFilePresent <- metadataFile[!is.na(imputation_method) , list(imputation_method = paste(imputation_method, collapse = ', ')),
                                    by = c("geographicAreaM49_fi", "measuredElement",
                                           "timePointYears","measuredItemISSCFC")]
setnames(metadataFilePresent, 'imputation_method', 'Metadata_Value')

# Metadata structure
metadata2save <- metadataFilePresent[, `:=` (Metadata = 'GENERAL',
                                             Metadata_Element = 'COMMENT',
                                             Metadata_Language = 'en')]

message('fi_trade2CommodityDB: Metadata ready.')

save <- SaveData(domain = "FisheriesCommodities", dataset = 'commodities_total', 
                 data = data2save, metadata = metadata2save, waitTimeout = 100000)

paste0("ISSCFC commodity aggregation completed successfully!!! ",
       save$inserted, " observations written, ",
       save$ignored, " weren't updated, ",
       save$discarded, " had problems.")

## Initiate email
from = "sws@fao.org"
to = swsContext.userEmail
subject = "Commodity dataset aggregation"
body = paste0("Data have been properly aggrergated from the source datatable. There have been: ", 
              save$inserted, " observations written, ",
              save$ignored, " weren't updated, ",
              save$discarded, " had problems.")


sendmail(from = from, to = to, subject = subject, msg = body)

paste0("ISSCFC commodity aggregation completed successfully!!! ",
       save$inserted, " observations written, ",
       save$ignored, " weren't updated, ",
       save$discarded, " had problems. 
       Plugin has sent an email to ", swsContext.userEmail)
