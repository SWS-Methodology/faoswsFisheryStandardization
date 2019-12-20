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
##' * Processed prodution datable: processed_prod_national_detail_imputed


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
country <- paste("'",swsContext.datasets[[1]]@dimensions$geographicAreaM49_fi@keys, collapse = ",", "'")
country <- gsub(" ", "", country)
yearVals <- swsContext.datasets[[1]]@dimensions$timePointYears@keys
where <- paste("geographicaream49_fi in (", country, ")", sep = "")
procprod <- ReadDatatable('processed_prod_national_detail_imputed', where = where)
procprod <- procprod[timepointyears %in% as.character(yearVals), ]

} else if(dataInterest == 'All countries' & yearInterest == 'session'){
  
  yearVals <- swsContext.datasets[[1]]@dimensions$timePointYears@keys
  procprod <- ReadDatatable('processed_prod_national_detail_imputed')
  procprod <- procprod[timepointyears %in% as.character(yearVals), ]
  
}else if(dataInterest == 'Selected country and years' & yearInterest == 'all'){
  
  country <- paste("'",swsContext.datasets[[1]]@dimensions$geographicAreaM49_fi@keys, collapse = ",", "'")
  country <- gsub(" ", "", country)
  where <- paste("geographicaream49_fi in (", country, ")", sep = "")
  procprod <- ReadDatatable('processed_prod_national_detail_imputed', where = where)
  
} else {
  
  procprod <- ReadDatatable('processed_prod_national_detail_imputed')
  
}


# Delete unnecessary columns
procprod <- procprod[ , c("nationalquantity", "nationalquantityunit",
                          "id_isscfc", "nationalcode", "nationaldescription", "remarks", 
                          "id_nationalcode", "measureditemnational") := NULL]

# set names according to the commodity dataset
setnames(procprod, c("geographicaream49_fi", "measuredelement", "timepointyears",
                     "measureditemisscfc", "quantitymt", "flagobservationstatus",
                     "flagmethod", "approach"), 
         c("geographicAreaM49_fi", "measuredElement", "timePointYears",
           "measuredItemISSCFC", "Value", "flagObservationStatus",
           "flagMethod", "approach"))

message('fi_ProcProd2CommodityDB: Processed production datat loaded')

# Take columns of interest
dataFile <- procprod[ , .(geographicAreaM49_fi, measuredElement, timePointYears, 
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
message('fi_ProcProd2CommodityDB: Data ready.')
#-- Metadata ----

# Approach used goes in the metadata
metadataFile <- procprod[ , .(geographicAreaM49_fi, measuredElement, timePointYears, 
                              measuredItemISSCFC, approach)]
setkey(metadataFile)
metadataFile <- unique(metadataFile)

# Message
metadataFilePresent <- metadataFile[!is.na(approach) , list(approach = paste(approach, collapse = ', ')),
                                     by = c("geographicAreaM49_fi", "measuredElement",
                                            "timePointYears","measuredItemISSCFC")]
setnames(metadataFilePresent, 'approach', 'Metadata_Value')

# Metadata structure
metadata2save <- metadataFilePresent[, `:=` (Metadata = 'GENERAL',
                                       Metadata_Element = 'COMMENT',
                                       Metadata_Language = 'en')]

message('fi_ProcProd2CommodityDB: Metadata ready.')

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
