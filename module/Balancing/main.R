##' # Balancing process for the Supply Utilization Account
##'
##' **Author: Charlotte Taglioni**
##'
##' **Description:**
##'
##' This module is designed to balance the SUA so that all the availability
##' is allocated among elements
##' 
##' **Inputs:**
##'
##' * SUA unbalanced resulting from fi_SuaFilling plugin
##' 
##' * Datatables for balancing elements

##'
##' **Flag assignment:**
##'
##' | Observation Status Flag | Method Flag|
##' | --- | --- | --- |


## load the library

suppressMessages({
  library(faosws)
  library(faoswsUtil)
  library(data.table)
})

#-- Token QA ----
if(CheckDebug()){
  
  SETTINGS = ReadSettings("module/Balancing/sws.yml")
  
  ## If you're not on the system, your settings will overwrite any others
  R_SWS_SHARE_PATH = SETTINGS[["share"]]
  
  ## Define where your certificates are stored
  SetClientFiles(SETTINGS[["certdir"]])
  
  ## Get session information from SWS. Token must be obtained from web interface
  GetTestEnvironment(baseUrl = SETTINGS[["server"]],
                     token = SETTINGS[["token"]])
  
}

#-- Parameters ----
message("Getting parameters")

sessionCountry <- swsContext.datasets[[1]]@dimensions$geographicAreaM49_fi@keys 
yearVals <- swsContext.datasets[[1]]@dimensions$timePointYears@keys


message(paste('Session country ', sessionCountry))

#-- Needed datasets ----

message("Pulling data from SUA unbalanced")

KeySUA <- DatasetKey(domain = "FisheriesCommodities", dataset = "fi_sua_unbalanced", dimensions = list(
  geographicAreaM49_fi = Dimension(name = "geographicAreaM49_fi", keys = sessionCountry),
  measuredElement = Dimension(name = "measuredElement", keys = GetCodeList("FisheriesCommodities", "fi_sua_unbalanced","measuredElement" )[,code]),
  measuredItemFaostat_L2 = Dimension(name = "measuredItemFaostat_L2", keys =  GetCodeList("FisheriesCommodities", "fi_sua_unbalanced","measuredItemFaostat_L2" )[,code] ),
  timePointYears = Dimension(name = "timePointYears", keys = yearVals )))

SUAunbal <- GetData(KeySUA)

elementSignTable <- data.table( description = c("Input [t]", "Production [t]", "Import Quantity [t]",
                                                "Stock Variation [t]", "Export Quantity [t]", "Re-Exports Quantity [t]", 
                                                "Extraction Rate [kg/t]", "Feed [t]", "Seed [t]", "Waste [t]",
                                                "Processing [t]", "Food [t]", "Residual other uses [t]"),
                                measuredElement = c("5302", "5510", "5610", "5071", "5910", "5912", "5423", "5520", 
                                                    "5525", "5016", "5023", "5141", "5166"),
                                FBSsign = c(0, 1, 1, 
                                            1, -1, -1,
                                            0, -1, -1, -1, -1, -1, -1) )

SUAunbal <-  merge(SUAunbal, elementSignTable[, .(measuredElement, FBSsign)], by = "measuredElement", all.x = TRUE)

if(any(is.na(SUAunbal$FBSsign))){
  stop('There is an element in the SUA not included in the availability calculation.')
}




SUAunbal[, availability := sum(Value * FBSsign, na.rm = TRUE), 
         by = list(geographicAreaM49_fi, timePointYears, measuredItemFaostat_L2)]

#----------------
message('Pulling balancing elements')

balancingElements <- ReadDatatable('balancing_elements')
setnames(balancingElements, names(balancingElements), c("geographicAreaM49_fi", 
                                                  "measuredItemFaostat_L2",
                                                  "measuredElement",
                                                  "start_year", "end_year", "share"))

balancingElements[ end_year == "LAST"]$end_year <- as.character(max(unique(as.numeric(SUAunbal$timePointYears))))

balancingValues <- unique(SUAunbal[ , .(geographicAreaM49_fi, timePointYears , measuredItemFaostat_L2, availability) ])

balancing <- merge(balancingElements, balancingValues, by = c("geographicAreaM49_fi","measuredItemFaostat_L2"))
setnames(balancing, c("availability"), c("Value"))


balancing2merge <- balancing[ as.numeric(timePointYears) >= as.numeric(start_year) & as.numeric(timePointYears) <= as.numeric(end_year), Value := Value*share]
balancing2merge[ , c('start_year', 'end_year', 'share') := NULL]
balancing2merge[ , c('flagObservationStatus', 'flagMethod') := list('I','b')]

SUAbal <- rbind(SUAunbal[ , .(geographicAreaM49_fi, timePointYears , 
                              measuredItemFaostat_L2, measuredElement, 
                              Value, flagObservationStatus, flagMethod)], balancing2merge)

# Sum if there is a balancing elements that was already present there
SUAbal[ , list(ValueAggr = sum(Value, na.rm = TRUE), 
               flagObservationStatusAggr = max(flagObservationStatus),
               flagMethodAggr = "s"),
        by=c("geographicAreaM49_fi",
             "measuredItemFaostat_L2",
             "measuredElement",
             "timePointYears")]


SUAbalAvail <- merge(SUAbal, elementSignTable[, .(measuredElement, FBSsign)], by = "measuredElement", all.x = TRUE)

SUAbalAvail[, availability := sum(Value * FBSsign, na.rm = TRUE), 
            by = list(geographicAreaM49_fi, timePointYears, measuredItemFaostat_L2)]

if(any(round(SUAbalAvail$availability) != 0)){
  message("Balancing process has failed. Please check data.")
  fail <- SUAbalAvail[round(availability) != 0, ]
  print(fail)
}



SUAbalAvail[, c("FBSsign", "availability"):=NULL]

#-- SUA with nutrient ----
# ## Add NutrientFactors
nutrientFactors <- ReadDatatable("fishery_nutrient")
nutrientFactors$calories <- as.numeric(nutrientFactors$calories)
nutrientFactors$proteins <- as.numeric(nutrientFactors$proteins)
nutrientFactors$fats <- as.numeric(nutrientFactors$fats)

SUA_with_nutrient <- merge(SUAbalAvail, nutrientFactors, by.x = "measuredItemFaostat_L2", by.y = "ics", all.x = TRUE)

SUA_with_nutrient[measuredElement=="5141", calories:=Value*calories]
SUA_with_nutrient[measuredElement=="5141", proteins:=Value*proteins]
SUA_with_nutrient[measuredElement=="5141", fats:=Value*fats]
SUA_with_nutrient[measuredElement!="5141",`:=`(c("calories", "proteins", "fats"),list(0,0,0) )]

SUAnutrients <-  melt.data.table(SUA_with_nutrient[measuredElement=="5141", ],
                 id.vars = c('geographicAreaM49_fi', 'measuredItemFaostat_L2', 'timePointYears'),
                 measure.vars = c('calories', 'proteins','fats'),
                 variable.name = 'measuredElement', value.name = 'Value')
SUAnutrients$measuredElement <- ifelse(SUAnutrients$measuredElement == 'calories', '261',
                                   ifelse(SUAnutrients$measuredElement == 'proteins', '271',
                                          ifelse(SUAnutrients$measuredElement == 'fats', '281', SUAnutrients$measuredElement)))

SUAnutrients[ , c('flagObservationStatus', 'flagMethod') := list('I','i')]
SUAnutrients <- unique(SUAnutrients)

SUA_with_nutrient[ , c('calories', 'proteins','fats') := NULL] 

SUA2save <- rbind(SUA_with_nutrient, SUAnutrients)
message("Saving data")
stats <- SaveData(domain = "FisheriesCommodities", 
                  dataset = "fi_sua_unbalanced", 
                  data = SUA2save, waitTimeout = 2000000)

message <- paste0("SUA balancing and nutrients inclusion process completed successfully!!! ",
       stats$inserted, " observations written, ",
       stats$ignored, " weren't updated, ",
       stats$discarded, " had problems.")

##-- send Email with notification of correct execution ----

## Initiate email
from = "sws@fao.org"
to = swsContext.userEmail
subject = "fi_SuaBalancing plug-in has correctly run"
body = message

sendmailR::sendmail(from = from, to = to, subject = subject, msg = body)
paste0(message, "Email sent to ", swsContext.userEmail)


