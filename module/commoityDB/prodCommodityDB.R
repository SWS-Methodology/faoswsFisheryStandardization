##  This plugin is 
##
##


suppressMessages({
  library(data.table)
  library(faosws)
  library(faoswsImputation)
  library(faoswsFlag)
  library(faoswsProcessing)
  library(faoswsEnsure)
  library(faoswsProduction)
  library(ggplot2)
  }) 

# 
#  if(CheckDebug()){
#    
#    SETTINGS = ReadSettings("sws1.yml")
#    
#    ## If you're not on the system, your settings will overwrite any others
#    R_SWS_SHARE_PATH = SETTINGS[["share"]]
#    
#    ## Define where your certificates are stored
#    SetClientFiles(SETTINGS[["certdir"]])
#    
#    ## Get session information from SWS. Token must be obtained from web interface
#    GetTestEnvironment(baseUrl = SETTINGS[["server"]],
#                       token = SETTINGS[["token"]])
#    
#  }
#  
#  KeyCommodityDB = DatasetKey(domain = "Fisheries", dataset = "fi_commodity_db", dimensions = list(
#    geographicAreaM49 = Dimension(name = "geographicAreaM49", keys = currentCountry),
#    measuredItemISSCFC = Dimension(name = "measuredItemISSCFC", keys = GetCodeList("Fisheries", "fi_commodity_db","measuredItemISSCFC" )[,code]),
#    measuredElement = Dimension(name = "measuredElement", keys = c("5510")),
#    timePointYears = Dimension(name = "timePointYears", keys = as.character(c(2000:2016) ))
#  ))
#  
#  ##Get Global Production Data
#  commodityDB_quantity=GetData(KeyCommodityDB)


##########################################################################################################################################################
## Pull data from a local CSV file (downloaded form fishStatJ) and reshape it: line 45-98 won't be used anymore, once
## the commodity DB will be properly migrated in the SWS the GetData function (line 41) will pull data directly from the SWS
## together with FLAGS!

commodityDB_quantity = fread("data/commodityDB_Quantity.csv", header = TRUE)
commodityDB_quantity = melt(
  commodityDB_quantity,
  id.vars = colnames(commodityDB_quantity)[c(1:7)],
  measure.vars = colnames(commodityDB_quantity)[c(8:48)],
  variable.name = "timePointYears",
  value.name = "Value"
)
commodityDB_quantity[, timePointYears := as.numeric(as.character(timePointYears))]

#Prof of concept: impute figures for 2014, 2015, 2016

## Assign FLAGS: data are currently imported from a csv file downloaded directly from fishStatJ
## Obviously once the measuredItemISSCFC classification will be properly populated into the SWS
## the module has to pull directly from the SWS

## Manage F figures: since flags had been stored in the same cell together with numeric figures, to reassign
## the proper official FLAG we started looking on those figures that have been manually adjusted and flagged as "F"

## The flag combination (observation +method flag) that is used to identify manual adjustement
## that cannot be overwritten by any R routine is (E, f):


commodityDB_quantity[grepl("F",Value) , flagObservationStatus:="E"]
commodityDB_quantity[grepl("F",Value) & timePointYears<"2014", flagMethod:="f"]
commodityDB_quantity[grepl("F",Value) & timePointYears>"2013", flagMethod:="-"]
## Note that all the manual adjustment up to 2013 are flagged as (E,f) and PROTECTED (not overwritten)
## while all those figures obtained as manual judgment after 2013 are flagged as (E, -) and not protected.


## Remove the flag from the numeric cells:
commodityDB_quantity[,Value:=gsub("F","" ,Value)]
commodityDB_quantity[, Value := as.numeric(Value)]
commodityDB_quantity[, measuredElement := as.character(measuredElement)]

## We assume that all the other figures are official and coming from questionnaire "line 58--> we assigne "q" to the method flag
## (this is not completly correct)
## Hint: the method flag can also be unknown, if official data comes from multiple sources and there is no way to
## discriminat data come from different sources.

commodityDB_quantity[is.na(flagObservationStatus) & !is.na(Value),flagObservationStatus:=""]
commodityDB_quantity[is.na(flagMethod) & !is.na(Value),flagMethod:="q"]

# I keep only the columns I need removing labels:
commodityDB_quantity=commodityDB_quantity[,.(Country_code,
                                             ISSCFC_Code,
                                             measuredElement,
                                             timePointYears,
                                             Value,
                                             flagObservationStatus,
                                             flagMethod)]

setnames(commodityDB_quantity, c("Country_code","ISSCFC_Code"), c("geographicAreaM49","measuredItemISSCFC"))
##########################################################################################################################################################

## Rename this dataset: is the reference to test my output.
## I copy the commodityDB_quantity without any modification (except for flags), and I will use this data.table 
## as benchmark to evaluate the 

trade_commodityDB_quantity=copy(commodityDB_quantity)
trade_commodityDB_quantity=trade_commodityDB_quantity[(!is.na(Value) & !is.na(flagObservationStatus)),]


## discard TRADE
commodityDB_quantity=commodityDB_quantity[measuredElement=="51"]

## It is important to fill missing records only to those series that are currently open.
## Are defined "open series "
## It is not simple to identify the cell that are empty because no data must be there from 
## missing data that has to be imputed through


seriesToBlock=commodityDB_quantity[is.na(Value) & timePointYears=="2013",]
seriesToBlock=unique(seriesToBlock[,.(geographicAreaM49, measuredElement, measuredItemISSCFC)])
commodityDB_quantity=commodityDB_quantity[!seriesToBlock,,on=c("geographicAreaM49","measuredElement", "measuredItemISSCFC")]

commodityDB_quantity=commodityDB_quantity[(!is.na(Value) & !is.na(flagObservationStatus)),]

##############################################################################################################

## rechape the dataset in order to reproduce the SWS data structure
## apply remove non protected flag from 2014 onwords

commodityDB_quantity=removeNonProtectedFlag(commodityDB_quantity, keepDataUntil="2013")

commodityDB_quantity=expandYear(commodityDB_quantity,areaVar="geographicAreaM49",
                                elementVar="measuredElement" ,
                                itemVar="measuredItemISSCFC",
                                yearVar="timePointYears" ,
                                valueVar="2016")


##Imputation
#removeNoInfo(commodityDB_quantity)

fishImputationParamenters=defaultImputationParameters()
fishImputationParamenters$imputationValueColumn="Value"
fishImputationParamenters$imputationFlagColumn="flagObservationStatus"
fishImputationParamenters$imputationMethodColumn="flagMethod"
fishImputationParamenters$byKey=c("geographicAreaM49","measuredItemISSCFC")
fishImputationParamenters$estimateNoData=FALSE


commodityDB_quantity=removeNoInfo(commodityDB_quantity[measuredElement=="51"],
             value="Value", observationFlag = "flagObservationStatus",
             byKey = c(fishImputationParamenters$byKey, "measuredElement"))

commodityDB_quantityImputed=imputeVariable(commodityDB_quantity[measuredElement=="51"],
                                            imputationParameters=fishImputationParamenters)

commodityDB_quantityImputed=commodityDB_quantityImputed[timePointYears>2013]

#SaveData(commodityDB_quantityImputed)
##############################################################################################################

## Re-shape the just imputed 
Plot_imputed=copy(commodityDB_quantityImputed)
Plot_imputed=Plot_imputed[flagObservationStatus=="I",]
Plot_imputed[,imputed:="new_imputation"]
trade_commodityDB_quantity[, imputed:="old_series"]

compareImputedvsNot=rbind(Plot_imputed,trade_commodityDB_quantity)

commodityDB_quantityImputedCheck1=merge(commodityDB_quantity, commodityDB_quantityImputed, by=c("measuredElement", "geographicAreaM49", "measuredItemISSCFC", "timePointYears"),
      suffixes = c("_preImp", "_postImp"), all.y = TRUE)



plotCompare(compareImputedvsNot,geoVar="geographicAreaM49",elVar="measuredElement", elVector="51",itemVar="measuredItemISSCFC", 
            directory="C:/Users/ROSA/Desktop/Fisheries", title="comparison",status="imputed", geomPoint = "flagObservationStatus")
