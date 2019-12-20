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
                     token = "3c2b2fac-d5ab-47e5-bb74-dae5f83de3c2")
  
}
# sessionCountry <- c("36", "116", "250", "364", "372", "586", "604", "724", "840", "862", "894")
sessionCountry <- c("116", "372", "586", "724", "840", "862", "894")
yearVals <- as.character(1961:2016)

KeyComm <- DatasetKey(domain = "FisheriesCommodities", dataset = "fi_sua_balanced_control", dimensions = list(
  geographicAreaM49_fi = Dimension(name = "geographicAreaM49_fi", keys = sessionCountry),
  measuredItemFaostat_L2 = Dimension(name = "measuredItemFaostat_L2", keys = GetCodeList("FisheriesCommodities", "fi_sua_balanced_control","measuredItemFaostat_L2" )[,code]),
  measuredElementSuaFbs = Dimension(name = "measuredElementSuaFbs", keys = GetCodeList("FisheriesCommodities", "fi_sua_balanced_control","measuredElementSuaFbs" )[,code]),
  timePointYears = Dimension(name = "timePointYears", keys = yearVals )))


## It should be the commodity DB validated
# KeyComm <- DatasetKey(domain = "Fisheries Commodities", dataset = "commodities_total_validated", dimensions = list(
#   geographicAreaM49_fi = Dimension(name = "geographicAreaM49_fi", keys = GetCodeList("FisheriesCommodities", "commodities_total","geographicAreaM49_fi" )[,code]),
#   measuredItemISSCFC = Dimension(name = "measuredItemISSCFC", keys = GetCodeList("FisheriesCommodities", "commodities_total","measuredItemISSCFC" )[,code]),
#   measuredElement = Dimension(name = "measuredElement", keys = GetCodeList("FisheriesCommodities", "commodities_total","measuredElement" )[,code]),
#   timePointYears = Dimension(name = "timePointYears", keys = GetCodeList("FisheriesCommodities", "commodities_total","timePointYears" )[,code])))

SUAFr <- SUA2sws #GetData(KeyComm)
balancingItems <- SUAFr[flagMethod == 'b', ]
balancingItems[ , c("Value", "flagObservationStatus", "timePointYears") := NULL]
setkey(balancingItems)
balancingItems <- unique(balancingItems)
noTime <- unique(balancingItems[ , .(geographicAreaM49_fi, measuredElementSuaFbs, measuredItemFaostat_L2, flagMethod)])

noTime[ , c('start_year', 'end_year', 'share') := list('1948', 'LAST', 1) ]
noTime[ , c("flagMethod") := NULL]

setnames(noTime,  c("geographicAreaM49_fi", "measuredItemFaostat_L2", "measuredElementSuaFbs"), 
         c("geographic_area_m49_fi", "measured_item_faostat_l2", "measured_element"))

changeset <- Changeset('balancing_elements')
AddInsertions(changeset, noTime)
Finalize(changeset)

