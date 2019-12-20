library(readxl)
library(data.table)
library(faosws) 
library(faoswsProcessing)
library(faoswsUtil)
library(faoswsModules)
library(faoswsFlag)
library(faoswsImputation)
library(faoswsStandardization)

#-- Commodity ----
setwd('C:/Users/Taglionic/Documents/Github/fisheriescommoditiesshinySWS')
if(CheckDebug()){
  
  SETTINGS = ReadSettings("token/sws.yml")
  
  ## If you're not on the system, your settings will overwrite any others
  R_SWS_SHARE_PATH = SETTINGS[["share"]]
  
  ## Define where your certificates are stored
  SetClientFiles(SETTINGS[["certdir"]])
  
  ## Get session information from SWS. Token must be obtained from web interface
  GetTestEnvironment(baseUrl = SETTINGS[["server"]],
                     token = SETTINGS[["token"]])
  
}

#----
setwd("C:/Users/Taglionic/Documents/Github/faoswsFisheryStandardization")

#-- Export approach ----
map <- readxl::read_xlsx("Input_documents/Mappings2017_18062019.xlsx", 
                         sheet = 1, col_names = TRUE)

map <- as.data.table(map)
setnames(map, names(map), c("measuredItemISSCFC", "Country", "geographicAreaM49_fi", 
                            "timePointYears", "measuredItemISSCFC_exp", "a", "b"))
map$measuredItemISSCFC <- gsub(" ", "", map$measuredItemISSCFC, fixed = TRUE)

map[, c("a","b") := NULL]

map <- map[-which(apply(map, 1, function(x)all(is.na(x)))),]

# put start year and end year
# if all timePointYear values are 2017 then the validity period is supposed to be "1948-LAST"
unique(map$timePointYears)

###Get Commodity Data
currentCountry <- as.character(unique(map$geographicAreaM49_fi))
# start_year <- "2000"
# end_year <- "2017"
# KeyComm <- DatasetKey(domain = "Fisheries Commodities", dataset = "commodities_total", dimensions = list(
#   geographicAreaM49_fi = Dimension(name = "geographicAreaM49_fi", keys = currentCountry), # GetCodeList("FisheriesCommodities", "commodities_total","geographicAreaM49_fi" )[,code]),
#   measuredItemISSCFC = Dimension(name = "measuredItemISSCFC", keys = GetCodeList("FisheriesCommodities", "commodities_total","measuredItemISSCFC" )[,code]),
#   measuredElement = Dimension(name = "measuredElement", keys = GetCodeList("FisheriesCommodities", "commodities_total","measuredElement" )[,code]),
#   timePointYears = Dimension(name = "timePointYears", keys = as.character(start_year:end_year) ))) # GetCodeList("FisheriesCommodities", "commodities_total","timePointYears" )[,code]
# 
# commodityDB0 <- GetData(KeyComm)

commodityDB0 <- read.csv("Input_documents/cdb62.csv")
commodityDB0 <- as.data.table(commodityDB0)
countryCommCdb <- unique(commodityDB0[, .(geographicAreaM49_fi, measuredItemISSCFC)])

mapExp <- map[ , c("Country","timePointYears") := NULL]

map_isscfc <- GetCodeList("FisheriesCommodities", "commodities_total","measuredItemISSCFC" )[,.(code, description)]

# check all ISSCFC codes are in SWS
all(mapExp$measuredItemISSCFC %in% map_isscfc$code)
all(mapExp$measuredItemISSCFC_exp %in% map_isscfc$code)


mapExp[!measuredItemISSCFC_exp %in% map_isscfc$code]

setkey(mapExp)
mapExp <- mapExp[!duplicated(mapExp)]

# Adding missing commodities present in the commodity database but not in the mapping

countryCommExp <- unique(mapExp[ , .(geographicAreaM49_fi, measuredItemISSCFC_exp)])

countries <- unique(countryCommCdb$geographicAreaM49_fi)

setComm <-  list()

for(i in 1:length(countries)){
  setComm[[i]]  <- countryCommCdb[geographicAreaM49_fi == countries[i] & !measuredItemISSCFC %in% countryCommExp[geographicAreaM49_fi == countries[i] ]$measuredItemISSCFC_exp, ]
}

setComm <- as.data.table(rbindlist(setComm))
setnames(setComm, "measuredItemISSCFC", "measuredItemISSCFC_exp")

comm2add<-rbind(setComm, countryCommExp)
setkey(comm2add)
# check no duplicates
comm2add[duplicated(comm2add)]
comm2add <- unique(comm2add)

# merge mapping table with missing objects
prod2addComplete <- merge(mapExp, comm2add, by = c("geographicAreaM49_fi", "measuredItemISSCFC_exp"),
                          suffixes = c("_map", "_missing"), all = TRUE)
prod2addComplete$measuredItemISSCFC_exp <- as.character(prod2addComplete$measuredItemISSCFC_exp)
prod2addComplete[ , measuredItemISSCFC := ifelse(is.na(measuredItemISSCFC), 
                                                 measuredItemISSCFC_exp, 
                                                 measuredItemISSCFC)]
prod2addComplete$start_year <- "1948"
prod2addComplete$end_year <- "LAST"
prod2addComplete$selection <- TRUE

setkey(prod2addComplete)
prod2addComplete[duplicated(prod2addComplete) ]

mapCompleteExp <- prod2addComplete[ ,.(geographicAreaM49_fi,
                                       start_year, end_year,
                                       measuredItemISSCFC,
                                       measuredItemISSCFC_exp, selection)]

setnames(mapCompleteExp, names(mapCompleteExp), c("geographic_area_m49_fi",
                                                  "start_year", "end_year",
                                                  "measured_item_isscfc",
                                                  "measured_item_isscfc_exp",
                                                  "selection"))

write.csv(mapCompleteExp, file = "C:/Users/Taglionic/Documents/Github/faoswsFisheryStandardization/Input_documents/isscfc_mapping_export_62countries.csv",
          row.names = FALSE)


#-- Primary production approach ----

mapP <- readxl::read_xlsx("Input_documents/Mappings2017_18062019.xlsx", 
                          sheet = 2, col_names = TRUE)
mapP <- as.data.table(mapP)
setnames(mapP, names(mapP), c("geographicAreaM49_fi", "Country", "measuredItemISSCFC", 
                              "isscaap", "asfis", "description"))

mapP$measuredItemISSCFC <- gsub(" ", "", mapP$measuredItemISSCFC, fixed = TRUE)
mapP <- mapP[-which(apply(mapP, 1, function(x)all(is.na(x)))),]

mapP$geographicAreaM49_fi <- as.character(mapP$geographicAreaM49_fi)
names(mapP)
mapP[ , c("Country", "description") := NULL]

# adapt the table to SWS data.table

mapP[is.na(asfis), asfis := "all"]

setkey(mapP)
mapP[duplicated(mapP)]
mapP <- mapP[!duplicated(mapP)]

countryCommProd <- unique(mapP[ , .(geographicAreaM49_fi, measuredItemISSCFC)])

countriesP <- unique(countryCommProd$geographicAreaM49_fi)

setCommP <-  list()

for(i in 1:length(countriesP)){
  setCommP[[i]]  <- countryCommCdb[geographicAreaM49_fi == countriesP[i] & !measuredItemISSCFC %in% countryCommProd[geographicAreaM49_fi == countriesP[i] ]$measuredItemISSCFC, ]
}

comm2addP<-rbind(as.data.table(rbindlist(setCommP)), countryCommProd)
setkey(comm2addP)
comm2addP[duplicated(comm2addP) ]
comm2addP <- unique(comm2addP)

# ISSCFC - ISSCAAP mapping

isscfc2isscaap <- read.csv("C:/Users/Taglionic/Documents/Github/faoswsFisheryStandardization/Input_documents/isscfc_isscaap_classification_CodesOnly.csv")
isscfc2isscaap <- as.data.table(isscfc2isscaap)
isscfc2isscaap <- isscfc2isscaap[ , .(isscaap, isscfc)]
setkey(isscfc2isscaap)
isscfc2isscaap <- unique(isscfc2isscaap[!is.na(isscfc) & isscaap != "X"])
isscfc2isscaap$isscaap <- as.character(isscfc2isscaap$isscaap)

addProd <- merge(comm2addP, mapP, by = c("geographicAreaM49_fi", "measuredItemISSCFC"), all = TRUE)

addISSCAAP <- merge(addProd, isscfc2isscaap, by.x = c("measuredItemISSCFC"), by.y = c("isscfc"), all.x = TRUE)

addISSCAAP[ , isscaap:= ifelse(is.na(isscaap.x), isscaap.y, isscaap.x)]
addISSCAAP[ , asfis := ifelse(is.na(asfis), "all", asfis)]
addISSCAAP[ , c("isscaap.x", "isscaap.y") := NULL]

addISSCAAP[is.na(isscaap)]
addISSCAAP <- addISSCAAP[!is.na(isscaap)]

addISSCAAP$start_year <- "1948"
addISSCAAP$end_year <- "LAST"
addISSCAAP$ratio <- 999
addISSCAAP$selection <- TRUE

mapCcodesP2 <- addISSCAAP[ , .(geographicAreaM49_fi,
                               start_year, end_year,
                               measuredItemISSCFC,
                               isscaap, asfis, 
                               ratio, selection)]

setnames(mapCcodesP2, names(mapCcodesP2), c("geographic_area_m49_fi",
                                            "start_year", "end_year",
                                            "measured_item_isscfc", 
                                            "isscaap", "asfis", 
                                            "ratio", "selection"))


setkey(mapCcodesP2)
mapCcodesP2[ duplicated(mapCcodesP2)]
mapCcodesP2 <- mapCcodesP2[ !duplicated(mapCcodesP2)]

write.csv(mapCcodesP2, file = "C:/Users/Taglionic/Documents/Github/faoswsFisheryStandardization/Input_documents/isscfc_mapping_prod_62countries.csv",
          row.names = FALSE)
