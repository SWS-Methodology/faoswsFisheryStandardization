#########################################################################################################
# Calculate Global Production
#
# 02-Feb-2018  first release
# 05-Feb-2018  fixed flags and hidden attributes
# 27-Jul-2018  added PROD; fixed aggregation of Status=N
# 07-Sep-2018  adjusted waitTimeout = 10000
# 18-Jun-2019  added wipe of previous data
#########################################################################################################
# IN  Aquaculture (aqua)
# IN  Capture (capture)
# OUT Global Production by Source (fi_global_production_source)
# OUT Global Production (fi_global_production)
#########################################################################################################
# WARNING: check carefully before running: this script generates history/cannot be undone
# NOTE: needs to run as core as it writes to datasets (not session)
#########################################################################################################

library(dplyr)
library(data.table)
library(faosws)

#-- Thomas Berger -----
# if(CheckDebug()) {
#   #setwd("C:/USR/Manuals/2019/FIPS/SWS/GlobalProduction")
#   
#   # session for QA
#   SetClientFiles("C:/Users/berger/Documents/R-QA")
#   GetTestEnvironment("https://hqlqasws1.hq.un.fao.org:8181/sws", "4399d773-bd1c-4b63-972a-cbe6ed706256")
#   
#   # session for PROD
#   # GetTestEnvironment("https://hqlprswsas1.hq.un.fao.org:8181/sws", "ed9ab545-7812-4c87-9465-5dd08f40c0b8")
# }

#-- Charlotte Taglioni -----
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

#########################################################################################################
# calculate a new flag for the aggregate value
#########################################################################################################

#-- Thomas Berger -----
myAggregate<- function(quantity, flag) {
  if (length(quantity) == 1) {
     # for a single observation, just use the flag
     return (as.character(flag))
  }

  # for more than 1 observation: aggregate by flags
  agg1 <- aggregate(quantity, by=list(flag), FUN=sum)
  # agg1 has 2 variables: group, num
  
  # order by quantity, largest quantity first
  agg1 <- agg1[ order(-agg1[,2]), ]

  
  # if the total quantity>0 and the flag of te largest value is 'E' or '' then use it
  if (sum(quantity)>0  & (agg1[1,1] == "E" | agg1[1,1] == "" | agg1[1,1] == "I")) {
    return (as.character(agg1[1,1]))
  }
  
  # if there is a single "N" use it (takes precedence)
  if (nrow(agg1[agg1[,1] == "N",])>0) {
     return("N")
  }
  
  # if there is one "M" use it
  if (nrow(agg1[agg1[,1] == "M",])>0) {
    return("M")
  }
  
  # otherwise it's ' '
  return("")
}

#########################################################################################################
#GetDatasetNames("Fisheries")
#GetDatasetNames("FisheriesCommon")
#GetDatasetConfig ("Fisheries","aqua")
#GetDatasetConfig ("Fisheries","capture")

#320: FI_002                    Quantity [No] = only capture
#321: FI_001                Quantity [Tonnes] = both capture and aquaculture

#[1] "geographicAreaM49_fi"      "fisheriesAsfis"            "fisheriesProductionSource"
#[4] "fisheriesCatchArea"        "measuredElement"           "timePointYears"    

dim1 = Dimension(name = "geographicAreaM49_fi", keys = GetCodeList("Fisheries", "aqua", "geographicAreaM49_fi")$code)
dim2 = Dimension(name = "fisheriesAsfis", keys = GetCodeList("Fisheries", "aqua", "fisheriesAsfis")$code)
dim3 = Dimension(name = "fisheriesProductionSource", keys = GetCodeList("Fisheries", "aqua", "fisheriesProductionSource")$code)
dim4 = Dimension(name = "fisheriesCatchArea", keys = GetCodeList("Fisheries", "aqua", "fisheriesCatchArea")$code)
dim5 = Dimension(name = "measuredElement", keys = "FI_001")
dim6 = Dimension(name = "timePointYears", keys = GetCodeList("Fisheries", "aqua", "timePointYears")$code) 
key1 <- DatasetKey(domain = "Fisheries", dataset = "aqua", dimensions = list(dim1, dim2, dim3, dim4, dim5, dim6))
aqua_quantity <- GetData(key1)

#[1] "geographicAreaM49_fi" "fisheriesAsfis"       "fisheriesCatchArea"   "measuredElement"     
#[5] "timePointYears"   
dim1 = Dimension(name = "geographicAreaM49_fi", keys = GetCodeList("Fisheries", "capture", "geographicAreaM49_fi")$code)
dim2 = Dimension(name = "fisheriesAsfis", keys = GetCodeList("Fisheries", "capture", "fisheriesAsfis")$code)
dim4 = Dimension(name = "fisheriesCatchArea", keys = GetCodeList("Fisheries", "capture", "fisheriesCatchArea")$code)
dim5 = Dimension(name = "measuredElement", keys = "FI_001")
dim6 = Dimension(name = "timePointYears", keys = GetCodeList("Fisheries", "aqua", "timePointYears")$code) 
key2 <- DatasetKey(domain = "Fisheries", dataset = "capture", dimensions = list(dim1, dim2, dim4, dim5, dim6))
cap_quantity <- GetData(key2)

dim5 = Dimension(name = "measuredElement", keys = "FI_002")
key2 <- DatasetKey(domain = "Fisheries", dataset = "capture", dimensions = list(dim1, dim2, dim4, dim5, dim6))
cap_numbers <- GetData(key2)

dim1 = Dimension(name = "geographicAreaM49_fi", keys = GetCodeList("Fisheries", "fi_global_production_source", "geographicAreaM49_fi")$code)
dim2 = Dimension(name = "fisheriesAsfis", keys = GetCodeList("Fisheries", "fi_global_production_source", "fisheriesAsfis")$code)
dim3 = Dimension(name = "fisheriesProductionSource", keys = GetCodeList("Fisheries", "fi_global_production_source", "fisheriesProductionSource")$code)
dim4 = Dimension(name = "fisheriesCatchArea", keys = GetCodeList("Fisheries", "fi_global_production_source", "fisheriesCatchArea")$code)
dim5 = Dimension(name = "measuredElement", keys = c("FI_001", "FI_002"))
dim6 = Dimension(name = "timePointYears", keys = GetCodeList("Fisheries", "fi_global_production_source", "timePointYears")$code) 
key1 <- DatasetKey(domain = "Fisheries", dataset = "fi_global_production_source", dimensions = list(dim1, dim2, dim3, dim4, dim5, dim6))
globalprod_source <- GetData(key1)

dim1 = Dimension(name = "geographicAreaM49_fi", keys = GetCodeList("Fisheries", "fi_global_production", "geographicAreaM49_fi")$code)
dim2 = Dimension(name = "fisheriesAsfis", keys = GetCodeList("Fisheries", "fi_global_production", "fisheriesAsfis")$code)
dim4 = Dimension(name = "fisheriesCatchArea", keys = GetCodeList("Fisheries", "fi_global_production", "fisheriesCatchArea")$code)
dim5 = Dimension(name = "measuredElement", keys = c("FI_001", "FI_002"))
dim6 = Dimension(name = "timePointYears", keys = GetCodeList("Fisheries", "fi_global_production", "timePointYears")$code) 
key1 <- DatasetKey(domain = "Fisheries", dataset = "fi_global_production", dimensions = list(dim1, dim2, dim4, dim5, dim6))
globalprod <- GetData(key1)

message('GP: Data loaded')
#########################################################################################################
# calculate Global Production (by source)
#########################################################################################################

# merge aqua_quantity and cap_quantity
aqua_quantity$flagCurrency <- NULL
cap_quantity$fisheriesProductionSource <- "CAPTURE"
globalprod_source_new <- rbind (aqua_quantity, cap_quantity)

# merge capture numbers
cap_numbers$fisheriesProductionSource <- "CAPTURE"
globalprod_source_new <- rbind (globalprod_source_new, cap_numbers)

# wipe existing values
# dat <- merge(globalprod_source[,1:7], globalprod_source_new[,1:7], by = c('geographicAreaM49_fi','fisheriesAsfis','fisheriesCatchArea','fisheriesProductionSource','measuredElement','timePointYears'), all = TRUE, suffixes = c("_1", ""))

#-- Charlotte Taglioni -----
dat <- merge(globalprod_source[, 1:7, with = FALSE], globalprod_source_new[,1:7, with = FALSE], 
             by = c('geographicAreaM49_fi','fisheriesAsfis', 'fisheriesCatchArea',
                    'fisheriesProductionSource', 'measuredElement','timePointYears'), 
             all = TRUE, suffixes = c("_1", ""))

#-- Thomas Berger -----
wipe_value <- dat[is.na(dat$Value),]
wipe_value$Value_1  <- NULL
wipe_value$flagObservationStatus <- NA
wipe_value$flagMethod <- NA
wipe_value$flagCurrency <- NA
wipe_value <- as.data.table(wipe_value)

# write NA values back to SWS in order to wipe existing data
if (nrow(wipe_value) > 0) {
  globalprod_source_new <- rbind (globalprod_source_new, wipe_value)
}

setattr(globalprod_source_new,".internal.selfref",NULL)  
# SaveData("Fisheries", "fi_global_production_source", globalprod_source_new, waitTimeout = Inf)
write.csv(globalprod_source_new, file = "fi_global_production_source.csv",row.names=FALSE)

message('GP: GP by Sources done')
#########################################################################################################
# calculate Global Production (without source)
#########################################################################################################
aqua_quantity$fisheriesProductionSource <- NULL
cap_quantity$fisheriesProductionSource <- NULL
cap_numbers$fisheriesProductionSource <- NULL

globalprod_new <- rbind (aqua_quantity, cap_quantity) 
globalprod_new <- rbind (globalprod_new, cap_numbers)

message('GP: grouping GP')

globalprod_new <- globalprod_new %>% 
  group_by(geographicAreaM49_fi,fisheriesAsfis,fisheriesCatchArea,measuredElement,timePointYears) %>% 
  summarise(Flag=myAggregate(Value,flagObservationStatus), Quantity=sum(Value))
globalprod_new <- globalprod_new %>% ungroup()

colnames(globalprod_new)[colnames(globalprod_new)=="Quantity"]  <- "Value"
colnames(globalprod_new)[colnames(globalprod_new)=="Flag"]  <- "flagObservationStatus"
globalprod_new$flagMethod <- ""

# for aggregated Values>0,status=N is no longer correct
globalprod_new$flagObservationStatus[which(globalprod_new$Value>0 & globalprod_new$flagObservationStatus=='N')] <- ''

# order columns
globalprod_new <- globalprod_new[,c('geographicAreaM49_fi','fisheriesAsfis','fisheriesCatchArea','measuredElement','timePointYears','Value','flagObservationStatus','flagMethod') ]

message('GP: updating GP')
# remove grouping
globalprod_new <- as.data.table(globalprod_new)

# wipe existing values
# dat <- merge(globalprod[,1:6], globalprod_new[,1:6], by = c('geographicAreaM49_fi','fisheriesAsfis','fisheriesCatchArea','measuredElement','timePointYears'), all = TRUE, suffixes = c("_1", ""))

#-- Charlotte Taglioni -----
dat <- merge(globalprod[, 1:6, with = FALSE], globalprod_new[, 1:6, , with = FALSE], by = c('geographicAreaM49_fi','fisheriesAsfis','fisheriesCatchArea','measuredElement','timePointYears'), all = TRUE, suffixes = c("_1", ""))

#-- Thomas Berger -----
wipe_value <- dat[is.na(dat$Value),]
wipe_value$Value_1  <- NULL
wipe_value$flagObservationStatus <- NA
wipe_value$flagMethod <- NA
wipe_value$flagCurrency <- NA
wipe_value <- as.data.table(wipe_value)

# write NA values back to SWS in order to wipe existing data
if (nrow(wipe_value) > 0) {
  globalprod_new <- rbind (globalprod_new, wipe_value)
}

message('GP: savinging GP')
globalprod_new <- as.data.table(globalprod_new)
SaveData("Fisheries", "fi_global_production", globalprod_new, waitTimeout = Inf)
write.csv(globalprod_new, file = "fi_global_production.csv",row.names=FALSE)


#########################################################################################################
"Global Production calulated OK"