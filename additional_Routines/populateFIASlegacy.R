suppressMessages({
  library(faosws)
  library(faoswsUtil)
  library(data.table)
  #library(xlsx)
  library(readxl)
})

message('fi_SUA_FBS_upload: Initializing plugin')

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
                     token = SETTINGS[["token"]])
  
  # Cerca .Rprofile
  R_SWS_SHARE_PATH <- "R:/"
} else {
  R_SWS_SHARE_PATH <- Sys.getenv("R_SWS_SHARE_PATH")
}

#setwd('C:/Users/Charlotte/OneDrive - Food and Agriculture Organization/Github/faoswsFisheryStandardization/DataLegacy/Legacy_16_IX_2020/FBS/')

# path <- file.path(R_SWS_SHARE_PATH, "FisheriesFBS")

dttot <- ReadDatatable("fbs_fias_tot2019")

files <- iconv(list.files('DataLegacy/Legacy_16_IX_2020/SUA'),
               to='UTF-8')

####
# files <- files[-c(169, 170)]
####

country <- unique(gsub('.xlsx','', files))

files <- iconv(list.files('DataLegacy/Legacy_16_IX_2020/FBS'),
               to='UTF-8')
country <- unique(gsub('Std.xlsx','', files))

# filesnew <- list.files('FBSMarch2020')
# countrynew <- unique(gsub('[.].*','', filesnew))
# countrynew <- unique(gsub('Std','',countrynew))
# countrynew <- countrynew[- c(5,13,25,28) ]
# country <- countrynew
# countrynew[countrynew %in% country]
# countrynew[!countrynew %in% country]
# 
# country <- country[!country %in% countrynew]
####
geocode <- ReadDatatable('importgeocodefias')
countrycode <- geocode[filecountryname %in% country ]
geocode[!filecountryname %in% country ]


if(any(!country %in% geocode$filecountryname)){
  stop(paste('Missing country:', paste(country[!country %in% geocode$filecountryname], 
                                       collapse = ', ')))
}


Ncountry <-nrow(countrycode)

# Datatable to convert elements
element <- ReadDatatable('oldfias2measuredelement')
element <- element[, label := NULL]
flags <- ReadDatatable('oldfias2flags')
flags <- flags[, label := NULL]
fbselement <- ReadDatatable('oldfias2measuredelementfbs')
fbsgroup <- ReadDatatable('oldfias2measureditemfbs')

SUA2sws <- data.table()
FBS2sws <- data.table()
FBStot <- data.table()

for(i in 1:Ncountry){
  #-- FBS ----
  print(paste(countrycode[i,]$filecountryname, i, '/', Ncountry))
  #setwd('C:/Users/Charlotte/OneDrive - Food and Agriculture Organization/Github/faoswsFisheryStandardization/DataLegacy/FBSlegacyConv')
  filename <- paste('DataLegacy/Legacy_16_IX_2020/FBS/' ,countrycode[i,]$filecountryname , 'Std.xlsx', sep = '')
  # filename <-stringr::str_c(countrycode[i,]$filecountryname , '_FBS.xlsx', sep = '')
  # undebug(readxl:::read_xlsx_)
  # 
  # library(openxlsx)
  # fbs <- openxlsx::read.xlsx(xlsxFile = files[3], sheet = 1, startRow = 10)
  fbs <- readxl::read_xlsx(filename)
  # fbs <- xlsx::read.xlsx(paste(countrycode[i,]$filecountryname , '_FBS.xlsx', sep = ''), 
  #                 sheetIndex = 1, header = TRUE)
  fbs <- as.data.table(fbs)
  
  setnames(fbs, names(fbs), gsub('[.]', ' ', names(fbs)))
  fbs$timePointYears <- gsub('[.]0', '', fbs$timePointYears)
  fbs1 <- melt(fbs, id.vars = names(fbs)[1:2],
                 measure.vars = 3:ncol(fbs),
                 variable.name = c('measuredElementSuaFbs'),
                 value.name = 'Value',
               variable.factor = FALSE)
  
  fbs1 <- fbs1[!is.na(Value)]
  fbs1[ , c("flagObservationStatus", "flagMethod") := list('I', 's')]
  fbs1$measuredElementSuaFbs <- as.character(fbs1$measuredElementSuaFbs)
  
  fbs1 <- merge(fbs1, fbselement, by.x = 'measuredElementSuaFbs', by.y = 'oldlabel', all.x = TRUE)
  
  if(any(!unique(fbs1[is.na(currentcode)]$measuredElementSuaFbs) %in% fbselement$oldlabel)){
    elnotfound <- fbs1[is.na(currentcode)]$measuredElementSuaFbs
    stop(paste('Unrecognised element!Element:', elnotfound))
  }
  
  fbs1[ , measuredElementSuaFbs:= NULL]
  setnames(fbs1, 'currentcode', 'measuredElementSuaFbs')
  
  fbs1 <- merge(fbs1, fbsgroup, 
                by.x = 'measuredItemFaostat_L2', by.y = 'oldlabel',
                all.x = TRUE)
  
  fbs1[measuredItemFaostat_L2 == 'Totals', currentcode :='Totals']
  if(any(!unique(fbs1[is.na(currentcode)]$measuredItemFaostat_L2) %in% fbsgroup$oldlabel)){
    elnotfound <- fbs1[is.na(currentcode)]$measuredItemFaostat_L2
    stop(paste('Unrecognised element!Element:', elnotfound))
  }
  
  fbs1[ , measuredItemFaostat_L2:= NULL]
  setnames(fbs1, 'currentcode', 'measuredItemFaostat_L2')
  
  fbs1[ , geographicAreaM49_fi := countrycode[i,]$m49code]
  fbsTot <- fbs1[measuredItemFaostat_L2 == 'Totals']
  setnames(fbsTot, c("measuredItemFaostat_L2", 
                     "timePointYears",
                     "measuredElementSuaFbs",
                     "Value",
                     "flagObservationStatus", 
                     "flagMethod",
                     "geographicAreaM49_fi"), c("measureditemfaostat_l2", 
                                                "timepointyears",
                                                "measuredelementsuafbs",
                                                "value",
                                                "flagobservationstatus", 
                                                "flagmethod",
                                                "geographicaream49_fi"))
  setcolorder(fbsTot, c("geographicaream49_fi",
                        "measureditemfaostat_l2", 
                        "timepointyears",
                        "measuredelementsuafbs",
                        "value",
                        "flagobservationstatus", 
                        "flagmethod"))
  
  FBStot <- rbind(FBStot, fbsTot)
  
  Pop <- fbsTot[measuredelementsuafbs == '511' ]
  Pop[ , measureditemfaostat_l2:= NULL]
  
  FBS <- fbs1[measuredItemFaostat_L2 != 'Totals']
  
  FBS <- as.data.table(FBS)
  FBS$measuredItemFaostat_L2 <- as.character(FBS$measuredItemFaostat_L2)
  FBS$geographicAreaM49_fi <- as.character(FBS$geographicAreaM49_fi)
  FBS$timePointYears <- as.character(FBS$timePointYears)
  FBS$measuredElementSuaFbs <- as.character(FBS$measuredElementSuaFbs)
  FBS$flagObservationStatus <- as.character(FBS$flagObservationStatus)
  FBS$flagMethod <- as.character(FBS$flagMethod)
  FBS$Value <- as.numeric(FBS$Value)
  
  setcolorder(FBS, c("geographicAreaM49_fi", "measuredElementSuaFbs", "measuredItemFaostat_L2",
                     "timePointYears", "Value", "flagObservationStatus", "flagMethod"))
  
  FBS2sws <- rbind(FBS2sws, FBS)
}
  
  setwd('C:/Users/Charlotte/OneDrive - Food and Agriculture Organization/Github/faoswsFisheryStandardization/DataLegacy/SUAlegacy')
  
  # #-- SUA ----

  for(i in 1:Ncountry){
    #-- FBS ----
    print(paste(countrycode[i,]$filecountryname, i, '/', Ncountry))
    #setwd('C:/Users/Charlotte/OneDrive - Food and Agriculture Organization/Github/faoswsFisheryStandardization/DataLegacy/FBSlegacyConv')
    
    sua0 <- readxl::read_xlsx(paste('DataLegacy/Legacy_16_IX_2020/SUA/' ,countrycode[i,]$filecountryname, '.xlsx', sep = ''))
    # sua0 <- xlsx::read.xlsx(paste('FBSMarch2020/', countrycode[i,]$filecountryname, '.xlsx', sep = ''), 
    #                 sheetIndex = 1, header = TRUE)
    # 
    sua0 <- as.data.table(sua0)
    
    setnames(sua0, names(sua0)[seq(9,121, by=2)], names(sua0)[seq(8,120, by=2)])
    
    sua1 <- melt(sua0, id.vars = colnames(sua0)[c(1:6)],
                 measure.vars = c(seq(8,120, by=2)),
                 variable.name = c('timePointYears'),
                 value.name = 'Value'
    )
    sua1 <- as.data.table(sua1)
    sua1 <- sua1[!is.na(Country)]
    sua1$timePointYears <- gsub('X', '', sua1$timePointYears)
    sua1$timePointYears <- as.character(as.integer(sua1$timePointYears))
    
    sua2 <- melt(sua0, id.vars = colnames(sua0)[c(1:6)],
                 measure.vars = c(seq(9,121, by=2)),
                 variable.name = c('timePointYears'),
                 value.name = 'Valueflag'
    )
    
    # put years instead of ..9 -> 1961 ecc..
    sua2 <- as.data.table(sua2)
    sua2 <- sua2[!is.na(Country)]
    sua2$timePointYears <- gsub('X', '', sua2$timePointYears)
    sua2$timePointYears <- as.character(as.integer(sua2$timePointYears))
    SUA1 <- merge(sua1, sua2, by = colnames(sua1)[c(1:7)], all.x = TRUE)
    setnames(SUA1, names(SUA1[, c(1,3,5), with = FALSE]), c("geographicAreaM49_fi", "measuredItemFaostat_L2", "measuredElementSuaFbs"))
    SUA1$measuredElementSuaFbs <- as.character(SUA1$measuredElementSuaFbs)
    
    SUA <- merge(SUA1, element,
                 by.x = c('measuredElementSuaFbs'), by.y = c('oldcode'),
                 all = TRUE)
    SUA$Valueflag <- as.character(SUA$Valueflag)
    
    if(any(!unique(SUA[is.na(currentcode)]$measuredElementSuaFbs) %in% c("66", "77", "96"))){
      elnotfound <- SUA[is.na(currentcode)]$measuredElementSuaFbs[which(any(!unique(SUA[is.na(currentcode)]$measuredElementSuaFbs) %in% c("77", "96")))]
      stop(paste('Unrecognised element!Element:', elnotfound))
    }
    
    SUA <- SUA[!is.na(currentcode),]
    SUA[, measuredElementSuaFbs := NULL]
    setnames(SUA, 'currentcode', 'measuredElementSuaFbs')
    
    # Conversion:
    # NA = ( , -)
    # F = (E, f)
    # C = (I, i)
    # B = (E, b)
    # . = (E, -) ask how to change
    SUA[is.na(Valueflag), Valueflag := '']
    SUA <- merge(SUA, flags, by.x = 'Valueflag', by.y = 'oldflag', all.x = TRUE)
    setnames(SUA, c('flagobservationstatus', 'flagmethod'), c('flagObservationStatus', 'flagMethod'))
    SUA[ , c("Country", "Commodity", "Element", "Valueflag") := NULL]
    SUA <- SUA[!is.na(Value)]
    SUA <- as.data.table(SUA)
    SUA$geographicAreaM49_fi <- as.character(SUA$geographicAreaM49_fi)
    SUA$measuredItemFaostat_L2 <- as.character(SUA$measuredItemFaostat_L2)
    SUA$measuredElementSuaFbs <- as.character(SUA$measuredElementSuaFbs)
    SUA$timePointYears <- as.character(SUA$timePointYears)
    SUA$Value <- as.numeric(SUA$Value)
    SUA$flagObservationStatus <- as.character(SUA$flagObservationStatus)
    SUA$flagMethod <- as.character(SUA$flagMethod)
    
    SUA2merge <- SUA[ , .(geographicAreaM49_fi,
                          measuredItemFaostat_L2,
                          timePointYears)]
    setkey(SUA2merge)
    SUA2merge <- unique(SUA2merge)
    
    SUAPop <- merge(SUA2merge, Pop, by.x = c('geographicAreaM49_fi', 'timePointYears'),
                    by.y = c('geographicaream49_fi', 'timepointyears'))
    
    setnames(SUAPop, c('measuredelementsuafbs', 'value', 'flagobservationstatus', 'flagmethod'),
             c('measuredElementSuaFbs', 'Value', 'flagObservationStatus', 'flagMethod'))
    
    SUAtot <- rbind(SUA, SUAPop)
    SUA2sws <- rbind(SUA2sws, SUAtot)
    
  }
  
setwd('C:/Users/Charlotte/OneDrive - Food and Agriculture Organization/Github/faoswsFisheryStandardization/DataLegacy')

FBS2sws$timePointYears <- as.character(as.integer(FBS2sws$timePointYears))
FBStot$timepointyears <- as.character(as.integer(FBStot$timepointyears))

FBS2sws[measuredElementSuaFbs != '511', c("flagObservationStatus", "flagMethod") := list('I', 's')]
SUA2sws[measuredElementSuaFbs == '511', c("flagObservationStatus", "flagMethod") := list('X', 'h')]
FBS2sws[measuredElementSuaFbs == '511', c("flagObservationStatus", "flagMethod") := list('X', 'h')]
FBStot[measuredelementsuafbs != '511', c("flagobservationstatus", "flagmethod") := list('I', 's')]
FBStot[measuredelementsuafbs == '511', c("flagobservationstatus", "flagmethod") := list('X', 'h')]


saveRDS(FBS2sws, 'FBS13Oct20.rds')
saveRDS(SUA2sws, 'SUA13Oct20.rds')
saveRDS(FBStot, 'FBStot13Oct20.rds')

changeset <- Changeset("fbs_fias_tot2019")
AddInsertions(changeset, FBStot)
Finalise(changeset)

SUA2sws <- readRDS('SUAnew2.rds')
FBS2sws <-readRDS('FBSnew2.rds')

setwd('C:/Users/Charlotte/OneDrive - Food and Agriculture Organization/Github/faoswsFisheryStandardization')
if(CheckDebug()){
  library(faoswsModules)
  SETTINGS = ReadSettings("sws.yml")
  ## If you're not on the system, your settings will overwrite any others
  R_SWS_SHARE_PATH = SETTINGS[["share"]]
  ## Define where your certificates are stored
  SetClientFiles(SETTINGS[["certdir"]])
  ## Get session information from SWS. Token must be obtained from web interface
  GetTestEnvironment(baseUrl = SETTINGS[["server"]],
                     token = 'a236346b-8c86-49a9-ae81-4fcae7dba39a') # sua token
}

# countries to delete

countries2del <- unique(SUA2sws$geographicAreaM49_fi)

suakeys <- DatasetKey(domain = "Fisheries Commodities", dataset = "fi_sua_balanced_legacy", dimensions = list(
  geographicAreaM49_fi = Dimension(name = "geographicAreaM49_fi", keys = countries2del),
  measuredItemISSCFC = Dimension(name = "measuredItemFaostat_L2", keys = GetCodeList("FisheriesCommodities", "fi_sua_balanced_legacy","measuredItemFaostat_L2" )[,code]),
  measuredElement = Dimension(name = "measuredElementSuaFbs", keys = GetCodeList("FisheriesCommodities", "fi_sua_balanced_legacy","measuredElementSuaFbs" )[,code]),
  timePointYears = Dimension(name = "timePointYears", keys = GetCodeList("FisheriesCommodities", "fi_sua_balanced_legacy","timePointYears" )[,code] )))

sua2del <- GetData(suakeys)

sua2del[ , c('Value', 'flagObservationStatus', 'flagMethod') := list(NA, NA, NA)]

sessionKey_sua = swsContext.datasets[[1]]
CONFIG1 <- GetDatasetConfig(sessionKey_sua@domain, sessionKey_sua@dataset)


stats1 <-SaveData(domain = CONFIG1$domain,
                  dataset = CONFIG1$dataset,
                  data = sua2del, waitTimeout = Inf)

paste0("SUA data upload process completed successfully! ",
       stats1$inserted, " observations written, ",
       stats1$ignored, " weren't updated, ",
       stats1$discarded, " had problems.")


SUA2sws$Value <- as.numeric(SUA2sws$Value)
SUA2sws$timePointYears <- as.character(SUA2sws$timePointYears)

sessionKey_sua = swsContext.datasets[[1]]
CONFIG1 <- GetDatasetConfig(sessionKey_sua@domain, sessionKey_sua@dataset)


stats1 <-SaveData(domain = CONFIG1$domain,
                  dataset = CONFIG1$dataset,
                  data = SUA2sws, waitTimeout = Inf)

paste0("SUA data upload process completed successfully! ",
       stats1$inserted, " observations written, ",
       stats1$ignored, " weren't updated, ",
       stats1$discarded, " had problems.")

FBS2sws$Value <- as.numeric(FBS2sws$Value)
FBS2sws$timePointYears <- as.character(FBS2sws$timePointYears)



if(CheckDebug()){
  library(faoswsModules)
  SETTINGS = ReadSettings("sws.yml")
  ## If you're not on the system, your settings will overwrite any others
  R_SWS_SHARE_PATH = SETTINGS[["share"]]
  ## Define where your certificates are stored
  SetClientFiles(SETTINGS[["certdir"]])
  ## Get session information from SWS. Token must be obtained from web interface
  GetTestEnvironment(baseUrl = SETTINGS[["server"]],
                     token = 'ce3b26e0-3f92-4344-a817-242a61c31104') # fbs legacy token
}

sessionKey_fbs = swsContext.datasets[[1]]
CONFIG2 <- GetDatasetConfig(sessionKey_fbs@domain, sessionKey_fbs@dataset)

stats2 <- SaveData(domain = CONFIG2$domain, 
                   dataset = CONFIG2$dataset, 
                   data = FBS2sws, waitTimeout = Inf)


paste0("FBS data upload process completed successfully! ",
       stats2$inserted, " observations written, ",
       stats2$ignored, " weren't updated, ",
       stats2$discarded, " had problems.")


##-- send Email with notification of correct execution ----

## Initiate email
from = "sws@fao.org"
to = swsContext.userEmail
subject = "fi_SUA_FBS_upload plug-in has correctly run"
body = "The plug-in has saved the data in your sessions"

sendmailR::sendmail(from = from, to = to, subject = subject, msg = body)

message('fi_SUA_FBS_upload: email sent')
