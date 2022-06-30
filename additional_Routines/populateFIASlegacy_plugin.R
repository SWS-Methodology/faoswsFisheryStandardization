suppressMessages({
  library(faosws)
  library(faoswsUtil)
  library(data.table)
  library(xlsx)
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

path <- file.path(R_SWS_SHARE_PATH, "FisheriesFBS")

dttot <- ReadDatatable("fbs_fias_tot2019")

files <- list.files('SUA-Aug-2019')

####
files <- files[-c(169, 170)]
####
country <- unique(gsub('_.*','', files))

####
filesnew <- list.files('C:/Users/Cha/OneDrive - Food and Agriculture Organization/Documents_fisheries/SUAs-FBS_AB')
countrynew <- unique(gsub('[.].*','', filesnew))
countrynew <- unique(gsub('Std','',countrynew))

countrynew[countrynew %in% country]
countrynew[!countrynew %in% country]

country <- country[!country %in% countrynew]
####
geocode <- ReadDatatable('importgeocodefias')
countrycode <- geocode[filecountryname %in% country ]$m49code

if(any(!country %in% geocode$filecountryname)){
  stop(paste('Missing country:', paste(country[!country %in% geocode$filecountryname], 
             collapse = ', ')))
}


Ncountry <-length(country)

# Datatable to convert elements
element <- ReadDatatable('oldfias2measuredelement')
flags <- ReadDatatable('oldfias2flags')

fbselement <- ReadDatatable('oldfias2measuredelementfbs')
fbsgroup <- ReadDatatable('oldfias2measureditemfbs')

SUA2sws <- data.table()
FBS2sws <- data.table()

for(i in 35:Ncountry){
  #-- FBS ----
  print(country[i])
  fbs <- xlsx::read.xlsx(paste('SUA-Aug-2019/', country[i], '_FBS data.xlsx', sep =''), sheetIndex = 1)
  fbs <- as.data.table(fbs)
  setnames(fbs, names(fbs), gsub('[.]', ' ', names(fbs)))
  
  fbs1 <- melt(fbs, id.vars = names(fbs)[1:2],
               measure.vars = 3:ncol(fbs),
               variable.name = c('measuredElementSuaFbs'),
               value.name = 'Value')
  
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
  
  fbs1[ , geographicAreaM49_fi := countrycode[i]]
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
  
  dttot_c <- ReadDatatable("fbs_fias_tot2019", readOnly = FALSE)
  changeset <- Changeset("fbs_fias_tot2019")
  AddDeletions(changeset, dttot_c[geographicaream49_fi == countrycode[i]])
  Finalise(changeset)
  
  AddInsertions(changeset, fbsTot)
  Finalise(changeset)
  
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
  
  
  #-- SUA ----
  sua0 <- xlsx::read.xlsx(paste('SUA-Aug-2019/', country[i], '_SUA data.xlsx', sep = ''), 
                          sheetIndex = 1, header = TRUE)
  sua0 <- as.data.table(sua0)
  setnames(sua0, names(sua0)[1:7], as.character(sua0[1, 1:7, with = F]))
  setnames(sua0, names(sua0)[c(1,3,5)],
           c("geographicAreaM49_fi", "measuredItemFaostat_L2", "measuredElementSuaFbs"))
  setnames(sua0, names(sua0)[c(2,4,6,7)], as.character(sua0[1, c(2,4,6,7), with = F]))
  setnames(sua0,  names(sua0)[seq(8,ncol(sua0)-1, by=2)], as.character(sua0[1, seq(8,(ncol(sua0)-1), by=2), with = F]))
  setnames(sua0,  
           names(sua0)[seq(9,ncol(sua0), by=2)], 
           paste('Y', as.character(sua0[1, seq(8,(ncol(sua0)-1), by=2), with = F]), sep = ''))
  #sua0 <- sua0[-1,]
  sua1 <- melt(sua0, id.vars = colnames(sua0)[c(1:6)],
                 measure.vars = c(seq(8,ncol(sua0)-1, by=2)),
                 variable.name = c('timePointYears'),
                 value.name = 'Value')
  sua1 <- as.data.table(sua1)
  sua1 <- sua1[!is.na(Country) ]

  sua2 <- melt(sua0, id.vars = colnames(sua0)[c(1:6)],
                 measure.vars = c(seq(9,ncol(sua0), by=2)),
                 variable.name = c('timePointYears'),
                 value.name = 'Valueflag')
  
  sua2$timePointYears <- gsub('Y', '', sua2$timePointYears)
  sua2 <- as.data.table(sua2)
  sua2 <- sua2[!is.na(Country) ]
  SUA1 <- merge(sua1, sua2, by = colnames(sua1)[c(1:7)], all.x = TRUE)
  SUA1$measuredElementSuaFbs <- as.character(SUA1$measuredElementSuaFbs)

  SUA <- merge(SUA1, element[, label := NULL], 
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
  SUA <- merge(SUA, flags[, label := NULL], by.x = 'Valueflag', by.y = 'oldflag', all.x = TRUE)
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


SUA2sws[measuredElementSuaFbs != '511', c("flagObservationStatus", "flagMethod") := list('I', 's')]
FBS2sws[measuredElementSuaFbs != '511', c("flagObservationStatus", "flagMethod") := list('I', 's')]
SUA2sws[measuredElementSuaFbs == '511', c("flagObservationStatus", "flagMethod") := list('X', 'h')]
FBS2sws[measuredElementSuaFbs == '511', c("flagObservationStatus", "flagMethod") := list('X', 'h')]


if(CheckDebug()){
  library(faoswsModules)
  SETTINGS = ReadSettings("sws.yml")
  ## If you're not on the system, your settings will overwrite any others
  R_SWS_SHARE_PATH = SETTINGS[["share"]]
  ## Define where your certificates are stored
  SetClientFiles(SETTINGS[["certdir"]])
  ## Get session information from SWS. Token must be obtained from web interface
  GetTestEnvironment(baseUrl = SETTINGS[["server"]],
                     token = '5407bd9d-f273-40a1-9b9d-94834f5e78b1') # sua token
}


sessionKey_sua = swsContext.datasets[[1]]
CONFIG1 <- GetDatasetConfig(sessionKey_sua@domain, sessionKey_sua@dataset)


stats1 <-SaveData(domain = CONFIG1$domain,
         dataset = CONFIG1$dataset,
         data = SUA2sws, waitTimeout = 2000000)

paste0("SUA data upload process completed successfully! ",
       stats1$inserted, " observations written, ",
       stats1$ignored, " weren't updated, ",
       stats1$discarded, " had problems.")

if(CheckDebug()){
  library(faoswsModules)
  SETTINGS = ReadSettings("sws.yml")
  ## If you're not on the system, your settings will overwrite any others
  R_SWS_SHARE_PATH = SETTINGS[["share"]]
  ## Define where your certificates are stored
  SetClientFiles(SETTINGS[["certdir"]])
  ## Get session information from SWS. Token must be obtained from web interface
  GetTestEnvironment(baseUrl = SETTINGS[["server"]],
                     token = '3440d567-58f2-46cd-a554-55db2956b588') # fbs legacy token
}

sessionKey_fbs = swsContext.datasets[[2]]
CONFIG2 <- GetDatasetConfig(sessionKey_fbs@domain, sessionKey_fbs@dataset)

stats2 <- SaveData(domain = CONFIG2$domain, 
         dataset = CONFIG2$dataset, 
         data = FBS2sws, waitTimeout = 2000000)


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
