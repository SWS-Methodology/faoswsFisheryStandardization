## The plugin aim is to upload data provided by the FIAS unit
## as legacy data 

suppressMessages({
  library(faosws)
  library(faoswsUtil)
  library(data.table)
  library(openxlsx)
})

message('fi_SUA_FBS_upload: Initializing plugin')

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



countrycode <- as.character(swsContext.computationParams$countrycode)
countryname <- as.character(swsContext.computationParams$countryname)

where <- paste("geographicaream49_fi = '", countrycode, "' ", 
               sep = "")
  
dttot <- ReadDatatable("fbs_fias_tot2019", where = where)
Pop <- dttot[measuredelementsuafbs == '511' ]
Pop[ , measureditemfaostat_l2:= NULL]

namesuafile <- paste(countryname, '_SUA data.xlsx', sep = "")
path <- file.path(R_SWS_SHARE_PATH, "FisheriesFBS", namesuafile)
SUA <- read.xlsx(path)

message('fi_SUA_FBS_upload: reading SUA data')

SUA <- as.data.table(SUA)
names(SUA)

setnames(SUA, c(1, 3, 5),
         c('geographicAreaM49_fi',
           'measuredItemFaostat_L2',
           'measuredElementSuaFbs'))
SUA <- SUA[!is.na(geographicAreaM49_fi)]
SUA <- SUA[ , Unit := NULL]
years <- data.table(timePointYears = unique(as.numeric(names(SUA)[seq(7, ncol(SUA) - 1,by = 2)])),
                    id = 1:length(unique(as.numeric(names(SUA)[seq(7, ncol(SUA) - 1,by = 2)]))))


SUA$measuredElementSuaFbs <- ifelse(SUA$measuredElementSuaFbs == '51', '5510',
                                    ifelse(SUA$measuredElementSuaFbs == '61', '5610',
                                           ifelse(SUA$measuredElementSuaFbs == '62', '5622',
                                                  ifelse(SUA$measuredElementSuaFbs == '63', '5637',
                                                         ifelse(SUA$measuredElementSuaFbs == '91', '5910',
                                                                ifelse(SUA$measuredElementSuaFbs == '92', '5922',
                                                                       ifelse(SUA$measuredElementSuaFbs == '93', '5937',
                                                                              ifelse(SUA$measuredElementSuaFbs == '101', '5520',
                                                                                     ifelse(SUA$measuredElementSuaFbs == '111', '5525',
                                                                                            ifelse(SUA$measuredElementSuaFbs == '121', '5016',
                                                                                                   ifelse(SUA$measuredElementSuaFbs == '131', '5023',
                                                                                                          ifelse(SUA$measuredElementSuaFbs == '141', '5141',
                                                                                                                 ifelse(SUA$measuredElementSuaFbs == '151', '5166',
                                                                                                                        ifelse(SUA$measuredElementSuaFbs == '31', '5302',
                                                                                                                               ifelse(SUA$measuredElementSuaFbs == '41', '5423',
                                                                                                                                      ifelse(SUA$measuredElementSuaFbs == '71', '5071',
                                                                                                                                             ifelse(SUA$measuredElementSuaFbs == '77', '5052',
                                                                                                                                                    SUA$measuredElementSuaFbs)))))))))))))))))


# flag and value with same data

SUAmelt <- melt(SUA, id.vars = c(1:6),
                measure.vars = list(names(SUA)[seq(7, ncol(SUA) - 1,by = 2)], names(SUA)[seq(8, ncol(SUA),by = 2)]),
                variable.name = 'id',
                value.name =  c('Value', 'flagObservationStatus'))

SUAmelt[is.na(flagObservationStatus), flagObservationStatus := '']

SUAmelt[ , c("flagObservationStatus", "flagMethod") := list(ifelse(flagObservationStatus == 'F', 'E', 
                                                               ifelse(flagObservationStatus == 'C', 'I', 
                                                                      ifelse(flagObservationStatus == 'B', 'E', 
                                                                             ifelse(flagObservationStatus == '.', 'E', 
                                                                                    '')))),
                                                        ifelse(flagObservationStatus == 'F', 'f', 
                                                               ifelse(flagObservationStatus == 'C', 'i', 
                                                                      ifelse(flagObservationStatus == 'B', 'b', 
                                                                             ifelse(flagObservationStatus == '.', '-', '-')))))]

SUAwithYears <- merge(SUAmelt, years, by = 'id', all = TRUE) 

SUA <- SUAwithYears[ , c("Country", "Commodity", "Element", "id") := NULL]
SUA <- SUA[!is.na(Value)]

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

SUA2save <- rbind(SUA, SUAPop)
filename <- paste('sua', countryname, sep ="")
# saveRDS(SUA2save, paste('reshaped/',filename, '.rds', sep =""))
# write.csv(SUA2save, paste('reshaped/',filename, '.csv', sep =""), row.names = FALSE)

message('fi_SUA_FBS_upload: SUA data saving')

stats <- SaveData(domain = "FisheriesCommodities",
         dataset = "fi_sua_balanced_control",
         data = SUA2save, waitTimeout = 2000000)

#-- FBS ----

namefbsfile <- paste(countryname, '_FBS data.xlsx', sep = "")

pathfbs <- file.path(R_SWS_SHARE_PATH, "FisheriesFBS", namefbsfile)

fbs <- read.xlsx(pathfbs)
message('fi_SUA_FBS_upload: reading FBS data')

fbs <- as.data.table(fbs)
names(fbs)

fbs1 <- melt(fbs, id.vars = names(fbs)[1:2],
               measure.vars = 3:ncol(fbs),
               variable.name = c('measuredElementSuaFbs'),
               value.name = 'Value')

fbs1 <- fbs1[!is.na(Value)]
fbs1[ , c("flagObservationStatus", "flagMethod") := list('E', 'f')]
unique(fbs1$measuredItemFaostat_L2)
unique(fbs1$measuredElementSuaFbs)
fbs1$measuredElementSuaFbs <- as.character(fbs1$measuredElementSuaFbs)

fbs1$measuredElementSuaFbs <- ifelse(fbs1$measuredElementSuaFbs == "Production", '5510',
                                       ifelse(fbs1$measuredElementSuaFbs == "Meals.input", '5302',
                                              ifelse(fbs1$measuredElementSuaFbs == "Other.non.food.uses", '5153',
                                                     ifelse(fbs1$measuredElementSuaFbs == "Other.non-food.uses", '5153',
                                                     ifelse(fbs1$measuredElementSuaFbs == "Imports", '5610',
                                                            ifelse(fbs1$measuredElementSuaFbs == "Food.Imports", '5610',
                                                            ifelse(fbs1$measuredElementSuaFbs == "Exports", '5910',
                                                                   ifelse(fbs1$measuredElementSuaFbs == "Food.Exports", '5910',
                                                                   ifelse(fbs1$measuredElementSuaFbs == "Stock.variations", '5071',
                                                                          ifelse(fbs1$measuredElementSuaFbs == "Total.food.supply", '5141',
                                                                                 ifelse(fbs1$measuredElementSuaFbs == "Population", '511',
                                                                                        ifelse(fbs1$measuredElementSuaFbs == "Per.capita.food", '5038',
                                                                                               ifelse(fbs1$measuredElementSuaFbs == "Calories", '264',
                                                                                                      ifelse(fbs1$measuredElementSuaFbs == "Proteins", '274',
                                                                                                             ifelse(fbs1$measuredElementSuaFbs == "Fats", '284',
                                                                                                                    fbs1$measuredElementSuaFbs)))))))))))))))





if(any(!fbs1$measuredElementSuaFbs %in% c('5510', '5302', '5153',
                                          '5610', '5910', '5071',
                                          '5141', '511', '5038',
                                          '264', '274', '284'))){
  stop('There is an unrecognised element in the FBS file!')
}

fbs1$measuredItemFaostat_L2 <- ifelse(fbs1$measuredItemFaostat_L2 == "Freshwater and diadromous fish", "10",
                                        ifelse(fbs1$measuredItemFaostat_L2 == "Demersal fish", "20",
                                               ifelse(fbs1$measuredItemFaostat_L2 == "Pelagic fish", "30",
                                                      ifelse(fbs1$measuredItemFaostat_L2 == "Marine fish, other", "40",
                                                             ifelse(fbs1$measuredItemFaostat_L2 == "Crustaceans", "50",
                                                                    ifelse(fbs1$measuredItemFaostat_L2 == "Molluscs, excl. cephalopods", "60",
                                                                           ifelse(fbs1$measuredItemFaostat_L2 == "Cephalopods", "70",
                                                                                  ifelse(fbs1$measuredItemFaostat_L2 == "Aquatic animals, others", "90",
                                                                                         fbs1$measuredItemFaostat_L2))))))))


setnames(fbs1, 2, "timePointYears")
fbs1[ , geographicAreaM49_fi := countrycode]
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

dttot <- ReadDatatable("fbs_fias_tot2019", readOnly = FALSE, where = where)
changeset <- Changeset("fbs_fias_tot2019")
AddDeletions(changeset, dttot[geographicaream49_fi == countrycode])
Finalise(changeset)

AddInsertions(changeset, fbsTot)
Finalise(changeset)
message('fi_SUA_FBS_upload: fbs_fias_tot2019 datatable updated')

FBS <- fbs1[measuredItemFaostat_L2 != 'Totals']


FBS$measuredItemFaostat_L2 <- as.character(FBS$measuredItemFaostat_L2)
FBS$geographicAreaM49_fi <- as.character(FBS$geographicAreaM49_fi)
FBS$timePointYears <- as.character(FBS$timePointYears)
FBS$measuredElementSuaFbs <- as.character(FBS$measuredElementSuaFbs)
FBS$flagObservationStatus <- as.character(FBS$flagObservationStatus)
FBS$flagMethod <- as.character(FBS$flagMethod)
FBS$Value <- as.numeric(FBS$Value)

setcolorder(FBS, c("geographicAreaM49_fi", "measuredElementSuaFbs", "measuredItemFaostat_L2",
                   "timePointYears", "Value", "flagObservationStatus", "flagMethod"))

filenamefbs <- paste('fbs', countryname, sep ="")

# saveRDS(FBS, paste('reshaped/',filenamefbs, '.rds', sep =""))
# write.csv(FBS, paste('reshaped/',filenamefbs, '2.csv', sep =""), row.names = FALSE)



FBS$Value <- as.numeric(FBS$Value)
SaveData(domain = "FisheriesCommodities", 
         dataset = "fi_fbs_fias_control", 
         data = FBS, waitTimeout = 2000000)

message('fi_SUA_FBS_upload: FBS data saved')

paste0("SUA data upload process completed successfully!!! ",
       stats$inserted, " observations written, ",
       stats$ignored, " weren't updated, ",
       stats$discarded, " had problems.")

##-- send Email with notification of correct execution ----

## Initiate email
from = "sws@fao.org"
to = swsContext.userEmail
subject = "fi_SUA_FBS_upload plug-in has correctly run"
body = "The plug-in has saved the SUAs in your session"

sendmailR::sendmail(from = from, to = to, subject = subject, msg = body)

message('fi_SUA_FBS_upload: email sent')