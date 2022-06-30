library(xlsx)

suppressMessages({
  library(faosws)
  library(faoswsUtil)
  library(data.table)
})

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
  
}


dttot <- ReadDatatable("fbs_fias_tot2019")
Pop <- dttot[measuredelementsuafbs == '511' ]
Pop[ , measureditemfaostat_l2:= NULL]

country <- c('Australia', 'Cambodia', 'Ireland', 'Pakistan', 'Spain', 'USA', 'Venezuela', 'Zambia', 'France', 'Iran', 'Peru')
countrycode <- c('36', '116', '372', '586', '724', '840', '862', '894', '250', '364', '604')
Ncountry <-length(country)

SUA2sws <- data.table()
FBS2sws <- data.table()

for(i in 1:Ncountry){
  
suaFr <- readxl::read_xlsx(paste('SUA-Aug-2019/', country[i], '_SUA data.xlsx', sep = ''))

suaFr <- as.data.table(suaFr)

setnames(suaFr, names(suaFr)[seq(9,121, by=2)], names(suaFr)[seq(8,120, by=2)])

suaFr1 <- melt(suaFr, id.vars = colnames(suaFr)[c(1:6)],
     measure.vars = c(seq(8,120, by=2)),
     variable.name = c('timePointYears'),
     value.name = 'Value'
    )
suaFr1 <- as.data.table(suaFr1)
suaFr1 <- suaFr1[!is.na(Country) ]

suaFr2 <- melt(suaFr, id.vars = colnames(suaFr)[c(1:6)],
               measure.vars = c(seq(9,121, by=2)),
               variable.name = c('timePointYears'),
               value.name = 'Valueflag'
               )

# put years instead of ..9 -> 1961 ecc..
suaFr2 <- as.data.table(suaFr2)
suaFr2 <- suaFr2[!is.na(Country) ]
SUA1 <- merge(suaFr1, suaFr2, by = colnames(suaFr1)[c(1:7)], all.x = TRUE)
setnames(SUA1, c("a", "..3", "..5"), c("geographicAreaM49_fi", "measuredItemFaostat_L2", "measuredElementSuaFbs"))
SUA1$measuredElementSuaFbs <- as.character(SUA1$measuredElementSuaFbs)
SUA <- SUA1[!measuredElementSuaFbs %in% c('96', '181')]

SUA96 <- SUA1[measuredElementSuaFbs == '96']
SUA181 <- SUA1[measuredElementSuaFbs == '181']
all(is.na(SUA96$Value))
all(is.na(SUA181$Value))

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



SUA$Valueflag <- as.character(SUA$Valueflag)
unique(SUA$Valueflag)

# Conversion:
# NA = ( , -)
# F = (E, f)
# C = (I, i)
# B = (E, b)
# . = (E, -) ask how to change

SUA[ , c("flagObservationStatus", "flagMethod") := list(ifelse(Valueflag == 'F', 'E', 
                                                               ifelse(Valueflag == 'C', 'I', 
                                                                      ifelse(Valueflag == 'B', 'E', 
                                                                             ifelse(Valueflag == '.', 'E', 
                                                                                    ifelse(is.na(Valueflag), '',
                                                                                           ifelse(Valueflag == 'T', 'P',Valueflag)))))),
                                                        ifelse(Valueflag == 'F', 'f', 
                                                               ifelse(Valueflag == 'C', 'i', 
                                                                      ifelse(Valueflag == 'B', 'b', 
                                                                             ifelse(Valueflag == '.', '-', 
                                                                                    ifelse(Valueflag == 'T', '-', 
                                                                                    ifelse(is.na(Valueflag), '-', Valueflag)))))))]

SUA[is.na(flagObservationStatus)]$flagObservationStatus <- ''
SUA[is.na(flagMethod)]$flagMethod <- '-'
SUA[ , c("Country", "Commodity", "Element", "Valueflag") := NULL]
SUA <- SUA[!is.na(Value)]

SUA <- as.data.table(SUA)
SUA$geographicAreaM49_fi <- as.character(SUA$geographicAreaM49_fi)
SUA$measuredItemFaostat_L2 <- as.character(SUA$measuredItemFaostat_L2)
SUA$measuredElementSuaFbs <- as.character(SUA$measuredElementSuaFbs)
SUA$timePointYears <- as.character(SUA$timePointYears)
SUA$timePointYears <- as.numeric(SUA$timePointYears)
SUA$timePointYears <- round(SUA$timePointYears)
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

SUAFr <- rbind(SUA, SUAPop)
#saveRDS(SUAFr, paste('SUA-Aug-2019/sua', country[i], '.rds', sep = ''))

SUA2sws <- rbind(SUA2sws, SUAFr)

#suaFr2save <- readRDS(paste('SUA-Aug-2019/sua', country[i], '.rds', sep = ''))
#write.csv(suaFr2save, "SUA-Aug-2019/suaFr.csv", row.names = FALSE)

#-- FBS ----

fbsFr <- readxl::read_xlsx(paste('SUA-Aug-2019/', country[i], '_FBS data.xlsx', sep =''))

fbsFr <- as.data.table(fbsFr)
names(fbsFr)

fbsFr1 <- melt(fbsFr, id.vars = names(fbsFr)[1:2],
               measure.vars = 3:ncol(fbsFr),
               variable.name = c('measuredElementSuaFbs'),
               value.name = 'Value')

fbsFr1 <- fbsFr1[!is.na(Value)]
fbsFr1[ , c("flagObservationStatus", "flagMethod") := list('E', 'f')]
unique(fbsFr1$measuredItemFaostat_L2)
unique(fbsFr1$measuredElementSuaFbs)
fbsFr1$measuredElementSuaFbs <- as.character(fbsFr1$measuredElementSuaFbs)

fbsFr1$measuredElementSuaFbs <- ifelse(fbsFr1$measuredElementSuaFbs == "Production", '5510',
                                       ifelse(fbsFr1$measuredElementSuaFbs == "Meals input", '5302',
                                              ifelse(fbsFr1$measuredElementSuaFbs == "Other non-food uses", '5153',
                                                     ifelse(fbsFr1$measuredElementSuaFbs == "Imports", '5610',
                                                            ifelse(fbsFr1$measuredElementSuaFbs == "Food Imports", '5610',
                                                            ifelse(fbsFr1$measuredElementSuaFbs == "Exports", '5910',
                                                                   ifelse(fbsFr1$measuredElementSuaFbs == "Food Exports", '5910',
                                                                   ifelse(fbsFr1$measuredElementSuaFbs == "Stock variations", '5071',
                                                                          ifelse(fbsFr1$measuredElementSuaFbs == "Total food supply", '5141',
                                                                                 ifelse(fbsFr1$measuredElementSuaFbs == "Population", '511',
                                                                                        ifelse(fbsFr1$measuredElementSuaFbs == "Per capita food", '5038',
                                                                                               ifelse(fbsFr1$measuredElementSuaFbs == "Calories", '264',
                                                                                                      ifelse(fbsFr1$measuredElementSuaFbs == "Proteins", '274',
                                                                                                             ifelse(fbsFr1$measuredElementSuaFbs == "Fats", '284',
                                                                                                                    fbsFr1$measuredElementSuaFbs))))))))))))))






fbsFr1$measuredItemFaostat_L2 <- ifelse(fbsFr1$measuredItemFaostat_L2 == "Freshwater and diadromous fish", "10",
                                        ifelse(fbsFr1$measuredItemFaostat_L2 == "Demersal fish", "20",
                                               ifelse(fbsFr1$measuredItemFaostat_L2 == "Pelagic fish", "30",
                                                      ifelse(fbsFr1$measuredItemFaostat_L2 == "Marine fish, other", "40",
                                                             ifelse(fbsFr1$measuredItemFaostat_L2 == "Crustaceans", "50",
                                                                    ifelse(fbsFr1$measuredItemFaostat_L2 == "Molluscs, excl. cephalopods", "60",
                                                                           ifelse(fbsFr1$measuredItemFaostat_L2 == "Cephalopods", "70",
                                                                                  ifelse(fbsFr1$measuredItemFaostat_L2 == "Aquatic animals, others", "90",
                                                                                         fbsFr1$measuredItemFaostat_L2))))))))


setnames(fbsFr1, 2, "timePointYears")
fbsFr1[ , geographicAreaM49_fi := countrycode[i]]
fbsFrTot <- fbsFr1[measuredItemFaostat_L2 == 'Totals']
setnames(fbsFrTot, c("measuredItemFaostat_L2", 
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
setcolorder(fbsFrTot, c("geographicaream49_fi",
                        "measureditemfaostat_l2", 
                        "timepointyears",
                        "measuredelementsuafbs",
                        "value",
                        "flagobservationstatus", 
                        "flagmethod"))

dttot <- ReadDatatable("fbs_fias_tot2019", readOnly = FALSE)
changeset <- Changeset("fbs_fias_tot2019")
AddDeletions(changeset, dttot[geographicaream49_fi == countrycode[i]])
Finalise(changeset)

AddInsertions(changeset, fbsFrTot)
Finalise(changeset)

FBS <- fbsFr1[measuredItemFaostat_L2 != 'Totals']

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
#saveRDS(FBS, 'SUA-Aug-2019/fbsFr.rds')
#fbsFr2save <- readRDS('SUA-Aug-2019/fbsFr.rds')
#is.data.table(fbsFr2save)
#apply(fbsFr2save, 2,class)
#fbsFr2save$Value <- as.numeric(fbsFr2save$Value)

}


SUA2sws[measuredElementSuaFbs != '511', c("flagObservationStatus", "flagMethod") := list('I', 's')]
FBS2sws[measuredElementSuaFbs != '511', c("flagObservationStatus", "flagMethod") := list('I', 's')]
SUA2sws[measuredElementSuaFbs == '511', c("flagObservationStatus", "flagMethod") := list('X', 'h')]
FBS2sws[measuredElementSuaFbs == '511', c("flagObservationStatus", "flagMethod") := list('X', 'h')]

#-- SUA Token QA ----
if(CheckDebug()){
  
  library(faoswsModules)
  SETTINGS = ReadSettings("sws.yml")
  
  ## If you're not on the system, your settings will overwrite any others
  R_SWS_SHARE_PATH = SETTINGS[["share"]]
  
  ## Define where your certificates are stored
  SetClientFiles(SETTINGS[["certdir"]])
  
  ## Get session information from SWS. Token must be obtained from web interface
  GetTestEnvironment(baseUrl = SETTINGS[["server"]],
                     token = '01016c25-53ce-4385-9be0-aea7f5dfd35d')
  
}


SaveData(domain = "FisheriesCommodities",
         dataset = "fi_sua_balanced_control",
         data = SUA2sws, waitTimeout = 2000000)

# class(SUA2sws$flagMethod)
unique(SUA$measuredElementSuaFbs)
#-- FBS Token QA ----
if(CheckDebug()){
  
  library(faoswsModules)
  SETTINGS = ReadSettings("sws.yml")
  
  ## If you're not on the system, your settings will overwrite any others
  R_SWS_SHARE_PATH = SETTINGS[["share"]]
  
  ## Define where your certificates are stored
  SetClientFiles(SETTINGS[["certdir"]])
  
  ## Get session information from SWS. Token must be obtained from web interface
  GetTestEnvironment(baseUrl = SETTINGS[["server"]],
                     token = '9d9da577-e201-4661-b146-14e542eef304')
  
}

SaveData(domain = "FisheriesCommodities", 
         dataset = "fi_fbs_fias_control", 
         data = FBS2sws, waitTimeout = 2000000)

# ##-- Perù ----
# 
# suaPr <- readxl::read_xlsx('SUA-Aug-2019/Peru_SUA data.xlsx', sheet = 1)
# 
# suaPr <- as.data.table(suaPr)
# 
# setnames(suaPr, names(suaPr)[seq(9,121, by=2)], names(suaPr)[seq(8,120, by=2)])
# 
# suaPr1 <- melt(suaPr, id.vars = colnames(suaPr)[c(1:6)],
#                measure.vars = c(seq(8,120, by=2)),
#                variable.name = c('timePointYears'),
#                value.name = 'Value'
# )
# suaPr1 <- as.data.table(suaPr1)
# suaPr1 <- suaPr1[!is.na(Country) ]
# 
# suaPr2 <- melt(suaPr, id.vars = colnames(suaPr)[c(1:6)],
#                measure.vars = c(seq(9,121, by=2)),
#                variable.name = c('timePointYears'),
#                value.name = 'Valueflag'
# )
# 
# # put years instead of ..9 -> 1961 ecc..
# suaPr2 <- as.data.table(suaPr2)
# suaPr2 <- suaPr2[!is.na(Country) ]
# SUA1 <- merge(suaPr1, suaPr2, by = colnames(suaPr1)[c(1:7)], all.x = TRUE)
# setnames(SUA1, c("..1", "..3", "..5"), c("geographicAreaM49_fi", "measuredItemFaostat_L2", "measuredElementSuaFbs"))
# SUA1$measuredElementSuaFbs <- as.character(SUA1$measuredElementSuaFbs)
# SUA <- SUA1[!measuredElementSuaFbs %in% c('96', '181')]
# 
# SUA96 <- SUA1[measuredElementSuaFbs == '96']
# SUA181 <- SUA1[measuredElementSuaFbs == '181']
# all(is.na(SUA96$Value))
# all(is.na(SUA181$Value))
# 
# SUA$measuredElementSuaFbs <- ifelse(SUA$measuredElementSuaFbs == '51', '5510',
#                                     ifelse(SUA$measuredElementSuaFbs == '61', '5610',
#                                            ifelse(SUA$measuredElementSuaFbs == '62', '5622',
#                                                   ifelse(SUA$measuredElementSuaFbs == '63', '5637',
#                                                          ifelse(SUA$measuredElementSuaFbs == '91', '5910',
#                                                                 ifelse(SUA$measuredElementSuaFbs == '92', '5922',
#                                                                        ifelse(SUA$measuredElementSuaFbs == '93', '5937',
#                                                                               ifelse(SUA$measuredElementSuaFbs == '101', '5520',
#                                                                                      ifelse(SUA$measuredElementSuaFbs == '111', '5525',
#                                                                                             ifelse(SUA$measuredElementSuaFbs == '121', '5016',
#                                                                                                    ifelse(SUA$measuredElementSuaFbs == '131', '5023',
#                                                                                                           ifelse(SUA$measuredElementSuaFbs == '141', '5141',
#                                                                                                                  ifelse(SUA$measuredElementSuaFbs == '151', '5166',
#                                                                                                                         ifelse(SUA$measuredElementSuaFbs == '31', '5302',
#                                                                                                                                ifelse(SUA$measuredElementSuaFbs == '41', '5423',
#                                                                                                                                       ifelse(SUA$measuredElementSuaFbs == '71', '5071',
#                                                                                                                                              ifelse(SUA$measuredElementSuaFbs == '77', '5052',
#                                                                                                                                                     SUA$measuredElementSuaFbs)))))))))))))))))
# 
# 
# 
# SUA$Valueflag <- as.character(SUA$Valueflag)
# unique(SUA$Valueflag)
# 
# # Conversion:
# # NA = ( , -)
# # F = (E, f)
# # C = (I, i)
# # B = (E, b)
# # . = (E, -) ask how to change
# 
# SUA[ , c("flagObservationStatus", "flagMethod") := list(ifelse(Valueflag == 'F', 'E', 
#                                                                ifelse(Valueflag == 'C', 'I', 
#                                                                       ifelse(Valueflag == 'B', 'E', 
#                                                                              ifelse(Valueflag == '.', 'E', 
#                                                                                     ifelse(is.na(Valueflag), '', Valueflag))))),
#                                                         ifelse(Valueflag == 'F', 'f', 
#                                                                ifelse(Valueflag == 'C', 'i', 
#                                                                       ifelse(Valueflag == 'B', 'b', 
#                                                                              ifelse(Valueflag == '.', '-', 
#                                                                                     ifelse(is.na(Valueflag), '-', Valueflag))))))  ]
# 
# SUA[is.na(flagObservationStatus)]$flagObservationStatus <- ''
# SUA[is.na(flagMethod)]$flagMethod <- '-'
# SUA[ , c("Country", "Commodity", "Element", "Valueflag") := NULL]
# SUA <- SUA[!is.na(Value)]
# 
# SUA$geographicAreaM49_fi <- as.character(SUA$geographicAreaM49_fi)
# SUA$measuredItemFaostat_L2 <- as.character(SUA$measuredItemFaostat_L2)
# SUA$measuredElementSuaFbs <- as.character(SUA$measuredElementSuaFbs)
# SUA$timePointYears <- as.character(SUA$timePointYears)
# SUA$Value <- as.numeric(SUA$Value)
# SUA$flagObservationStatus <- as.character(SUA$flagObservationStatus)
# SUA$flagMethod <- as.character(SUA$flagMethod)
# 
# SUA2merge <- SUA[ , .(geographicAreaM49_fi,
#                       measuredItemFaostat_L2,
#                       timePointYears)]
# setkey(SUA2merge)
# SUA2merge <- unique(SUA2merge)
# 
# SUAPop <- merge(SUA2merge, Pop, by.x = c('geographicAreaM49_fi', 'timePointYears'),
#                 by.y = c('geographicaream49_fi', 'timepointyears'))
# 
# setnames(SUAPop, c('measuredelementsuafbs', 'value', 'flagobservationstatus', 'flagmethod'),
#          c('measuredElementSuaFbs', 'Value', 'flagObservationStatus', 'flagMethod'))
# 
# SUAPr <- rbind(SUA, SUAPop)
# 
# #-- Token QA ----
# if(CheckDebug()){
#   
#   library(faoswsModules)
#   SETTINGS = ReadSettings("sws.yml")
#   
#   ## If you're not on the system, your settings will overwrite any others
#   R_SWS_SHARE_PATH = SETTINGS[["share"]]
#   
#   ## Define where your certificates are stored
#   SetClientFiles(SETTINGS[["certdir"]])
#   
#   ## Get session information from SWS. Token must be obtained from web interface
#   GetTestEnvironment(baseUrl = SETTINGS[["server"]],
#                      token = SETTINGS[["token"]])
#   
# }
# 
# saveRDS(SUAPr, 'SUA-Aug-2019/suaPr.rds')
# 
# suaPr2save <- readRDS('SUA-Aug-2019/suaPr.rds')
# 
# SaveData(domain = "FisheriesCommodities", 
#          dataset = "fi_sua_balanced_control", 
#          data = suaPr2save, waitTimeout = 2000000)
# 
# #-- FBS ----
# 
# fbsPr <- readxl::read_xlsx('SUA-Aug-2019/Peru_FBS data.xlsx', sheet = 1)
# 
# fbsPr <- as.data.table(fbsPr)
# names(fbsPr)
# 
# fbsPr1 <- melt(fbsPr, id.vars = names(fbsPr)[1:2],
#                measure.vars = 3:ncol(fbsPr),
#                variable.name = c('measuredElementSuaFbs'),
#                value.name = 'Value')
# 
# fbsPr1 <- fbsPr1[!is.na(Value)]
# fbsPr1[ , c("flagObservationStatus", "flagMethod") := list('E', 'f')]
# unique(fbsPr1$measuredItemFaostat_L2)
# unique(fbsPr1$measuredElementSuaFbs)
# fbsPr1$measuredElementSuaFbs <- as.character(fbsPr1$measuredElementSuaFbs)
# 
# fbsPr1$measuredElementSuaFbs <- ifelse(fbsPr1$measuredElementSuaFbs == "Production", '5510',
#                                        ifelse(fbsPr1$measuredElementSuaFbs == "Meals input", '5302',
#                                               ifelse(fbsPr1$measuredElementSuaFbs == "Other non-food uses", '5153',
#                                                      ifelse(fbsPr1$measuredElementSuaFbs == "Imports", '5610',
#                                                             ifelse(fbsPr1$measuredElementSuaFbs == "Exports", '5910',
#                                                                    ifelse(fbsPr1$measuredElementSuaFbs == "Stock variations", '5071',
#                                                                           ifelse(fbsPr1$measuredElementSuaFbs == "Total food supply", '5141',
#                                                                                  ifelse(fbsPr1$measuredElementSuaFbs == "Population", '511',
#                                                                                         ifelse(fbsPr1$measuredElementSuaFbs == "Per capita food", '5038',
#                                                                                                ifelse(fbsPr1$measuredElementSuaFbs == "Calories", '264',
#                                                                                                       ifelse(fbsPr1$measuredElementSuaFbs == "Proteins", '274',
#                                                                                                              ifelse(fbsPr1$measuredElementSuaFbs == "Fats", '284',
#                                                                                                                     fbsPr1$measuredElementSuaFbs))))))))))))
# 
# 
# 
# 
# 
# 
# fbsPr1$measuredItemFaostat_L2 <- ifelse(fbsPr1$measuredItemFaostat_L2 == "Freshwater and diadromous fish", "10",
#                                         ifelse(fbsPr1$measuredItemFaostat_L2 == "Demersal fish", "20",
#                                                ifelse(fbsPr1$measuredItemFaostat_L2 == "Pelagic fish", "30",
#                                                       ifelse(fbsPr1$measuredItemFaostat_L2 == "Marine fish, other", "40",
#                                                              ifelse(fbsPr1$measuredItemFaostat_L2 == "Crustaceans", "50",
#                                                                     ifelse(fbsPr1$measuredItemFaostat_L2 == "Molluscs, excl. cephalopods", "60",
#                                                                            ifelse(fbsPr1$measuredItemFaostat_L2 == "Cephalopods", "70",
#                                                                                   ifelse(fbsPr1$measuredItemFaostat_L2 == "Aquatic animals, others", "90",
#                                                                                          fbsPr1$measuredItemFaostat_L2))))))))
# 
# 
# setnames(fbsPr1, "Peru - FIAS FBS", "timePointYears")
# fbsPr1[ , geographicAreaM49_fi := '604']
# fbsPrTot <- fbsPr1[measuredItemFaostat_L2 == 'Totals']
# setnames(fbsPrTot, c("measuredItemFaostat_L2", 
#                      "timePointYears",
#                      "measuredElementSuaFbs",
#                      "Value",
#                      "flagObservationStatus", 
#                      "flagMethod",
#                      "geographicAreaM49_fi"), c("measureditemfaostat_l2", 
#                                                 "timepointyears",
#                                                 "measuredelementsuafbs",
#                                                 "value",
#                                                 "flagobservationstatus", 
#                                                 "flagmethod",
#                                                 "geographicaream49_fi"))
# setcolorder(fbsPrTot, c("geographicaream49_fi",
#                         "measureditemfaostat_l2", 
#                         "timepointyears",
#                         "measuredelementsuafbs",
#                         "value",
#                         "flagobservationstatus", 
#                         "flagmethod"))
# 
# dttot <- ReadDatatable("fbs_fias_tot2019", readOnly = FALSE)
# changeset <- Changeset("fbs_fias_tot2019")
# 
# AddDeletions(changeset, dttot[geographicaream49_fi == '604'])
# Finalise(changeset)
# 
# AddInsertions(changeset, fbsPrTot)
# Finalise(changeset)
# 
# FBS <- fbsPr1[measuredItemFaostat_L2 != 'Totals']
# 
# 
# FBS$measuredItemFaostat_L2 <- as.character(FBS$measuredItemFaostat_L2)
# FBS$geographicAreaM49_fi <- as.character(FBS$geographicAreaM49_fi)
# FBS$timePointYears <- as.character(FBS$timePointYears)
# FBS$measuredElementSuaFbs <- as.character(FBS$measuredElementSuaFbs)
# FBS$flagObservationStatus <- as.character(FBS$flagObservationStatus)
# FBS$flagMethod <- as.character(FBS$flagMethod)
# FBS$Value <- as.numeric(FBS$Value)
# 
# setcolorder(FBS, c("geographicAreaM49_fi", "measuredElementSuaFbs", "measuredItemFaostat_L2",
#                    "timePointYears", "Value", "flagObservationStatus", "flagMethod"))
# 
# saveRDS(FBS, 'SUA-Aug-2019/fbsPr.rds')
# 
# fbsPr2save <- readRDS('SUA-Aug-2019/fbsPr.rds')
# is.data.table(fbsPr2save)
# SaveData(domain = "FisheriesCommodities", 
#          dataset = "fi_fbs_fias_control", 
#          data = fbsPr2save, waitTimeout = 2000000)
# 
# ##-- Iran ----
# 
# suaIr <- readxl::read_xlsx('SUA-Aug-2019/Iran_SUA data.xlsx', sheet = 1)
# 
# suaIr <- as.data.table(suaIr)
# 
# setnames(suaIr, names(suaIr)[seq(9,121, by=2)], names(suaIr)[seq(8,120, by=2)])
# 
# suaIr1 <- melt(suaIr, id.vars = colnames(suaIr)[c(1:6)],
#                measure.vars = c(seq(8,120, by=2)),
#                variable.name = c('timePointYears'),
#                value.name = 'Value'
# )
# suaIr1 <- as.data.table(suaIr1)
# suaIr1 <- suaIr1[!is.na(Country) ]
# 
# suaIr2 <- melt(suaIr, id.vars = colnames(suaIr)[c(1:6)],
#                measure.vars = c(seq(9,121, by=2)),
#                variable.name = c('timePointYears'),
#                value.name = 'Valueflag'
# )
# 
# # put years instead of ..9 -> 1961 ecc..
# suaIr2 <- as.data.table(suaIr2)
# suaIr2 <- suaIr2[!is.na(Country) ]
# SUA1 <- merge(suaIr1, suaIr2, by = colnames(suaIr1)[c(1:7)], all.x = TRUE)
# setnames(SUA1, c("..1", "..3", "..5"), c("geographicAreaM49_fi", "measuredItemFaostat_L2", "measuredElementSuaFbs"))
# SUA1$measuredElementSuaFbs <- as.character(SUA1$measuredElementSuaFbs)
# SUA <- SUA1[!measuredElementSuaFbs %in% c('96', '181')]
# 
# SUA96 <- SUA1[measuredElementSuaFbs == '96']
# SUA181 <- SUA1[measuredElementSuaFbs == '181']
# all(is.na(SUA96$Value))
# all(is.na(SUA181$Value))
# 
# SUA$measuredElementSuaFbs <- ifelse(SUA$measuredElementSuaFbs == '51', '5510',
#                                     ifelse(SUA$measuredElementSuaFbs == '61', '5610',
#                                            ifelse(SUA$measuredElementSuaFbs == '62', '5622',
#                                                   ifelse(SUA$measuredElementSuaFbs == '63', '5637',
#                                                          ifelse(SUA$measuredElementSuaFbs == '91', '5910',
#                                                                 ifelse(SUA$measuredElementSuaFbs == '92', '5922',
#                                                                        ifelse(SUA$measuredElementSuaFbs == '93', '5937',
#                                                                               ifelse(SUA$measuredElementSuaFbs == '101', '5520',
#                                                                                      ifelse(SUA$measuredElementSuaFbs == '111', '5525',
#                                                                                             ifelse(SUA$measuredElementSuaFbs == '121', '5016',
#                                                                                                    ifelse(SUA$measuredElementSuaFbs == '131', '5023',
#                                                                                                           ifelse(SUA$measuredElementSuaFbs == '141', '5141',
#                                                                                                                  ifelse(SUA$measuredElementSuaFbs == '151', '5166',
#                                                                                                                         ifelse(SUA$measuredElementSuaFbs == '31', '5302',
#                                                                                                                                ifelse(SUA$measuredElementSuaFbs == '41', '5423',
#                                                                                                                                       ifelse(SUA$measuredElementSuaFbs == '71', '5071',
#                                                                                                                                              ifelse(SUA$measuredElementSuaFbs == '77', '5052',
#                                                                                                                                                     SUA$measuredElementSuaFbs)))))))))))))))))
# 
# 
# 
# SUA$Valueflag <- as.character(SUA$Valueflag)
# unique(SUA$Valueflag)
# 
# # Conversion:
# # NA = ( , -)
# # F = (E, f)
# # C = (I, i)
# # B = (E, b)
# # . = (E, -) ask how to change
# 
# SUA[ , c("flagObservationStatus", "flagMethod") := list(ifelse(Valueflag == 'F', 'E', 
#                                                                ifelse(Valueflag == 'C', 'I', 
#                                                                       ifelse(Valueflag == 'B', 'E', 
#                                                                              ifelse(Valueflag == '.', 'E', 
#                                                                                     ifelse(is.na(Valueflag), '', Valueflag))))),
#                                                         ifelse(Valueflag == 'F', 'f', 
#                                                                ifelse(Valueflag == 'C', 'i', 
#                                                                       ifelse(Valueflag == 'B', 'b', 
#                                                                              ifelse(Valueflag == '.', '-', 
#                                                                                     ifelse(is.na(Valueflag), '-', Valueflag))))))  ]
# 
# SUA[is.na(flagObservationStatus)]$flagObservationStatus <- ''
# SUA[is.na(flagMethod)]$flagMethod <- '-'
# SUA[ , c("Country", "Commodity", "Element", "Valueflag") := NULL]
# SUA <- SUA[!is.na(Value)]
# 
# SUA$geographicAreaM49_fi <- as.character(SUA$geographicAreaM49_fi)
# SUA$measuredItemFaostat_L2 <- as.character(SUA$measuredItemFaostat_L2)
# SUA$measuredElementSuaFbs <- as.character(SUA$measuredElementSuaFbs)
# SUA$timePointYears <- as.character(SUA$timePointYears)
# SUA$Value <- as.numeric(SUA$Value)
# SUA$flagObservationStatus <- as.character(SUA$flagObservationStatus)
# SUA$flagMethod <- as.character(SUA$flagMethod)
# 
# SUA2merge <- SUA[ , .(geographicAreaM49_fi,
#                       measuredItemFaostat_L2,
#                       timePointYears)]
# setkey(SUA2merge)
# SUA2merge <- unique(SUA2merge)
# 
# SUAPop <- merge(SUA2merge, Pop, by.x = c('geographicAreaM49_fi', 'timePointYears'),
#                 by.y = c('geographicaream49_fi', 'timepointyears'))
# 
# setnames(SUAPop, c('measuredelementsuafbs', 'value', 'flagobservationstatus', 'flagmethod'),
#          c('measuredElementSuaFbs', 'Value', 'flagObservationStatus', 'flagMethod'))
# 
# SUAIr <- rbind(SUA, SUAPop)
# 
# #-- Token QA ----
# if(CheckDebug()){
#   
#   library(faoswsModules)
#   SETTINGS = ReadSettings("sws.yml")
#   
#   ## If you're not on the system, your settings will overwrite any others
#   R_SWS_SHARE_PATH = SETTINGS[["share"]]
#   
#   ## Define where your certificates are stored
#   SetClientFiles(SETTINGS[["certdir"]])
#   
#   ## Get session information from SWS. Token must be obtained from web interface
#   GetTestEnvironment(baseUrl = SETTINGS[["server"]],
#                      token = SETTINGS[["token"]])
#   
# }
# 
# saveRDS(SUAIr, 'SUA-Aug-2019/suaIr.rds')
# suaIr2save <- readRDS('SUA-Aug-2019/suaIr.rds')
# SaveData(domain = "FisheriesCommodities", 
#          dataset = "fi_sua_balanced_control", 
#          data = suaIr2save, waitTimeout = 2000000)
# 
# #-- FBS ----
# 
# fbsIr <- readxl::read_xlsx('SUA-Aug-2019/Iran_FBS data.xlsx', sheet = 1)
# 
# fbsIr <- as.data.table(fbsIr)
# names(fbsIr)
# 
# fbsIr1 <- melt(fbsIr, id.vars = names(fbsIr)[1:2],
#                measure.vars = 3:ncol(fbsIr),
#                variable.name = c('measuredElementSuaFbs'),
#                value.name = 'Value')
# 
# fbsIr1 <- fbsIr1[!is.na(Value)]
# fbsIr1[ , c("flagObservationStatus", "flagMethod") := list('E', 'f')]
# unique(fbsIr1$measuredItemFaostat_L2)
# unique(fbsIr1$measuredElementSuaFbs)
# fbsIr1$measuredElementSuaFbs <- as.character(fbsIr1$measuredElementSuaFbs)
# 
# fbsIr1$measuredElementSuaFbs <- ifelse(fbsIr1$measuredElementSuaFbs == "Production", '5510',
#                                        ifelse(fbsIr1$measuredElementSuaFbs == "Meals input", '5302',
#                                               ifelse(fbsIr1$measuredElementSuaFbs == "Other non-food uses", '5153',
#                                                      ifelse(fbsIr1$measuredElementSuaFbs == "Imports", '5610',
#                                                             ifelse(fbsIr1$measuredElementSuaFbs == "Exports", '5910',
#                                                                    ifelse(fbsIr1$measuredElementSuaFbs == "Stock variations", '5071',
#                                                                           ifelse(fbsIr1$measuredElementSuaFbs == "Total food supply", '5141',
#                                                                                  ifelse(fbsIr1$measuredElementSuaFbs == "Population", '511',
#                                                                                         ifelse(fbsIr1$measuredElementSuaFbs == "Per capita food", '5038',
#                                                                                                ifelse(fbsIr1$measuredElementSuaFbs == "Calories", '264',
#                                                                                                       ifelse(fbsIr1$measuredElementSuaFbs == "Proteins", '274',
#                                                                                                              ifelse(fbsIr1$measuredElementSuaFbs == "Fats", '284',
#                                                                                                                     fbsIr1$measuredElementSuaFbs))))))))))))
# 
# 
# 
# 
# 
# 
# fbsIr1$measuredItemFaostat_L2 <- ifelse(fbsIr1$measuredItemFaostat_L2 == "Freshwater and diadromous fish", "10",
#                                         ifelse(fbsIr1$measuredItemFaostat_L2 == "Demersal fish", "20",
#                                                ifelse(fbsIr1$measuredItemFaostat_L2 == "Pelagic fish", "30",
#                                                       ifelse(fbsIr1$measuredItemFaostat_L2 == "Marine fish, other", "40",
#                                                              ifelse(fbsIr1$measuredItemFaostat_L2 == "Crustaceans", "50",
#                                                                     ifelse(fbsIr1$measuredItemFaostat_L2 == "Molluscs, excl. cephalopods", "60",
#                                                                            ifelse(fbsIr1$measuredItemFaostat_L2 == "Cephalopods", "70",
#                                                                                   ifelse(fbsIr1$measuredItemFaostat_L2 == "Aquatic animals, others", "90",
#                                                                                          fbsIr1$measuredItemFaostat_L2))))))))
# 
# 
# setnames(fbsIr1, "Iran - FIAS FBS", "timePointYears")
# fbsIr1[ , geographicAreaM49_fi := '364']
# fbsIrTot <- fbsIr1[measuredItemFaostat_L2 == 'Totals']
# setnames(fbsIrTot, c("measuredItemFaostat_L2", 
#                      "timePointYears",
#                      "measuredElementSuaFbs",
#                      "Value",
#                      "flagObservationStatus", 
#                      "flagMethod",
#                      "geographicAreaM49_fi"), c("measureditemfaostat_l2", 
#                                                 "timepointyears",
#                                                 "measuredelementsuafbs",
#                                                 "value",
#                                                 "flagobservationstatus", 
#                                                 "flagmethod",
#                                                 "geographicaream49_fi"))
# setcolorder(fbsIrTot, c("geographicaream49_fi",
#                         "measureditemfaostat_l2", 
#                         "timepointyears",
#                         "measuredelementsuafbs",
#                         "value",
#                         "flagobservationstatus", 
#                         "flagmethod"))
# 
# dttot <- ReadDatatable("fbs_fias_tot2019")
# changeset <- Changeset("fbs_fias_tot2019")
# AddInsertions(changeset, fbsIrTot)
# Finalise(changeset)
# 
# FBS <- fbsIr1[measuredItemFaostat_L2 != 'Totals']
# 
# 
# FBS$measuredItemFaostat_L2 <- as.character(FBS$measuredItemFaostat_L2)
# FBS$geographicAreaM49_fi <- as.character(FBS$geographicAreaM49_fi)
# FBS$timePointYears <- as.character(FBS$timePointYears)
# FBS$measuredElementSuaFbs <- as.character(FBS$measuredElementSuaFbs)
# FBS$flagObservationStatus <- as.character(FBS$flagObservationStatus)
# FBS$flagMethod <- as.character(FBS$flagMethod)
# FBS$Value <- as.numeric(FBS$Value)
# 
# setcolorder(FBS, c("geographicAreaM49_fi", "measuredElementSuaFbs", "measuredItemFaostat_L2",
#                    "timePointYears", "Value", "flagObservationStatus", "flagMethod"))
# 
# saveRDS(FBS, 'SUA-Aug-2019/fbsIr.rds')
# fbsIr2save <- readRDS('SUA-Aug-2019/fbsIr.rds')
# is.data.table(fbsIr2save)
# SaveData(domain = "FisheriesCommodities", 
#          dataset = "fi_fbs_fias_control", 
#          data = fbsIr2save, waitTimeout = 2000000)
# 
# 
# 
