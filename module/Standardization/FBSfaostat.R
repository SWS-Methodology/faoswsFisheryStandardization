##' # FBS creation according Faostat and Fias methodologies
##'
##' **Author: Charlotte Taglioni**
##'
##' **Description:**
##'
##' This module aggregates and produces Food Balance Sheets according 
##' Faostat and Fias methodoligies starting from the SUA balanced
##' 
##' **Inputs:**
##'
##' * SUA balanced resulting from fi_SuaBalancing plugin
##' 
##' * Datatables for balancing elements

##'
##' **Flag assignment:**
##'
##' | Observation Status Flag | Method Flag|
##' | --- | --- | --- |

suppressMessages({
  library(faosws)
  library(faoswsUtil)
  library(data.table)
})

library(faoswsModules)

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

sessionCountry <- swsContext.datasets[[1]]@dimensions$geographicAreaM49_fi@keys 
yearVals <- swsContext.datasets[[1]]@dimensions$timePointYears@keys


message(paste('Session country ', sessionCountry))

message("Pulling data from SUA")

KeySUA <- DatasetKey(domain = "FisheriesCommodities", dataset = "fi_sua_unbalanced", dimensions = list(
  geographicAreaM49_fi = Dimension(name = "geographicAreaM49_fi", keys = sessionCountry),
  measuredElement = Dimension(name = "measuredElement", keys = GetCodeList("FisheriesCommodities", "fi_sua_unbalanced","measuredElement" )[,code]),
  measuredItemFaostat_L2 = Dimension(name = "measuredItemFaostat_L2", keys =  GetCodeList("FisheriesCommodities", "fi_sua_unbalanced","measuredItemFaostat_L2" )[,code] ),
  timePointYears = Dimension(name = "timePointYears", keys = yearVals )))

SUA <- GetData(KeySUA)

# get all conversion factors (or extration rates) from commodity tree
tree <- ReadDatatable('commodity_tree_fi_provisional')
primary <- unique(tree[!parent %in% child]$parent)

convFact <- unique(tree[weight == TRUE & !is.na(extraction_rate) , .(parent, child, extraction_rate)])

# add primary ICS with Er equal 1
convFact <- rbind(convFact, data.table(parent = primary, child = primary, extraction_rate = 1))

if(nrow(convFact[duplicated(convFact$child),]) > 0){
  stop('Duplication in conversion factors')
}

# SUA with standardized element, no zero weight elements
SUAstand <- merge(SUA2save, convFact, by.x = 'measuredItemFaostat_L2',
                  by.y = 'child')

# Standardised value is Value/eR
SUAstand[ , Value_stand := Value/extraction_rate]

#-- Get population data ----
elemKeys <- "511"

keyPop <- DatasetKey(domain = "population", dataset = "population_unpd", dimensions = list(
  geographicAreaM49 = Dimension(name = "geographicAreaM49", keys = sessionCountry),
  measuredElement = Dimension(name = "measuredElement", keys = elemKeys),
  timePointYears = Dimension(name = "timePointYears", keys = yearVals)
))

popSWS <- GetData(keyPop)

setnames(popSWS,c("geographicAreaM49", "measuredElement"),c("geographicAreaM49_fi", "measuredElementSuaFbs"))

##-- FAOSTAT ----
# Aggregate SUA by parent meals included for FAOSTAT

SUAstandAggr <- SUAstand[ , .(measuredElementSuaFbs, geographicAreaM49_fi, timePointYears,
                              flagObservationStatus, flagMethod, parent, Value_stand)]



SUAstandAggr$flagObservationStatus <- factor(SUAstandAggr$flagObservationStatus, levels = c('M', 'O', 'N', '', 'X', 'T', 'E', 'I'), ordered = TRUE)

SUAstandAggr <- SUAstandAggr[ , list(Value = sum(Value_stand, na.rm = TRUE),
                 flagObservationStatusAggr = max(flagObservationStatus),
                 flagMethodAggr = 's'), by = c("measuredElementSuaFbs", "geographicAreaM49_fi",
                                            "timePointYears", "parent")]

setnames(SUAstandAggr, c('parent', 'flagObservationStatusAggr', 'flagMethodAggr'), 
         c('measuredItemFaostat_L2', 'flagObservationStatus', 'flagMethod'))


faostatOutmain <- merge(SUAstandAggr, popSWS, by=c("geographicAreaM49_fi","timePointYears"), suffixes = c("","_pop")) 
# 
faostatOutmain[ measuredElementSuaFbs %in% c("261","271","281"), Value_caput_day:= (Value/Value_pop)/365]  
nutrient_caput_day <- faostatOutmain[measuredElementSuaFbs %in% c("261","271","281")
                         ,.(measuredItemFaostat_L2,geographicAreaM49_fi,timePointYears,measuredElementSuaFbs,Value_caput_day)]


nutrient_caput_day[measuredElementSuaFbs=="261",measuredElementSuaFbs:="calories_caput_day"]
nutrient_caput_day[measuredElementSuaFbs=="281",measuredElementSuaFbs:="fats_caput_day"]
nutrient_caput_day[measuredElementSuaFbs=="271",measuredElementSuaFbs:="proteins_caput_day"]

setnames(nutrient_caput_day,"Value_caput_day", "Value")

out=faostatOutmain[,.(measuredItemFaostat_L2,geographicAreaM49_fi,timePointYears,measuredElementSuaFbs, Value)]

out=rbind(out, nutrient_caput_day)



#-- FAOSTAT FBS standardization ----

message("Starting Faostat standardization")
faostatGroups <- data.table(measuredElementSuaFbs = c("5510", "5610", "5910", "5912", "5520", 
                                                "5525", "5166", "5071", "5016", "5141", "5036", "664"),
                            faostat = c('Production', 'Imports', 'Exports', 'Exports', 'Feed', 'Bait',
                                        'Other net uses', 'Other net uses', 'Other net uses',
                                        'Total food consumption', 'Food quantity/day/capita (gr)',
                                        'Food consumption (kcal)'),
                            measuredElementFaostat = c("5510", "5610", "5910", "5910", "5520", 
                                                       "5525", "5153", "5153", "5153", "5141", "5036", "664"))

FBSfaostat <- merge(SUAstandAggr, faostatGroups, by = "measuredElementSuaFbs")
FBSfaostat <- FBSfaostat[ , list(Value = sum(Value, na.rm = TRUE),
                                 flagObservationStatus = max(flagObservationStatus),
                                 flagMethod = 's'), by = c("geographicAreaM49_fi",
                                                               "timePointYears", "measuredItemFaostat_L2",
                                                               "faostat", "measuredElementFaostat")]

FBSfaostat <- FBSfaostat[ , Value := ifelse(measuredElementFaostat %in% c("264"), Value, Value/1000)]
setnames(FBSfaostat, c("faostat", "measuredElementFaostat"), c("element_description", "measuredElementSuaFbs"))
#-- Introduce population data

sua_fbs_mapping <- data.table(measuredItemFaostat_L2 = primary[primary!='1579'],
                           fbs = c(  "10",  "20",  "30",  "40",  "50",  "60",  "70",  "90"))


faostatOut <- merge(FBSfaostat, sua_fbs_mapping, by="measuredItemFaostat_L2", all.x = TRUE)
faostatOut[, measuredItemFaostat_L2:=NULL]
setnames(faostatOut, "fbs", "measuredItemFaostat_L2")




#-- FIAS ----
# Aggregate SUA by parent code take away meals
mealCodes <- c('1508', '1521', '1534', '1547', '1558', '1566', '1575', '1589')
SUAstandAggrFias <- SUAstand[ !measuredItemFaostat_L2 %in% mealCodes , .(measuredElementSuaFbs, geographicAreaM49_fi, timePointYears,
                              flagObservationStatus, flagMethod, parent, Value_stand)]

mealsInput <- SUAstand[ measuredItemFaostat_L2 %in% mealCodes , .(measuredElementSuaFbs, geographicAreaM49_fi, timePointYears,
                                                                   flagObservationStatus, flagMethod, 
                                                                  parent, Value_stand)]
setnames(mealsInput, c("parent", "Value_stand"), c("measuredItemFaostat_L2", "Value"))

SUAstandAggrFias$flagObservationStatus <- factor(SUAstandAggrFias$flagObservationStatus, levels = c('M', 'O', 'N', '', 'X', 'T', 'E', 'I'), ordered = TRUE)

SUAstandAggrFias <- SUAstandAggrFias[ , list(Value = sum(Value_stand, na.rm = TRUE),
                                     flagObservationStatusAggr = max(flagObservationStatus),
                                     flagMethodAggr = 's'), by = c("measuredElementSuaFbs", "geographicAreaM49_fi",
                                                                   "timePointYears", "parent")]

setnames(SUAstandAggrFias, c('parent', 'flagObservationStatusAggr', 'flagMethodAggr'), 
         c('measuredItemFaostat_L2', 'flagObservationStatus', 'flagMethod'))


# Include population data

fias_pop <- merge(SUAstandAggrFias, popSWS, by=c("geographicAreaM49_fi","timePointYears"), suffixes = c("","_pop"))  
fias_pop[ measuredElementSuaFbs %in% c("261","271","281"), Value_caput_day:= (Value/Value_pop)]  
nutrient_caput_year_FIAS <- fias_pop[measuredElementSuaFbs %in% c("261","271","281")
                                     ,.(measuredItemFaostat_L2,geographicAreaM49_fi,timePointYears,
                                        measuredElementSuaFbs,Value_caput_day,flagObservationStatus, flagMethod)]


nutrient_caput_year_FIAS[measuredElementSuaFbs=="261",measuredElementSuaFbs:="264"]
nutrient_caput_year_FIAS[measuredElementSuaFbs=="281",measuredElementSuaFbs:="284"]
nutrient_caput_year_FIAS[measuredElementSuaFbs=="271",measuredElementSuaFbs:="274"]

setnames(nutrient_caput_year_FIAS,"Value_caput_day", "Value")

fiasFbs <- fias_pop[,.(measuredItemFaostat_L2,geographicAreaM49_fi,timePointYears,measuredElementSuaFbs, 
                       Value, flagObservationStatus, flagMethod)]

fiasFbsTot1 <- rbind(fiasFbs, nutrient_caput_year_FIAS)
fiasFbsTot <- rbind(fiasFbsTot1, mealsInput)
IcsGroups <- unique(fiasFbsTot[ , .(timePointYears, measuredItemFaostat_L2)])

# Introduce population data
pop2merge <- merge(popSWS, IcsGroups, by = "timePointYears", all = TRUE)
fiasfbsPOP <- rbind(fiasFbsTot, pop2merge)

# mapp for FBS groups
sua_fbs_mapping <- data.table(measuredItemFaostat_L2 = primary[primary!='1579'],
                              fbs = c(  "10",  "20",  "30",  "40",  "50",  "60",  "70",  "90"))

fbsFaostatL1 <- merge(fiasfbsPOP, sua_fbs_mapping, by = "measuredItemFaostat_L2")
fbsFaostatL1[ , measuredItemFaostat_L2 := NULL]
setnames(fbsFaostatL1, "fbs", "measuredItemFaostat_L2")

#-- FIAS FBS standardization ---- 
message("Starting Fias standardization")
fiasGroups <- data.table(measuredElementSuaFbs = c("5510", "5302", "5610", "5910", "5912", "5520", 
                                             "5525", "5166", "5071", "5016", "5141", "511", "261", "264",
                                             "274", "284"),
                         fias = c('Production', 'Meals input', 'Imports', 'Exports', 'Exports', 'Other non-food uses', 
                                  'Other non-food uses', 'Other non-food uses', 'Other non-food uses', 'Stock variations',
                                  'Total food supply', 'Population', 'Per capita food',
                                  'Calories', 'Proteins', 'Fats'),
                         measuredElementFias = c("5510", "5302", "5610", "5910", "5910", "5153", 
                                                 "5153", "5153", "5153", "5071", "5141", "511", "261", # or "5036" instead of 261 
                                                 "264", "274", "284"))

# # Ics groups regarding meals
# MealsIcs <- c('1508', '1521', '1534', '1547', '1558', '1566', '1575', '1589')
# # select input values only for meals
# Mealsinput <- fiasFbsTot[ measuredElementSuaFbs == "5302" & measuredItemFaostat_L2 %in% MealsIcs]
# 
# MealsinputParent <- merge(Mealsinput,tree[parent %in% primary , .(parent, child)], by.x = c('measuredItemFaostat_L2'), by.y = c('child'))
# 
# # Meal inputs have to be taken away from production
# mealsProdAway <- merge(fiasFbsTot[measuredElementSuaFbs == "5510" & ! measuredItemFaostat_L2 %in% MealsIcs], MealsinputParent, 
#       by.x = c("geographicAreaM49_fi", "timePointYears", "measuredItemFaostat_L2"),
#       by.y = c("geographicAreaM49_fi", "timePointYears", "parent"),
#       suffixes = c("_prod", "_meals"), all.x = TRUE)
# 
# mealsProdAway$Value_meals <- as.numeric(mealsProdAway$Value_meals)
# mealsProdAway[ , Value := ifelse(is.na(Value_meals), Value_prod, (Value_prod - Value_meals)) ]
# mealsProdAway <- mealsProdAway[ , .(geographicAreaM49_fi, timePointYears, measuredItemFaostat_L2,
#                                     measuredElementSuaFbs_prod, Value, flagObservationStatus_prod,
#                                            flagMethod_prod)]
# setnames(mealsProdAway, c('measuredElementSuaFbs_prod', 'flagObservationStatus_prod', 'flagMethod_prod'), 
#          c('measuredElementSuaFbs', 'flagObservationStatus','flagMethod'))
# 
# SUAstandFias <- rbind(SUAstandAggr[ measuredElementSuaFbs != '5510' & ! measuredItemFaostat_L2 %in% MealsIcs], 
#                       Mealsinput, mealsProdAway)


# FBS
FBSfias <- merge(fbsFaostatL1, fiasGroups, by = "measuredElementSuaFbs")
FBSfias <- FBSfias[ , list(Value = sum(Value, na.rm = TRUE),
                                 flagObservationStatus = max(flagObservationStatus),
                                 flagMethod = 's'), by = c("geographicAreaM49_fi",
                                                           "timePointYears", "measuredItemFaostat_L2",
                                                           "fias", "measuredElementFias")]

fiasFBS2save <-  FBSfias[, .(geographicAreaM49_fi, timePointYears, measuredItemFaostat_L2, 
    measuredElementFias, Value, flagObservationStatus, flagMethod)]



message("Saving FIAS standardized data")
statsFias <- SaveData(domain = "FisheriesCommodities", 
                  dataset = "fi_fbs_fias", 
                  data = fiasFBS2save, waitTimeout = Inf)

paste0("Standardization process completed successfully! ",
       statsFias$inserted, " observations written, ",
       statsFias$ignored, " weren't updated, ",
       statsFias$discarded, " had problems.")

##-- send Email with notification of correct execution ----

## Initiate email
from = "sws@fao.org"
to = swsContext.userEmail
subject = "fi_standardization plug-in has correctly run"
body = "The plug-in has saved the SUAs unbalanced in your session"

sendmailR::sendmail(from = from, to = to, subject = subject, msg = body)
paste0("Email sent to ", swsContext.userEmail)
