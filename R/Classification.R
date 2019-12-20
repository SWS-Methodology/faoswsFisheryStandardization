library(readxl)

##-- Map ISSCFC and ISSCAAP  ----
commodity <- readxl::read_xlsx("C:/Users/Taglionic/Desktop/Fisheries/Documents_fisheries/ybklang_12032019.xlsx", 
                        sheet = 1, col_names = TRUE)
commodity <- as.data.table(commodity)
commodity_select <- commodity[ , .(`Internal Code`, `ISSCAAP Code`, `Fao Commodity Code`, `English Name`,  `ICS Code`, `Conversion Factor`)]

isscaapNames <- readxl::read_xlsx("C:/Users/Taglionic/Documents/Github/faoswsFisheryStandardization/Input_documents/classification.xlsx",
                        sheet = 5, col_names = TRUE, skip = 1)

isscaapNames <- as.data.table(isscaapNames)
isscaapNames <- isscaapNames[ , .( `ISSCAAP group: name`, `ISSCAAP group: code`)]
setkey(isscaapNames)
isscaapNames <- isscaapNames[ !duplicated(isscaapNames)]

isscaapNames$`ISSCAAP group: code` <- as.character(isscaapNames$`ISSCAAP group: code`)

commodity2isscaap <- merge(commodity_select, isscaapNames, by.x = "ISSCAAP Code", by.y = "ISSCAAP group: code",   
                          all = TRUE)

# Add some names that are not in the file
commodity2isscaap$`ISSCAAP group: name` <- ifelse(commodity2isscaap$`ISSCAAP Code`=="61", "Blue-whales, fin-whales",
                                                 ifelse(commodity2isscaap$`ISSCAAP Code`=="62", "Sperm-whales, pilot-whales",
                                                        ifelse(commodity2isscaap$`ISSCAAP Code`=="63","Eared seals, hair seals, walruses",
                                                               ifelse(commodity2isscaap$`ISSCAAP Code`=="81", "Pearls, mother-of-pearl, shells",
                                                                      ifelse(commodity2isscaap$`ISSCAAP Code`=="94", "Miscellaneous aquatic plants",
                                                                             ifelse(commodity2isscaap$`ISSCAAP Code`=="92", "Red seaweeds",
                                                                                  ifelse(commodity2isscaap$`ISSCAAP Code`=="82", "Corals",
                                                                                         ifelse(commodity2isscaap$`ISSCAAP Code`=="91", "Brown seaweeds", 
                                                                                                ifelse(commodity2isscaap$`ISSCAAP Code`=="93", "Green seaweeds",
                                                                                                       ifelse(commodity2isscaap$`ISSCAAP Code`=="83", "Sponges",
                                                                                                              ifelse(commodity2isscaap$`ISSCAAP Code`=="64", "Miscellaneous aquatic mammals", 
                                                                                                                     commodity2isscaap$`ISSCAAP group: name`) )))) )) ))))

# Add ICS codes
ics <- readxl::read_xlsx("C:/Users/Taglionic/Documents/Github/faoswsFisheryStandardization/Input_documents/classification.xlsx",
                         sheet = 1, col_names = TRUE)
ics <- as.data.table(ics)
ics <- ics[ , .(`ICS_FAOSTAT Code`, `English Name`)]
setnames(ics, "English Name", "ICS Label")

# Combine data.tables
commodity2isscaap_complete <- merge(commodity2isscaap, ics, by.x = "ICS Code", by.y = "ICS_FAOSTAT Code",
                                   all.x = TRUE)

commodity2isscaap_final <- commodity2isscaap_complete[, .( `ICS Code`,
                                                         `ICS Label`,
                                                         `ISSCAAP Code`, 
                                                         `ISSCAAP group: name`,    
                                                         `Internal Code`, 
                                                         `Fao Commodity Code`,
                                                         `English Name`,
                                                         `Conversion Factor`)]
commodity2isscaap_final <- commodity2isscaap_final[order(`ICS Code`),]
setnames(commodity2isscaap_final, names(commodity2isscaap_final), 
         c("ics", "ics_label", "isscaap", "isscaap_label", "internal_code", "isscfc", "isscfc_label", "conversion_factor"))

write.csv(commodity2isscaap_final, file = "C:/Users/Taglionic/Documents/Github/faoswsFisheryStandardization/Input_documents/isscfc_isscaap_classification.csv",
          row.names = FALSE, fileEncoding = "UTF-8")

##-- Map ASFIS and ISSCAAP  ----

isscaap2asfis <- readxl::read_xlsx("C:/Users/Taglionic/Documents/Github/faoswsFisheryStandardization/Input_documents/classification.xlsx",
                                  sheet = 6, col_names = TRUE)

isscaap2asfis <- as.data.table(isscaap2asfis)
isscaap2asfis <- isscaap2asfis[ , . (ISSCAAP, TAXOCODE, `3A_CODE`, Scientific_name, English_name)]
setnames(isscaap2asfis, names(isscaap2asfis), 
         c("isscaap", "asfis_taxocode", "asfis_alphacode", "asfis_label_scientific", "asfis_label_english"))

isscaapLabels <- unique(commodity2isscaap_final[, .(isscaap, isscaap_label)])

isscaap2asfis_final <- merge(isscaap2asfis, isscaapLabels, by = "isscaap", all = TRUE)

isscaap2asfis_final <- isscaap2asfis_final[order(isscaap2asfis_final$isscaap)]
isscaap2asfis_final <- isscaap2asfis_final[, .(isscaap,
                                               isscaap_label,
                                               asfis_taxocode,
                                               asfis_alphacode,
                                               asfis_label_scientific,
                                               asfis_label_english)]

write.csv(isscaap2asfis_final, file = "C:/Users/Taglionic/Documents/Github/faoswsFisheryStandardization/Input_documents/asfis_isscaap_classification.csv",
          row.names = FALSE, fileEncoding = "UTF-8")

##-- Map ISSCFC and ASFIS through ISSCAAP  ----

complete_table <- merge(isscaap2asfis_final, commodity2isscaap_final, 
                        by = c("isscaap", "isscaap_label"), all = TRUE,
                        allow.cartesian = TRUE)

complete_table <- complete_table[order(complete_table$ics),]

complete_table <- complete_table[ , .(ics, ics_label, isscaap, isscaap_label,
                                      internal_code, isscfc, isscfc_label, 
                                      asfis_taxocode, asfis_alphacode, 
                                      asfis_label_scientific, asfis_label_english)]




write.csv(complete_table, file = "C:/Users/Taglionic/Documents/Github/faoswsFisheryStandardization/Input_documents/FisheryClassification.csv",
          row.names = FALSE, fileEncoding = "UTF-8")
