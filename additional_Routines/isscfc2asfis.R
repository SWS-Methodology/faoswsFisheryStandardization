# Function to map codes from ISSCFC to ASFIS alphacode

isscfc2asfis <- function(codes, version = "latest"){ 
  
  library(data.table)
  ## Data Quality Checks
  # Class of the columns (character, numeric, double, etc)
  
  stopifnot(is(codes, "character"))
  if(!exists("swsContext.datasets")){
    stop("No swsContext.datasets object defined.  Thus, you probably ",
         "won't be able to read from the SWS and so this function won't ",
         "work.")
  }
  
  # validversions = c("latest", "2.1preliminary", "2.1")
  # latestversion = "2.1"
  # stopifnot(version %in% validversions)
  # if(version == "latest"){
  #   version = latestversion
  # }
  
  map_ii <- faosws::ReadDatatable("fishery_item_mapping")
  map_ia <- faosws::ReadDatatable("fishery_primary_mapping")
  
  stopifnot(codes %in% map_ii$isscfc)

 #  
 # aaa<- merge(map_ii, map_ia, by = "isscaap", all = TRUE,
 #        allow.cartesian = TRUE)
 #  
  out_ii <- merge(data.table(isscfc = unique(codes)), map_ii, by = "isscfc",
                 all.x = TRUE, allow.cartesian = TRUE)
  colnames(out_ii) <- c("measuredItemISSCFC", "isscaap", "ics", "English.Name")
  out_ia <- merge(out_ii, map_ia, by = "isscaap",
                  all.x = TRUE, allow.cartesian = TRUE)
  
  ## Set the key so we can sort by passing in the vector
  setkeyv(out_ia, "measuredItemISSCFC")
  return(list(ASFIS = unique(out_ia[codes, .(measuredItemISSCFC, alphacode), allow.cartesian = TRUE]),
              Whole = out_ia))
}

# Test:
# vec <- c("034.1.2.1.25","034.1.2.1.30")
# 
# prova <- isscfc2asfis(vec)
# 
# prova1 <- isscfc2asfis(vec[1], isscfcISSCAAP, asfisISSCAAP)
# 
# prova2 <- isscfc2asfis(vec[2], isscfcISSCAAP, asfisISSCAAP)
# 
# 
# vec1 <- c("034.1.1","034.1.1.1","034.1.1.1.11",
#           "034.1.1.1.19","034.1.1.2.90","034.1.2.1.10","034.1.2.1.19","034.1.2.1.20")
# 
# prova3 <- isscfc2asfis(vec1, isscfcISSCAAP, asfisISSCAAP)