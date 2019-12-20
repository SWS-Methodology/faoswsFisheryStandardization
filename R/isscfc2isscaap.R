# Function to map codes from ISSCFC to ISSCAAP

isscfc2isscaap <- function(codes, version = "latest"){ 
  library(data.table)
  
  stopifnot(is(codes, "character"))
  if(!exists("swsContext.datasets")){
    stop("No swsContext.datasets object defined.  Thus, you probably ",
         "won't be able to read from the SWS and so this function won't ",
         "work.")
  }
  
  map <- faosws::ReadDatatable("fishery_item_mapping")
  
  stopifnot(codes %in% map$isscfc) #SHOW A MESSAGE?
  
  # validversions = c("latest", "2.1preliminary", "2.1")
  # latestversion = "2.1"
  # stopifnot(version %in% validversions)
  # if(version == "latest"){
  #   version = latestversion
  # }
  
  
  
  out <- merge(data.table(isscfc = unique(codes)), map, by = "isscfc",
               all.x = TRUE, allow.cartesian = TRUE)
  ## Set the key so we can sort by passing in the vector
  setkeyv(out, "isscfc")
  return(list(isscaap = unique(out[codes, isscaap, allow.cartesian = TRUE]),
              Whole = out))
}

# Test:
# isscfc2isscaap("034.1.2.1.30")
# 
# vec <- c("034.1.2.1.25","034.1.2.1.30")
# isscfc2isscaap(vec,isscfcISSCAAP)
# 
# vec1 <- c("034.1.1","034.1.1.1","034.1.1.1.11",
# "034.1.1.1.19","034.1.1.2.90","034.1.2.1.10","034.1.2.1.19","034.1.2.1.20")
# 
# isscaap2asfis(unlist(isscfc2isscaap(vec1,isscfcISSCAAP)$isscaap), asfisISSCAAP)
