# Function to map codes from ISSCAAP to ASFIS

isscaap2asfis <- function(codes, version = "latest"){ 
  
  library(data.table)
  
  ## Data Quality Checks
  stopifnot(is(codes, "character"))
  if(!exists("swsContext.datasets")){
    stop("No swsContext.datasets object defined.  Thus, you probably ",
         "won't be able to read from the SWS and so this function won't ",
         "work.")
  }
  # 
  # validversions = c("latest", "2.1preliminary", "2.1")
  # latestversion = "2.1"
  # stopifnot(version %in% validversions)
  # if(version == "latest"){
  #   version = latestversion
  # }
  
  map <- faosws::ReadDatatable("fishery_primary_mapping")
  
  stopifnot(codes %in% map$isscaap)
  
  out <- merge(data.table(isscaap = unique(codes)), map, by = "isscaap",
               all.x = TRUE, allow.cartesian = TRUE)
  ## Set the key so we can sort by passing in the vector
  setkeyv(out, "isscaap")
  return(list(asfis = out[codes, alphacode, allow.cartesian = TRUE],
              Whole = out))
}

# Test:
# isscaap2asfis(12, asfisISSCAAP)

# isscaap2asfis(c(12,39), asfisISSCAAP)