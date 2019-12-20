# If for a period of time a certain quantity has to migrate from an ICS group to another
# this function modifies the link table (link_tableping) to be compliant with the SUA

## Needed packages ----
suppressMessages( {
  library(data.table)
  library(shiny)
  library(faosws)
  library(data.table)
  library(shiny)
  library(faoswsStandardization)
  library(faoswsProcessing)
  library(faoswsUtil)
  library(faoswsModules)
  library(faosws)
  library(faoswsFlag)
  library(DT)
  library(dplyr)
  library(faoswsFisheryStandardization)
})

link_function <- function(link_table, 
                          country,
                          lastYear=2016, 
                          flowcode = list(exp = "91", imp = "61", prd = "51")){
 
  # Select only country of interest
  link_table <- link_table[geographic_area_m49 == country, ]
  
   ### Checks

  # Stop if duplicates
  stopifnot(base::all.equal(link_table, unique(link_table)))
  
  # Stop if colnames are different from expected
  stopifnot(all.equal(colnames(link_table), c("geographic_area_m49", "flow",
                                     "start_year", "end_year",
                                     "from_code", "percentage", "to_code")))
  
  # Check if no errors in allocation 
  link_table[, check := sum(percentage),
                  by = c("geographic_area_m49","flow","from_code","start_year","end_year")]
  
  
  if(any(link_table$check > 1)){
    stop("More than 100% of the quantity is re-allocated!")
  }
  
  if(any(link_table[, check] < 1)){
    print("Less than 100% of the quantity is re-allocated!")
    
    # Column where remaining part is stored if less than 100% has been reallocated
    link_table[ , remain := (1 - link_table[ , check])]
    
    # Add rows for reamining part to allocate
    append <- link_table[which(link_table$remain!=0), .(geographic_area_m49 ,flow ,start_year, end_year ,from_code, check, remain)]
    append[, percentage := remain]
    append[ , to_code := from_code]
    
    link_table <- rbind(link_table, append)
    # Remove columns for check
    link_table <- link_table[,  c("check", "remain") := NULL]
  } else {
    # Remove columns for check
    link_table <- link_table[, check := NULL]
    }
  
  # Unfold "ALL" labels into c("PRD", "IMP", "EXP")
  # and "TRD" into c( "IMP", "EXP")

  all <- c("PRD", "IMP", "EXP")
  all <- data.table(flow=rep("ALL", length(all)),all=all)
  link_all <- merge(link_table, all, by="flow")
  link_all[,flow:=all]
  link_all[,all:=NULL]
  
  trade <- c( "IMP", "EXP")
  trade <- data.table(flow=rep("TRD", length(trade)),trade=trade)
  link_trade <- merge(link_table, trade, by="flow", allow.cartesian = TRUE)
  link_trade[,flow:=trade]
  link_trade[,trade:=NULL]
  
  # Re-create the link table again
  link_table <- link_table[!flow %in% c("ALL", "TRD"),]
  link_table <- rbind(link_table,link_trade,link_all)
  
  
  # Stop if still ALL and/or TRD labels
  if(any(link_table[ , flow %in% c("ALL", "TRD")])){
    stop("Still ALL and/or TRD labels appearing")
  }
  
  # Substitute LAST with actual last year
  link_table[end_year=="LAST", end_year:=as.character(lastYear)]
  link_table$end_year <- as.integer(link_table$end_year)
  
  # Change flow labels into codes
  link_table[flow=="IMP",flow:= flowcode$imp]
  link_table[flow=="EXP",flow:= flowcode$exp]
  link_table[flow=="PRD",flow:= flowcode$prd]
  
  ### Modify the ICS_Code according to the linkNew file:
  
  # Change columns names

  setnames(link_table, 
           old = c("geographic_area_m49", "flow", "from_code") , 
           new = c("geographicAreaM49", "measuredElement",  "ics"))
# Make sure the class each column is returned is compatible with the SUA
  
  return(link_table)
    
}

