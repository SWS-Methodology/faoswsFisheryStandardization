##' Fishery Standardization Parameters
##' 
##' Provides an object which contains the standardization parameters.  This 
##' allows for easy passing into functions.  The meaning of most variables is 
##' fairly clear, but a few obscure ones are described below:
##' 
##' 
##' @return A list with the standardization parameters.
##'   
##' @export

fisheryStandardizationParameters =function(){
  
  geoVar = "geographicAreaM49_fi"
  yearVar = "timePointYears"
  itemVar = "ics"
  list(
    geoVar = geoVar,
    yearVar = yearVar,
    itemVar = itemVar,
    elementVar = "measuredElement",
    mergeKey = c(geoVar, yearVar, itemVar), # For merging with the main data
    childVar = "child",
    parentVar = "parent",
    extractVar = "extraction_rate",
    productionCode = "5515",
    importCode = "5610",
    exportCode = "5910",
    stockCode = "5071",
    foodCode = "5141",
    foodProcCode = "5023",
    feedCode = "5520",
    baitingCode = "5525",
    wasteCode = "5016",
    residualCode = "5166"
  )
}