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
    productionCode = "51",
    importCode = "61",
    exportCode = "91",
    stockCode = "71",
    foodCode = "141",
    foodProcCode = "131",
    feedCode = "101",
    baitingCode = "111",
    wasteCode = "121",
    residualCode = "151"
  )
}