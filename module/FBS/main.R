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

if(CheckDebug()){
  
  SETTINGS = ReadSettings("sws1.yml")
  
  ## If you're not on the system, your settings will overwrite any others
  R_SWS_SHARE_PATH = SETTINGS[["share"]]
  
  ## Define where your certificates are stored
  SetClientFiles(SETTINGS[["certdir"]])
  
  ## Get session information from SWS. Token must be obtained from web interface
  GetTestEnvironment(baseUrl = SETTINGS[["server"]],
                     token = SETTINGS[["token"]])
  
}

## Im still working on three pilot countries:
countries=c("300", "203" , "40")
test1=list()


for(j in seq_along(countries)) {
  
  currentCountry=countries[j]  
  
  keyDim=c("geographicAreaM49_fi", "fisheriesAsfis", "measuredElement", "timePointYears")
  
  KeyGlobal = DatasetKey(domain = "Fisheries", dataset = "fi_global_production", dimensions = list(
    #geographicAreaM49_fi = Dimension(name = "geographicAreaM49_fi", keys = GetCodeList("Fisgheries", "fi_global_production","geographicAreaM49_fi" )[,code]),
    geographicAreaM49_fi = Dimension(name = "geographicAreaM49_fi", keys = currentCountry),
    fisheriesAsfis = Dimension(name = "fisheriesAsfis", keys = GetCodeList("Fisheries", "fi_global_production","fisheriesAsfis" )[,code]),
    fisheriesCatchArea = Dimension(name = "fisheriesCatchArea", keys = GetCodeList("Fisgheries", "fi_global_production","fisheriesCatchArea" )[,code]),
    measuredElement = Dimension(name = "measuredElement", keys = c("FI_001")),
    #timePointYears = Dimension(name = "timePointYears", keys = GetCodeList("Fisgheries", "fi_global_production","timePointYears" )[,code] )
    timePointYears = Dimension(name = "timePointYears", keys = as.character(c(2000:2016) ))
  ))
  
  ##Get Global Production Data
  globalProduction=GetData(KeyGlobal)
  
  
  if(CheckDebug()){
    
    SETTINGS = ReadSettings("sws.yml")
    
    ## If you're not on the system, your settings will overwrite any others
    R_SWS_SHARE_PATH = SETTINGS[["share"]]
    
    ## Define where your certificates are stored
    SetClientFiles(SETTINGS[["certdir"]])
    
    ## Get session information from SWS. Token must be obtained from web interface
    GetTestEnvironment(baseUrl = SETTINGS[["server"]],
                       token = SETTINGS[["token"]])
    
  }
  
  
  globalProduction=globalProduction[,sum(Value, na.rm = TRUE), by=c("geographicAreaM49_fi",
                                                                    "fisheriesAsfis",
                                                                    "measuredElement",
                                                                    "timePointYears")]
  setnames(globalProduction, "V1", "Value")
  
  ## Work on flags: aggragate observationFlag!!
  #globalProduction[,flagObservationStatus:=""]
  #globalProduction[,flagMethod:="c"]
  
  #globalProduction=globalProduction[!duplicated(globalProduction)]
  mapping=ReadDatatable("fishery_item_mapping")
  primaryMapping=ReadDatatable("fishery_primary_mapping")
  
  #primaryMapping = fread("data/PrimaryProduction_mapping.csv")
  setnames(primaryMapping, "alphacode","fisheriesAsfis")
  
  globalProductionMapping = merge(
    globalProduction,
    primaryMapping,
    by = c( "fisheriesAsfis"),
    all.x = TRUE)
  
  ## Integrate this info in the data.table  fishery_primary_mapping
  primary = c("1501", "1514", "1527","1540","1553", "1562", "1579",  "1570", "1587")
  
  globalProductionMapping= as.data.table(inner_join(globalProductionMapping,
                                                    unique(mapping[ics %in%  primary,.(ics,isscaap)]),
                                                    by = "isscaap") )
  
  globalProductionMapping=globalProductionMapping[ics %in% primary]
  
  globalProductionMapping=globalProductionMapping[,.(geographicAreaM49_fi,
                                                     timePointYears,
                                                     ics,
                                                     measuredElement,
                                                     Value)]
  globalProductionMapping[, Value := sum(Value, na.rm = TRUE), by = list(geographicAreaM49_fi,
                                                                         timePointYears,
                                                                         measuredElement,
                                                                         ics)]
  
  globalProductionMapping = globalProductionMapping[!duplicated(globalProductionMapping)]
  globalProductionMapping=globalProductionMapping[!is.na(ics)]
  ############################################################################################################
  ##Get Commodity Database, still local files (no commodity )
  commodityDB_value = fread("data/commodityDB_Value.csv", header = TRUE)
  ## Re-shape the data in order to have the year-dimension in one column
  ## Ensure that the Value column is numeric and that the missing values are properly classified as NA
  ##Value
  commodityDB_value = melt(
    commodityDB_value,
    id.vars = colnames(commodityDB_value)[c(1:7)],
    measure.vars = colnames(commodityDB_value)[c(8:48)],
    variable.name = "timePointYears",
    value.name = "Value"
  )
  ## Inconsistency in temrs of flow codes, trade flows referring to Values have different codes
  commodityDB_value[, measuredElement := as.character(measuredElement)]
  commodityDB_value[measuredElement=="91", measuredElement:="92"]
  commodityDB_value[measuredElement=="61", measuredElement:="62"]
  commodityDB_value[measuredElement=="51", measuredElement:="52"]
  commodityDB_value[, Value := as.numeric(Value)]
  
  ##Quantity
  commodityDB_quantity = fread("data/commodityDB_Quantity.csv", header = TRUE)
  commodityDB_quantity = melt(
    commodityDB_quantity,
    id.vars = colnames(commodityDB_quantity)[c(1:7)],
    measure.vars = colnames(commodityDB_quantity)[c(8:48)],
    variable.name = "timePointYears",
    value.name = "Value"
  )
  
  commodityDB_quantity[,Value:=gsub("F","" ,Value)]
  
  commodityDB_quantity[, Value := as.numeric(Value)]
  commodityDB_quantity[, measuredElement := as.character(measuredElement)]
  
  ##############################################################################################################
  commodityDB = rbind(commodityDB_value, commodityDB_quantity)
  commodityDB=commodityDB[Country_code==currentCountry]
  ##############################################################################################################
  
  ## Aggregate the ISSCFC_Code according to the mapping on the ICS classification
  ##Commodity Database
  ## First aggregation:
  ## map the ISSCFC_Code on the ICS_Code
  commodityDB[,.(Country_code, ISSCFC_Code, measuredElement, timePointYears,Value)]
  setnames(commodityDB, "ISSCFC_Code", "isscfc")
  commodityDB_aggregation = merge(commodityDB, mapping, by = "isscfc", all.x = TRUE)
  commodityDB_aggregation = commodityDB_aggregation[!is.na(Value)]
  
  sua_commodityDB = commodityDB_aggregation[, .(Country_code,
                                                ics,
                                                Trade_flow,
                                                measuredElement,
                                                timePointYears,
                                                Value)]
  
  sua_commodityDB[, Value := sum(Value, na.rm = TRUE), by = list(Country_code,
                                                                 ics,
                                                                 measuredElement,
                                                                 timePointYears)]
  
  sua_commodityDB = sua_commodityDB[!duplicated(sua_commodityDB)]
  sua_commodityDB=sua_commodityDB[,.(Country_code,
                                     ics,
                                     measuredElement,
                                     timePointYears,
                                     Value)]
  #########################################################################################################################
  
  link=ReadDatatable("link_mapping")
  link=link[geographic_area_m49==currentCountry]
  link=(unique(link))
  if(nrow(link)>0){
    
    lastYear=max(as.numeric(as.character(commodityDB[,timePointYears])))
    
    all=c("PRD", "IMP", "EXP")
    all=data.table(flow=rep("ALL", length(all)),all=all)
    link_all=merge(link, all, by="flow")
    link_all[,flow:=all]
    link_all[,all:=NULL]
    
    trade=c( "IMP", "EXP")
    trade=data.table(flow=rep("TRD", length(trade)),trade=trade)
    link_trade= merge(link, trade, by="flow", allow.cartesian = TRUE)
    link_trade[,flow:=trade]
    link_trade[,trade:=NULL]
    
    link=link[!flow %in% c("ALL", "TRD"),]
    link=rbind(link,link_trade,link_all)
    
    link[end_year=="LAST", end_year:=lastYear]
    link[,start_year:=as.numeric(as.character(start_year))]
    link[,end_year:=as.numeric(as.character(end_year))]
    
    link[,nYear:=(end_year-start_year)+1]
    linkNew=list()
    
    for(i in 1:dim(link)[1]){
      timePointYears=data.table(timePointYears=c(link[i,start_year]:link[i,end_year]))
      iter= link[i]
      iter= data.table(apply(iter, 2, rep, iter[,nYear]))
      linkNew[[i]]=data.table(iter, timePointYears=c(unique(iter[1,start_year]):unique(iter[1,end_year])) )
    }
    linkNew=rbindlist(linkNew)
    
    linkNew[,start_year:=NULL]
    linkNew[,nYear:=NULL]
    linkNew[,to_code:=NULL]
    linkNew[,start_year:=NULL]
    linkNew[,end_year:=NULL]
    
    setnames(linkNew, "from_code","ics")
    linkNew[flow=="IMP",flow:="61"]
    linkNew[flow=="EXP",flow:="91"]
    linkNew[flow=="PRD",flow:="51"]
    
    ### Modify the ICS_Code according to the linkNew file:
    setnames(linkNew,"geographic_area_m49" ,"Country_code")
    setnames(linkNew,"flow" ,"measuredElement")
    
    sua_commodityDB[,Country_code:=as.character(Country_code)]
    sua_commodityDB[,timePointYears:=as.integer(as.character(timePointYears) )]
    
    sua_commodityDB=merge(sua_commodityDB, linkNew, by=c("Country_code","timePointYears", "measuredElement","ics"), all.x = TRUE)
    sua_commodityDB[, percentage:=as.numeric(percentage)]
    sua_commodityDB[!is.na(percentage), Value:=Value*percentage]
    sua_commodityDB[, percentage:=NULL]
  }
  
  setnames(sua_commodityDB,c("Country_code"), c("geographicAreaM49_fi"))
  sua_commodityDB=sua_commodityDB[,.(geographicAreaM49_fi,
                                     measuredElement,
                                     ics,
                                     timePointYears,
                                     Value)]
  
  
  SUA=rbind(sua_commodityDB, globalProductionMapping)
  SUA[measuredElement=="FI_001", measuredElement:="51"]
  
  ### Step 1. balance the equation if the imbalance<0 (add the imbalance to the PROD)
  ### I do not have obs and method flag columns (I add fake 2 columns)
  ### This problem will be directly solved when I 
  
  ##SUA[,flagObservationStatus:=""]
  ##SUA[,flagMethod:="q"]
  
  #########################################################################################################################
  ##Expand the SUA 
  key = c("timePointYears", "geographicAreaM49_fi",  "ics")
  keyDataFrame = SUA[, key, with = FALSE]
  keyDataFrame=keyDataFrame[with(keyDataFrame, order(get(key)))]
  keyDataFrame=keyDataFrame[!duplicated(keyDataFrame)]
  elDataFrame = c("51","61","91","71","111","121", "141", "151", "131")
  ##elDataFrame = c("51","61","91","141")
  #elDataFrame = unique(SUA[,.(measuredElement)])
  elDataFrame=data.table(elVar=elDataFrame)
  colnames(elDataFrame) = "measuredElement"
  
  completeBasis =  data.table(merge.data.frame(keyDataFrame, elDataFrame))
  expandedData = merge(completeBasis, SUA, by = colnames(completeBasis), all.x = TRUE)
  expandedData = fillRecord(expandedData)
  # expandedData[is.na(flagObservationStatus), flagObservationStatus:="M"]
  # expandedData[is.na(flagObservationStatus), flagMethod:="u"]
  SUA=expandedData
  
  SUA=SUA[timePointYears %in% c(2000:2016)]
  #########################################################################################################################
  ## Compute the imbalance==availability
  
  SUA[, availability:=sum(ifelse(is.na(Value),0,Value)* 
                            ifelse(measuredElement=="51", 1,
                                   ifelse(measuredElement=="61",1,
                                          ifelse(measuredElement=="91", -1, 0)))), 
      by=c("geographicAreaM49_fi","ics",  "timePointYears")]
  
  
  
  ## Production can be touched only in case of processed items. The PRIMARY negative availabilities at this stage
  ## MUST be properly taken into account.
  ## In case we identify (in table toBeChecked) some primary negative availabilities, Send Email and STOP the process:
  
  toBeChecked=SUA[ics %in% primary & availability<0]
  ## if(!CheckDebug() &  nrow(toBeCheck)>0){
  ##   sendmailR::sendmail("jjjjj")
  ##   stop("There are negative primary availabilities, please check your mailbox!")
  ##   
  ## }else{
  
  
  ## Balance the line characterized by negative availabilities increasing the PRODUCTION:
  ## Before sending all the negative imbalance to Production, we should deviate a small portion of the TOT imbalance
  ## to stockVariation. This meand that stock is a supply-component and we have to chack that the sum of the series of stock variation 
  ## is still positive (or equal to zero)
  
  
  
  SUA[measuredElement=="51" & availability<0   ,
      Value:=ifelse(is.na(Value), -availability, Value-availability)]
  
  ### Compute again the imbalance==availability
  SUA[, availability:=sum(ifelse(is.na(Value),0,Value)* 
                            ifelse(measuredElement=="51", 1,
                                   ifelse(measuredElement=="61",1,
                                          ifelse(measuredElement=="91", -1, 0)))), 
      by=c("geographicAreaM49_fi","ics",  "timePointYears")]
  ### Step 2. Compute Food Processing 
  ### The Tree structure is quite strange, each processed product can be produced at many different processing levels
  ### according to the availabilty of the primary, secondary,... For example a canned can be produced starting
  ### from fresh fish (if it is available) but also from frozen fish (which might have been imported)
  commodityTree = ReadDatatable("fisheries_commodity_tree")
  lev=findProcessingLevel(commodityTree, from="parent", to="child", aupusParam = list(itemVar="parent"))
  commodityTreeLev0=commodityTree[parent %in% lev[processingLevel==0, parent],]
  
  
  treeGreece=fread("data/TreeCountrySpecific/tree_300.csv")
  
  SUA = processingCompute(SUA,treeGreece)
  
  
  
  ### Compute again the Availability including Food Processing in the equation
  SUA[, availability:=sum(ifelse(is.na(Value),0,Value)* 
                            ifelse(measuredElement=="51", 1,
                                   ifelse(measuredElement=="61",1,
                                          ifelse(measuredElement=="91", -1,
                                                 ifelse(measuredElement=="131", -1,
                                                        ifelse(measuredElement=="71", -1, 0) ))))), 
      by=c("geographicAreaM49_fi","ics",  "timePointYears")]
  
  
  
  ### What if the availability becomes negative after we have added the "processing component"?
  ### 1) It is possible that some derived items do not come directly from primary production, 
  ###    but from some altready derived items that are furtherly processed.
  
  ### In the current situation it is IMPOSSIBLE to have negative availabilities for ICS are code 
  ### idenfifyng processed items.
  
  #SUA_Czechia=SUA[geographicAreaM49_fi==currentCountry]
  
  secondRound=list()
  
  for(i in seq_along(SUA[, unique(timePointYears)]) ){
    
    currentY=SUA[, unique(timePointYears)][i]
    itemP=SUA[timePointYears==currentY & availability<0, unique(ics)]
    itemC=commodityTree[parent %in% itemP, unique(child)]
    itemC=SUA[timePointYears==currentY & ics %in%itemC,unique( ics)]
    secondRound[[i]]= SUA[timePointYears==currentY & ics %in% c(itemP, itemC),]
    
  }
  
 
  secondRound = rbindlist(secondRound)
  
 
  
  secondRound[measuredElement=="131",Value:=NA]
  
  
  secondRound[, availability:=sum(ifelse(is.na(Value),0,Value)* 
                                    ifelse(measuredElement=="51", 1,
                                           ifelse(measuredElement=="61",1,
                                                  ifelse(measuredElement=="91", -1,
                                                         ifelse(measuredElement=="131", -1,
                                                                ifelse(measuredElement=="71", -1, 0) ))))), 
              by=c("geographicAreaM49_fi","ics",  "timePointYears")]
  
  
  
  commodityTreeSecond=commodityTree[,.N, by=child]
  lev=findProcessingLevel(commodityTree, "parent", "child")
  setnames(lev, "temp", "parent")
  
  commodityTreeSecond=merge(commodityTreeSecond, commodityTree, by="child")
  commodityTreeSecond =merge(commodityTreeSecond, lev, by="parent")
  tt=commodityTreeSecond[!(processingLevel==0 & N!=1),]
  commodityTreeSL=tt[,.(parent, child, extraction_rate, weight)]
  
  #########################################################################################################
  # Second round
  #########################################################################################################
  
  SUA_secondRound = processingCompute(secondRound, commodityTreeSL) 
  
  ### Compute again the Availability including Food Processing in the equation
  
  SUA_secondRound[, availability:=sum(ifelse(is.na(Value),0,Value)* 
                                        ifelse(measuredElement=="51", 1,
                                               ifelse(measuredElement=="61",1,
                                                      ifelse(measuredElement=="91", -1,
                                                             ifelse(measuredElement=="131", -1,
                                                                    ifelse(measuredElement=="71", -1, 0) ))))), 
                  by=c("geographicAreaM49_fi","ics",  "timePointYears")]
  ######################################################################################################
  
  
  toExclude=SUA_secondRound[,.(timePointYears, geographicAreaM49_fi , ics, measuredElement )]
  SUA=SUA[!toExclude,, on=.(timePointYears, geographicAreaM49_fi , ics, measuredElement )]
  
  final=rbind(SUA, SUA_secondRound)
  
  
  final[, availability:=sum(ifelse(is.na(Value),0,Value)* 
                                        ifelse(measuredElement=="51", 1,
                                               ifelse(measuredElement=="61",1,
                                                      ifelse(measuredElement=="91", -1,
                                                             ifelse(measuredElement=="131", -1,
                                                                    ifelse(measuredElement=="71", -1, 0) ))))), 
        by=c("geographicAreaM49_fi","ics",  "timePointYears")]
 
  
  
  
  
  
  ## Send data to SWS:
  SUA_unbalaced=copy(final)
  SUA_unbalaced[,availability:=NULL]
  
  SUA_unbalaced[measuredElement=="51", measuredElement:="5510"]
  SUA_unbalaced[measuredElement=="61", measuredElement:="5610"]
  SUA_unbalaced[measuredElement=="91", measuredElement:="5910"]
  SUA_unbalaced[measuredElement=="131", measuredElement:="5023"]
  SUA_unbalaced[measuredElement=="71", measuredElement:="5071"]

  SUA_unbalaced[, measuredElement:=as.character(measuredElement)]
  
  
  SUA_unbalaced[,flagObservationStatus:="T"]
  SUA_unbalaced[,flagMethod:="s"]
  
  setnames(SUA_unbalaced, "ics", "fisheriesAsfis")
  
  postProcessing(data =  SUA_unbalaced, normalised=TRUE) 
  
  SUA_unbalaced[, `:=`("timePointYears", as.character(.SD[["timePointYears"]]))]
  SUA_unbalaced[is.na(Value), Value:=0]
  
 
  SaveData("Fisheries", "fisheries_sua_unbalanced", SUA_unbalaced)  
  
  
  
  
   ######################################################################################################
  #  Allocate the positive imbalance to the possible utilizations:
  #  This balancing is trivial and NOT correct
    final[measuredElement=="141" & availability>0, Value:=availability]
  
  ### Compute again the Availability including Food Component in the equation
  
  final[, availability:=sum(ifelse(is.na(Value),0,Value)* 
                                        ifelse(measuredElement=="51", 1,
                                               ifelse(measuredElement=="61",1,
                                                      ifelse(measuredElement=="91", -1,
                                                             ifelse(measuredElement=="131", -1,
                                                                    ifelse(measuredElement=="141",-1,
                                                                           ifelse(measuredElement=="71", -1, 0) ))))) ), 
                            by=c("geographicAreaM49_fi","ics",  "timePointYears")]
  
  #######################################################################################################
  ######################################################################################################
  # ## Add NutrientFactors
    nutrientFactors=ReadDatatable("fishery_nutrient")
 ## final=merge(final, nutrientFactors, by="ics", all.x = TRUE)
 ## final[measuredElement=="141", calories:=Value*calories]
 ## final[measuredElement=="141", proteins:=Value*proteins]
 ## final[measuredElement=="141", fats:=Value*fats]
 ## final[measuredElement!="141",`:=`(c("calories", "proteins", "fats"),list(0,0,0) )]
  
  # 
  # final, nutrientFactors
  # 
  # ######################################################################################################

  
  ##write.csv(final,paste0("C:/Users/ROSA/Desktop/Fisheries/batch0_SUA_pilotCounties/",currentCountry,".csv"), row.names = FALSE)
  
  
  ## Set the parameters to perform the function "standardizeTree"
  fisheryParams = fisheryStandardizationParameters()
  zeroWeight=commodityTree[weight==0, unique(child)]
  
  out=list()
  out_nutrient=list()
  additiveElements=c("calories", "proteins", "fats")
  
  for(t in seq_along(unique(final$timePointYears)) ){
    currentY=unique(final$timePointYears)[t]
    final_time=final[timePointYears==currentY]
    
    
    final_time[measuredElement=="51" & (!ics %in% primary), Value:=0]
    final_time=final_time[measuredElement!="131"]
    final_time=final_time[measuredElement!="31"]
    current = final_time[, fisheryStandardizeTree(data = .SD, tree = commodityTreeLev0,
                                                   standParams = fisheryParams, elements = "Value", zeroWeight= zeroWeight),
                          by = c(fisheryParams$elementVar)]
    
    
    
    final_time=merge(final_time, nutrientFactors, by="ics", all.x = TRUE)
    final_time[measuredElement=="141", calories:=Value*calories]
    final_time[measuredElement=="141", proteins:=Value*proteins]
    final_time[measuredElement=="141", fats:=Value*fats]
    final_time[measuredElement!="141",`:=`(c("calories", "proteins", "fats"),list(0,0,0) )]
    final_time[is.na(calories), calories:=0]
    final_time[is.na(proteins), proteins:=0]
    final_time[is.na(fats), fats:=0]
  
  #  The following code has to be re-adapted to standardize at primary level the additive elements
   
   if(length(additiveElements) > 0){
     additiveTree = copy(commodityTreeLev0)
     additiveTree[, c(fisheryParams$extractVar) := 1]
     nutrients = lapply(additiveElements, function(nutrient){
       temp = final_time[get(fisheryParams$elementVar) == fisheryParams$foodCode,
                      fisheryStandardizeTree(data = .SD, tree = additiveTree,
                                   standParams = fisheryParams, elements = nutrient )]
       temp[, Value := get(nutrient)]
       temp[, c(fisheryParams$elementVar) := nutrient]
       temp[, c(nutrient) := NULL]
       temp
     })
     fromProcessed= rbind(current, do.call("rbind", nutrients))
  
     fromPrimary=final_time[ics %in% primary]
     fromPrimary[,availability:=NULL]
     
     
    primaryNutrients= melt.data.table(
       data = fromPrimary, measure.vars = c("calories", "proteins","fats"),
       id.vars = c("geographicAreaM49_fi", "timePointYears", "ics"),
       variable.name = "measuredElement", value.name = "Value")
    primaryNutrients=primaryNutrients[Value!=0]
    fromPrimary[,calories:=NULL] 
    fromPrimary[,proteins:=NULL] 
    fromPrimary[,fats:=NULL] 
    
    primaryEq=rbind(fromProcessed, fromPrimary, primaryNutrients)
    primaryEq=primaryEq[,sum(Value, na.rm = TRUE), by=c(fisheryParams$mergeKey, "measuredElement")]
    setnames(primaryEq, "V1", "Value")
    
    primaryEq=primaryEq[ics %in% primary]
    
    out[[t]]=primaryEq
     
     }
  
    
  }
  
  
  
  out=rbindlist(out)

  out[, availability:=sum(ifelse(is.na(Value),0,Value)* 
                              ifelse(measuredElement=="51", 1,
                                     ifelse(measuredElement=="61",1,
                                            ifelse(measuredElement=="91", -1,
                                                   ifelse(measuredElement=="131", -1,
                                                          ifelse(measuredElement=="141",-1,0)))))), 
        by=c("geographicAreaM49_fi","ics",  "timePointYears")]
  
  
}