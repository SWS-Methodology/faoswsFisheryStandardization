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

if(CheckDebug()){
  
  SETTINGS = ReadSettings("module/populateGlobalProd/sws.yml")
  
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
    fisheriesAsfis = Dimension(name = "fisheriesAsfis", keys = GetCodeList("Fisgheries", "fi_global_production","fisheriesAsfis" )[,code]),
    fisheriesCatchArea = Dimension(name = "fisheriesCatchArea", keys = GetCodeList("Fisgheries", "fi_global_production","fisheriesCatchArea" )[,code]),
    measuredElement = Dimension(name = "measuredElement", keys = c("FI_001")),
    timePointYears = Dimension(name = "timePointYears", keys = GetCodeList("Fisgheries", "fi_global_production","timePointYears" )[,code] )
  ))
  
  ##Get Global Production Data
  globalProduction=GetData(KeyGlobal)
  
  globalProduction=globalProduction[,sum(Value, na.rm = TRUE), by=c("geographicAreaM49_fi",
                                                                    "fisheriesAsfis",
                                                                    "measuredElement",
                                                                    "timePointYears")]
  setnames(globalProduction, "V1", "Value")
  
  ## Work on flags: aggragate observationFlag!!
  globalProduction[,flagObservationStatus:=""]
  globalProduction[,flagMethod:="c"]
  
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
  commodityDB_value = fread("data/Greece/commodityDB_Value.csv", header = TRUE)
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
  commodityDB_quantity = fread("data/Greece/commodityDB_Quantity.csv", header = TRUE)
  commodityDB_quantity = melt(
    commodityDB_quantity,
    id.vars = colnames(commodityDB_quantity)[c(1:7)],
    measure.vars = colnames(commodityDB_quantity)[c(8:48)],
    variable.name = "timePointYears",
    value.name = "Value"
  )
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
  
  SUA[,flagObservationStatus:=""]
  SUA[,flagMethod:="q"]
  
  #########################################################################################################################
  ##Expand the SUA 
  key = c("timePointYears", "geographicAreaM49_fi",  "ics")
  keyDataFrame = SUA[, key, with = FALSE]
  keyDataFrame=keyDataFrame[with(keyDataFrame, order(get(key)))]
  keyDataFrame=keyDataFrame[!duplicated(keyDataFrame)]
  elDataFrame = unique(SUA[,.(measuredElement)])
  elDataFrame=data.table(elVar=elDataFrame)
  colnames(elDataFrame) = "measuredElement"
  
  completeBasis =  data.table(merge.data.frame(keyDataFrame, elDataFrame))
  expandedData = merge(completeBasis, SUA, by = colnames(completeBasis), all.x = TRUE)
  expandedData = fillRecord(expandedData)
  expandedData[is.na(flagObservationStatus), flagObservationStatus:="M"]
  expandedData[is.na(flagObservationStatus), flagMethod:="u"]
  SUA=expandedData
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
  
  
  ### Compute the ShareDownUp
  ### processingLevel = findProcessingLevel(commodityTree, from = "parent", to = "child")
  SUA1=copy(SUA)
  #keep only the availability which is important to compute the ShareDownUp
  SUA1=unique(SUA1[,.(geographicAreaM49_fi, timePointYears, ics,availability)])
  setnames(SUA1,"ics" ,"parent")
  SUA1[, parent:=as.character(parent)]
  SUA1=merge(SUA1, commodityTreeLev0, by="parent", allow.cartesian = TRUE)
  
  #### We are currently working with commodityTree0 this means that all weights are 1.
  #SUA1[, avChildEq:=availability*extraction_rate]
  #SUA1[, shareDownUpDEN:=sum(avChildEq, na.rm = TRUE), by=c("child", "timePointYears", "geographicAreaM49_fi") ]
  #
  #SUA1[, shareDownUp:=avChildEq/shareDownUpDEN ]
  #SUA1[, child:=as.character(child)] 
  
  SUA2=copy(SUA)
  
  setnames(SUA2, "ics", "child")
  SUA2[, child:=as.character(child)]
  SUA2=SUA2[measuredElement=="51",.(geographicAreaM49_fi,child, measuredElement, timePointYears,Value)]
  
  
  SUA_processing=merge(SUA1,  SUA2,
                       by= c("geographicAreaM49_fi" ,"child", "timePointYears"),
                       suffixes = c("_parent", "_child") )
  
  SUA_processing[, foodProcessing:=(Value*weight)/extraction_rate]
  
  SUA_processing[,foodProcessing:=sum(foodProcessing, na.rm = TRUE), by=c("geographicAreaM49_fi",
                                                                          "timePointYears",
                                                                          "parent")]
  
  SUA_processing=SUA_processing[,c("geographicAreaM49_fi",
                                   "timePointYears",
                                   "parent","foodProcessing"), with=FALSE]
  
  SUA_processing=unique(SUA_processing )
  SUA_processing[,measuredElement:="131"]
  setnames(SUA_processing, c("parent", "foodProcessing"), c("ics", "Value"))
  
  SUA[,availability:=NULL]
  SUA[,flagObservationStatus:=NULL]
  SUA[,flagMethod:=NULL]
  
  SUA=rbind(SUA, SUA_processing)
  
  ### Compute again the Availability including Food Processing in the equation
  SUA[, availability:=sum(ifelse(is.na(Value),0,Value)* 
                            ifelse(measuredElement=="51", 1,
                                   ifelse(measuredElement=="61",1,
                                          ifelse(measuredElement=="91", -1,
                                                 ifelse(measuredElement=="131", -1,0))))), 
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
  secondRound=secondRound[measuredElement!="131"]
  
  secondRound[measuredElement=="131", Value:=NA]
  secondRound[, availability:=sum(ifelse(is.na(Value),0,Value)* 
                                    ifelse(measuredElement=="51", 1,
                                           ifelse(measuredElement=="61",1,
                                                  ifelse(measuredElement=="91", -1,
                                                         ifelse(measuredElement=="131", -1,0))))), 
              by=c("geographicAreaM49_fi","ics",  "timePointYears")]
  
  #########################################################################################################
  # Second round
  #########################################################################################################
  
  SUA1_2=unique(secondRound[,.(geographicAreaM49_fi, timePointYears, ics,availability)])
  
  setnames(SUA1_2,"ics" ,"parent")
  SUA1_2[, parent:=as.character(parent)]
  SUA1_2=merge(SUA1_2, commodityTree, by="parent", allow.cartesian = TRUE)
  
  ### We are currently working with commodityTree0 this means that all weights are 1.
  SUA1_2[, avChildEq:=availability*extraction_rate]
  SUA1_2[, shareDownUpDEN:=sum(avChildEq, na.rm = TRUE), by=c("child", "timePointYears", "geographicAreaM49_fi") ]
  
  SUA1_2[, shareDownUp:=avChildEq/shareDownUpDEN ]
  SUA1_2[, child:=as.character(child)] 
  
  SUA2_2=copy(secondRound)
  
  setnames(SUA2_2, "ics", "child")
  SUA2_2[, child:=as.character(child)]
  SUA2_2=SUA2_2[measuredElement=="51",.(geographicAreaM49_fi,child, measuredElement, timePointYears,Value)]
  
  
  SUA_processing=merge(SUA1_2,  SUA2_2,
                       by= c("geographicAreaM49_fi" ,"child", "timePointYears"),
                       suffixes = c("_parent", "_child") )
  
  SUA_processing[, foodProcessing:=(Value*shareDownUp*weight)/extraction_rate]
  
  SUA_processing[,foodProcessing:=sum(foodProcessing, na.rm = TRUE), by=c("geographicAreaM49_fi",
                                                                          "timePointYears",
                                                                          "parent")]
  
  SUA_processing=SUA_processing[,c("geographicAreaM49_fi",
                                   "timePointYears",
                                   "parent","foodProcessing"), with=FALSE]
  
  
  SUA_processing=unique(SUA_processing )
  
  SUA_processing[,measuredElement:="131"]
  setnames(SUA_processing, c("parent", "foodProcessing"), c("ics", "Value"))
  
  #SUA_secondRound[,flagObservationStatus:=NULL]
  #SUA[,flagMethod:=NULL]
  secondRound[, availability:=NULL]
  SUA_secondRound=rbind(secondRound, SUA_processing)
  
  ### Compute ahain the Availability including Food Processing in the equation
  
  SUA_secondRound[, availability:=sum(ifelse(is.na(Value),0,Value)* 
                                        ifelse(measuredElement=="51", 1,
                                               ifelse(measuredElement=="61",1,
                                                      ifelse(measuredElement=="91", -1,
                                                             ifelse(measuredElement=="131", -1,0))))), 
                  by=c("geographicAreaM49_fi","ics",  "timePointYears")]
  ######################################################################################################
  
  
  toExclude=SUA_secondRound[,.(timePointYears, geographicAreaM49_fi , ics, measuredElement )]
  SUA=SUA[!toExclude,, on=.(timePointYears, geographicAreaM49_fi , ics, measuredElement )]
  
  final=rbind(SUA, SUA_secondRound)
  
  write.csv(final,paste0("C:/Users/ROSA/Desktop/Fisheries/batch0_SUA_pilotCounties/",currentCountry,".csv"), row.names = FALSE)
  
  
}