##'processingComputeSimplified
##'
##' This function has been created to compute the food processing.
##' The main assumption is based on the idea that each processed and 
##' preserved items is produced from the corresponding primary item.
##' The 'food processing'(131) component is always obtained as the sum 
##' of all the input necessary to produce all its children, unless the
##' primary availablity results lower that the total input (sum of all
##' the children input).
##' 
##' If this is the case, the primary 'food processing' is set to compensate
##' the total availability (equal to current availability in order to balance
##' the line) and the surplus amount of 'food processing' necessary to
##' produce all the children is associated to the 'secondary parents' items.
##' 
##' Secondary parents are chosen looking at the past SUA tables. See the routine
##' 
##' 
##'  
##' @param data data.table datataset containing SUA 
##' @param tree data.table dataset containing the commodity tree
##' @return Returns a dataset enlarged with the foodProcessing 
##' 
##' @export
# 
# data <- SUA_avail3
#    tree<- tree

processingComputeNew <- function(data ,tree){
  
  ## the principal ingredient to compute the foodProcessing is the extraction rates
  ## for those items that already have a protected figures for 31 the default extractio rate 
  ## in the commodity tree has to be updated:
  
  ### compute eR where there are data for it (should be protected data)
  primary <- sort(unique(tree[ !parent %in% child, parent ]))
  data <- removeNonProtectedFlag(data)

  dataProd <- data[measuredElement=="51" & !is.na(Value)]
  dataInput <- data[measuredElement=="31" & !is.na(Value)] 

  computeExtractionRate <- merge(dataProd, dataInput, by=c("geographicAreaM49", "timePointYears", "ics"), suffixes = c("_prod","_input"))
  computeExtractionRate[, extraction_rate_C:= ifelse(is.na(Value_prod/Value_input), NA, Value_prod/Value_input) ]
  computeExtractionRate <- computeExtractionRate[,.(geographicAreaM49, timePointYears,  ics, extraction_rate_C)]
  
  
  setnames(computeExtractionRate,"ics","child")
  
  # Substitute values if different from older ones
  # and only consider primary parent as the availability chack is done first only for with primary disposable products
  tree <- merge(tree ,computeExtractionRate, by="child", allow.cartesian = TRUE, all.x = TRUE)
  tree[!is.na(extraction_rate_C) & parent %in% primary, extraction_rate:=extraction_rate_C]
  tree[,extraction_rate_C:=NULL]
  tree[, c("geographicAreaM49", "timePointYears") := NULL]
  
  ### Compute the input (element 31) for each child
  dataCopy <- copy(data)
  setkey(dataCopy)
  
  # Data with extraction rate
 data_withER <- merge(dataCopy, tree[parent %in% primary,], by.x = "ics", by.y = "child", 
                      allow.cartesian = TRUE, all.x = TRUE)
  
 # check that no extraction rate is present for primary code (otherwise error)

 for(i in 1:nrow(data_withER)){
  if(!is.na(data_withER$extraction_rate[i]) & data_withER$ics[i] %in% primary){
    print("Extraction rates should not exist for primary ICS codes!")
  }
 }
  
 # Transform to primary equivalent obtaining input (31)
 # Input = Production/eR
 
 InputValue <- data_withER[measuredElement == "51" & !is.na(Value) & !is.na(extraction_rate), input := Value/extraction_rate ]
 InputValue <- InputValue[!is.na(input), .(geographicAreaM49,
                                           timePointYears,
                                           parent,
                                           ics,
                                           measuredElement,
                                           Value,
                                           flagObservationStatus,
                                           flagMethod,
                                           availability,
                                           weight,
                                           input)]
 
 setnames(InputValue, old = "Value", new = "ProductionValue")
 
 InputValue$measuredElement <- "31"
 
 # Adding Input values to data

 data31 <- merge(data_withER[measuredElement != "31"], InputValue, by = c("geographicAreaM49", 
                                       "timePointYears", 
                                       "parent", 
                                       "ics",
                                       "measuredElement", 
                                       "flagObservationStatus",
                                       "flagMethod",
                                       "weight",
                                       "availability"), 
       all = TRUE )
 
 # If input was NA now the calculated one is inserted.
 data31[ measuredElement=="31" & is.na(Value), Value := round(input.y)]
 data31[ , c( "input.x", "input.y" , "extraction_rate", "ProductionValue") := NULL]
 setkey(data31)
 data31 <- data31[!duplicated(data31)]
 #data31 <- data31[ , c("parent") := NULL]
 
 data2 <- merge(data, tree[, 1:3, with = FALSE], by.x = "ics", by.y="child", all.x = TRUE, allow.cartesian = TRUE)
 
 data2 <- rbind(data2[measuredElement != "31"], data31[measuredElement == "31"])
 
 # Compute food processing
 # not including parallel product
 # Unique value for each primary

 data131 <- data2[ ,
   processing := sum(Value * weight, na.rm = TRUE), 
   by = c("geographicAreaM49", "parent", "timePointYears")]
 
 setkey(data131)
 data131 <- data131[!duplicated(data131)]
 #data131 <- unique(data131[,.(geographicAreaM49, timePointYears, ics, availability)])
 
 # Take only processing by parent, year and country (no other data)
  data131byparent <- data131[, .(geographicAreaM49, timePointYears, parent, processing)]
  data131byparent <- data131byparent[!is.na(processing)]
  data131byparent <- data131byparent[!duplicated(processing)]
  
  setnames(data131byparent, old = "parent", new = "ics")
 
  # proc_avail_tree <- merge(data131byparent, tree[ , .(parent, child)] , by = "parent", allow.cartesian = TRUE)
  # proc_avail_tree2 <- merge(proc_avail_tree, 
  #                           data131[ , .(geographicAreaM49, timePointYears, ics, availability)], 
  #                           by.y = c("geographicAreaM49", "timePointYears", "ics"), by.x = c("geographicAreaM49", "timePointYears","parent"))
  # setkey(proc_avail_tree2)
  # 
  # proc_avail_tree2 <- proc_avail_tree2[! duplicated(proc_avail_tree2)]
  # 

 
  ## The 'food processing' has been just computed for any Primary Parent, 
 ## we have to compare the 'new component' based on the sum of all the child-input
 ## with the actual primary availability, in order to be sure that the new availabily
 ## (computed including the just computed food processing in the primary SUA line) does not produce
 ## a negative unbalance.
 
  
  
 
 data_131avail <- merge(data2[ , .(geographicAreaM49,
                                    timePointYears,
                                    ics,
                                    measuredElement,
                                    flagObservationStatus,
                                    flagMethod,
                                    weight,
                                    availability,
                                    Value)],
                        data131byparent,
                        by = c("geographicAreaM49",
                                 "timePointYears",
                                 "ics"),
                        all.x = TRUE)
  
 data_131availPrim <- data_131avail[ ics %in% primary]
 data_131availPrim <- data_131availPrim[ , secondLevelProcessing:=availability-processing]
 
 data_131availPrim[secondLevelProcessing<0, processing:= processing + secondLevelProcessing ]
 
 secondLevelProcessing <- data_131availPrim[secondLevelProcessing<0]
 
 setnames(secondLevelProcessing, "ics", "parent")
 
 toDeviate <- merge(secondLevelProcessing,tree, by= "parent", allow.cartesian = TRUE)
 
 setnames(toDeviate, c("parent","child"), c("parent_primary","parent_secondary"))
 
 toDeviate <- toDeviate[,.(parent_secondary, geographicAreaM49, timePointYears, availability ,processing ,secondLevelProcessing)]
 secondary <- tree[!parent %in% primary, unique(parent)]
 toDeviate <- toDeviate[parent_secondary %in% secondary] # which child are also in parent column 
 
 setnames(tree, "child", "parent_secondary")
 toDeviate <- merge(toDeviate , tree, by= "parent_secondary", allow.cartesian = TRUE)
 
 # Transform quantity to deviate on secondary parent into secondary equivalent
 toDeviate[, secondEquivalentProcessing:= (secondLevelProcessing * extraction_rate)*(-1)]
 toDeviate <- toDeviate[,.(geographicAreaM49, parent_secondary, timePointYears, secondEquivalentProcessing, availability)]
 
 
 toDeviateMelted <- melt(toDeviate,
                 id.vars = colnames(toDeviate[, c(1:3, 5), with = FALSE]),
                 measure.vars = colnames(toDeviate[,4, with = FALSE ]),
                 value.name = "Value" ,
                 variable.name = "measuredElement", variable.factor = FALSE)
 
 
 toDeviateMelted[measuredElement=="secondEquivalentProcessing", measuredElement:="131"]
 setkey(toDeviateMelted)
 toDeviateMelted <- unique(toDeviateMelted)
 
 setnames(toDeviateMelted, "parent_secondary", "ics")
 
 
 # Create one data.table with processing at all levels
 
 data_compute131 <- melt(data_131availPrim,
                       id.vars = colnames(data_131availPrim[ , c(1:3,8), with = FALSE]),
                       measure.vars = "processing",
                       value.name = "Value" ,
                       variable.name = "measuredElement", variable.factor = FALSE)
 
 
 data_compute131[measuredElement=="processing", measuredElement:="131"]
 
 # Put together processing in primary and secondary equivalent
 
 data_compute131 <- rbind(data_compute131, toDeviateMelted )
 setkey(data_compute131)
 data_compute131 <- unique(data_compute131)
 
 # Take all data but those with food processing and add the part with the recalculated ones
 data3 <- data2[measuredElement!="131",]
 data3 <- data3[,.(geographicAreaM49,timePointYears,  measuredElement, ics, Value, availability) ]
 data <- rbind(data3, data_compute131)
 
 return(data)
 
 
 # # Take data with not NA availability
 # data_131avail <- data_131avail[!is.na(availability)]
 # setnames(data_131avail, old = "ics", new = "parent")
 # 
 # # Separating data with enough primary availability from those where second parents are needed
 # primaryAvail <- data_131avail[availability >= processing]
 # secondaryAvail <- data_131avail[availability < processing]
 # 
 # # Data with primary and secondary availability
 # data_avail12 <- merge(data_131avail, tree, by = "parent", allow.cartesian = TRUE)
 # 
 # setnames(data31, old = "ics", new = "child")
 # data_avail12 <- merge(data_avail12, data31, by = c("geographicAreaM49",
 #                                                    "timePointYears",
 #                                                    "measuredElement",
 #                                                    "parent",
 #                                                    "child",
 #                                                    "flagObservationStatus",
 #                                                    "flagMethod",
 #                                                    "FBSsign") )
 # 
 # data_avail12$Value.x == data_avail12$Value.y
 # 
 # 
 # 
 # ##----
 # 
 # 
 # for( parentcode in seq_len(primary) ) {
 #   for( year in seq_len(unique(data31$timePointYears)) ) {
 #     for( childcode in seq_len(unique(data31$ics[parent == parentcode])) ) {
 #       
 #       
 #       availableQ <- unique(data31[ics == primary[parentcode] & 
 #                                     timePointYears == unique(data31$timePointYears)[year], 
 #                                   availability])
 #       children <- data31[parent == primary[parentcode] & 
 #                            timePointYears == unique(data31$timePointYears)[year] & 
 #                            measuredElement == "31" &
 #                            !is.na(Value), Value
 #                          ]
 #       
 #       childcode <- 1
 #       stepwise_avail <- availableQ - children[childcode]
 #       
 #       while( stepwise_avail > 0){
 #         
 #         childcode <- childcode + 1
 #         stepwise_avail <- availableQ - children[1:childcode]
 #       }
 #       
 #       
 #     }
 #     
 #   }
 #   
 # }
 # 
 # if( data31[parent == "1501" & 
 #            ics == "1502"& 
 #            timePointYears =="2000"  & 
 #            measuredElement == "31" , 
 #            availability] - data31[parent == "1501" 
 #                                   & ics == "1502"& 
 #                                   timePointYears =="2000" & 
 #                                   measuredElement == "31" , 
 #                                   Value] > 0 ) {}

  
      
}

