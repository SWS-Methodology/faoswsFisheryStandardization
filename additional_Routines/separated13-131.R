#-- Extraction rates calculation ----

eRcomputation <- function(data, tree){
  
  
  ## the principal ingredient to compute the foodProcessing is the extraction rates
  ## for those items that already have a protected figures for 31 the default extractio rate 
  ## in the commodity tree has to be updated:
  
  dataProd <- data[measuredElement=="5510"]
  dataInput <- data[measuredElement=="5302"]
  # # get flags from input data with correspondent rows
  # flagsInput <- combineFlag(data = dataInput, flagObservationStatusVar = flagObservationStatus, flagMethodVar = flagMethod)
  # flagsInput <- data.table(flag = flagsInput, row = 1:nrow(flagsInput))
  # # Only keep protected flags
  # flagsProt <- flagsInput[flagsInput %in% getProtectedFlag() ]
  # 
  # # only keep input data with protected data otherwise take tree extraction rates
  # dataInput <- dataInput[as.vector(flagsProt$row), ]
  
  if(nrow(dataInput) > 0 ){
    computeExtractionRate <- merge(dataProd, dataInput, by = c("geographicAreaM49_fi", "timePointYears", "ics"), suffixes = c("_prod","_input"))
    computeExtractionRate[timePointYears == max(as.numeric(data$timePointYears)) , extraction_rate_C := Value_prod/Value_input]
    computeExtractionRate <- computeExtractionRate[, .(ics, extraction_rate_C)]
    setnames(computeExtractionRate,"ics","child")
    computeExtractionRate <- unique(computeExtractionRate[ , extraction_rate_C := mean(extraction_rate_C), by = list(child)])
    
    
    # compare eR substitute calculated one only if parent is primary 
    # when secondary parent different as input expressed in primary equivalent
    # If the extraction rate is calculated from protected data then this one is kept in the tree
    treeNewER <- merge(tree ,computeExtractionRate, by=c("child"), all.x = TRUE) # by=c("timePointYears",  "child" ) from Francesca
    treeNewER[!is.na(extraction_rate_C) & parent %in% primary, extraction_rate:=extraction_rate_C]
    treeNewER[,c("extraction_rate_C"):=NULL]
    treeNewER <- treeNewER[!is.na(child)]
  } else {
    
    treeNewER <- tree
    
  }
    return(treeNewER)
  }
  
#-- Input calculation ----

  inputComputation <- function(data, treeNewER){
  ### Compute the input (element 31) for each children
  data1 <- copy(data)
  # keep only the availability which is important to compute the ShareDownUp
  setkey(data1)
  data1 <- unique(data1[,.(geographicAreaM49_fi, timePointYears, ics, measuredElement,Value)])
  setnames(data1, c("Value", "measuredElement", "ics"), 
           c("Value_child", "measuredElement_child" ,"child"))
  
  ## I consider that the only commodity that may be play the role of parent are the PRIMARY
  treePrimary <- treeNewER[parent %in%  primary]
  treePrimary[, child:=as.character(child)]
  
  # calculate input
  data1 <- merge(data1, treePrimary, by=c("child"), allow.cartesian = TRUE)
  data1[measuredElement_child=="5510", input:=Value_child/extraction_rate]
  
  # isolate the input element
  data_compute31= melt(data1,
                       id.vars = c("geographicAreaM49_fi", "timePointYears", "child"),
                       measure.vars = "input",
                       value.name = "Value" ,
                       variable.name = "measuredElement", variable.factor = FALSE)
  
  
  # set input == '5302'
  data_compute31[measuredElement=="input",measuredElement:="5302"]
  setnames(data_compute31, "child", "ics")
  data_compute31 <- data_compute31[!is.na(Value)]
  data_compute31[ , ':='(availability = NA, flagObservationStatus = 'E', flagMethod = 'i', FBSsign = 0)]
  
  # Remove all input data from the original data and add the only data31 part
  # existing data are included as computed with computed extraction rates
  # other input data are computed starting from given extraction rates
  dataNo31 <- data[measuredElement!="5302"]
  SUAinput <- rbind(dataNo31, data_compute31[,.(geographicAreaM49_fi, timePointYears,  ics, availability, measuredElement, Value)]) #rbind(data, data_compute31) #
  return(SUAinput)
}

#-- Food processing calculation ----
  
foodProcessingComputation <- function(SUAinput, treeNewER){
  
  treePrimary <- treeNewER[parent %in%  primary]
  treePrimary[, child:=as.character(child)]
  
  data_compute131 <- copy(SUAinput)
  setnames(data_compute131, "ics", "child")
  data_compute131 <- merge(data_compute131, treePrimary, by=c("child"))
  
  # sum all the inputs
  data_compute131[ measuredElement=="5302" , processing:=sum(Value, na.rm = TRUE), by=c("geographicAreaM49_fi",
                                                                                        "timePointYears",
                                                                                        "parent")]
  setkey(data_compute131)
  
  # processing is unique for each primary parent so take only these values
  
  data_compute131processing <- unique(data_compute131[!is.na(processing),.(geographicAreaM49_fi, parent,timePointYears, processing)])
  setnames(data_compute131processing, "parent", "ics")
  
  ## The 'food processing' has been just computed for any Primary Parent, 
  ## we have to compare the 'new component' based on the sum of all the child-input
  ## with the actual primary availability, in order to be sure that the new availabily
  ## (computed including the just computed food processing in the primary SUA line) does not produce
  ## a negative unbalance.
  
  data131 <- merge(SUAinput, data_compute131processing, by=c( "geographicAreaM49_fi","ics","timePointYears"), all.x = TRUE)
  setkey(data131)
  # take only the processing to check availability 
  data131 <- unique(data131[!is.na(processing),.(geographicAreaM49_fi, ics, timePointYears, availability, processing)])
  
  ## SeconLevelProcessing variable is computed to evaluate which primary availabilities are lower than the 
  ## food processing.
  data131[, secondLevelProcessing := availability-processing]
  
  # Lower the processing for the primary parent if availability is lower than processing 
  data131[secondLevelProcessing<0, processing:=processing + secondLevelProcessing ]
  
  # Primary ics from which not enough availability to cover processing
  secondLevelProcessing <- data131[secondLevelProcessing<0]
  setnames(secondLevelProcessing, "ics", "parent")
  
  # merge by parent so that secondary parents can be found
  # toDeviate has dimensions: (parent_primary, geographicAreaM49_fi, timePointYears,
  #   availability, processing, secondLevelProcessing, parent_secondary, weight, extraction_rate, rank)
  
  toDeviate <- merge(secondLevelProcessing,treeNewER[weight != FALSE], by=c("parent"), allow.cartesian = TRUE)
  setnames(toDeviate, c("parent","child"), c("parent_primary","parent_secondary"))
  
  # # tree only with possible secondary parents
  # secondary <- treeNewER[!parent %in% primary, unique(parent)]
  # 
  # # Keep a child only if it can also be a (secondary) parent
  # toDeviate <- toDeviate[parent_secondary %in% secondary]
  
  #########################
  
  # serve vedere input del secondo da SUAinput
  # comparare disposizione del secondo parent
  # 
  toDeviate2proc <- merge(toDeviate, SUAinput, by.x = c("geographicAreaM49_fi", "timePointYears",  "parent_secondary"),
                          by.y = c("geographicAreaM49_fi", "timePointYears",  "ics"), suffixes = c('_primary','_secondary'))
  
  # Make sure primary availability covers at least the rank 1 child
  
  rank1availCheck <- toDeviate2proc[parent_secondary %in%  unique(treeNewER[rank == 1 ]$parent) &
                     measuredElement == '5302' & 
                     Value > availability_primary ]
  
  if(nrow(rank1availCheck) > 0){
    
    message(paste('Not enough primary availability for groups: ', paste(unique(rank1availCheck$parent_primary), collapse = ","),
                  'for years: ', paste(unique(rank1availCheck$timePointYears), collapse = ",")))
    rank1availCheck
    
  }               
  
  toDeviate2proc2 <- unique(toDeviate2proc[, .( geographicAreaM49_fi, timePointYears, parent_secondary, parent_primary, availability_primary, processing, secondLevelProcessing, weight, extraction_rate, availability_secondary)])
  
  ##-- ----
  # Check if enough secondary availability to cover unbalance
  toDeviate2proc2[, availability_secondary_primEq := availability_secondary / extraction_rate]
  toDeviate2proc2[, availability_secondary_primEq_tot := sum(availability_secondary_primEq), by = list(geographicAreaM49_fi,
                                                                                                      timePointYears,
                                                                                                      parent_primary) ]
  checkingTab <- unique(toDeviate2proc2[ , .(geographicAreaM49_fi, timePointYears, parent_primary, availability_primary, 
                                             secondLevelProcessing, availability_secondary_primEq_tot)])
  
  
insufficiency <- checkingTab[availability_secondary_primEq_tot < (-1)*secondLevelProcessing ]
  
  if(nrow(insufficiency) > 0){
    
    print('PROBLEM! Not enough secondary availability to cover production!')
    problem <- insufficiency
    print(problem)
  }
  

  secondaryFP <-  merge(treeNewER[, .(parent, rank)], unique(toDeviate2proc2[, .(geographicAreaM49_fi,
                                     timePointYears,
                                     parent_primary,
                                     parent_secondary, 
                                     secondLevelProcessing,
                                     extraction_rate,
                                     availability_secondary, 
                                     availability_secondary_primEq)]),
                                     by.x = 'parent', by.y = 'parent_secondary')
  
  
  # # transform in secondary equivalent the excess of processing not covered by the primary
  # secondaryFP[, secondLevelProcessing:= (secondLevelProcessing * extraction_rate)*(-1)]
  secondaryFP[, secondLevelProcessing:= (secondLevelProcessing)*(-1)]
  setkey(secondaryFP)
  secondaryFP <- unique(secondaryFP)
  
  # Maximum level of processing is 4 so creating space for it
  secondaryFP[ , rank1Processing := 0]
  secondaryFP[ , thirdLevelProcessing := 0]
  secondaryFP[ , rank2Processing := 0]
  secondaryFP[ , fourthLevelProcessing := 0]
  secondaryFP[ , rank3Processing := 0]
  secondaryFP[ , fifthLevelProcessing := 0]
  secondaryFP[ , rank4Processing := 0]
  
  # Calculation of first secondary parent food processing already in secondary equivalent
  secondaryFP[ rank == 1, rank1Processing := ifelse(availability_secondary_primEq > secondLevelProcessing,
                                                    secondLevelProcessing*extraction_rate , availability_secondary_primEq*extraction_rate) ]
  
  # Where there is need of second secondary parent
   rank2needed <- unique(secondaryFP[rank == 1 & rank1Processing != 0 & availability_secondary_primEq < secondLevelProcessing, .(parent, timePointYears, parent_primary, secondLevelProcessing, availability_secondary_primEq) ])
  
   if(nrow(rank2needed) > 0){
   rank2needed[ , thirdLevelProcessing := secondLevelProcessing - availability_secondary_primEq]
   
   secondaryFP[ rank == 2 & timePointYears %in% rank2needed$timePointYears &
                  parent_primary %in% rank2needed$parent_primary, thirdLevelProcessing := rank2needed$thirdLevelProcessing ]
   secondaryFP[ rank == 2 & timePointYears %in% rank2needed$timePointYears &
                  parent_primary %in% rank2needed$parent_primary, 
                rank2Processing := ifelse(availability_secondary_primEq > thirdLevelProcessing, 
                                          thirdLevelProcessing*extraction_rate, availability_secondary_primEq*extraction_rate)]
   
  # Where there is need of third secondary parent
   
   rank3needed <- unique(secondaryFP[rank == 2 & rank2Processing != 0 & availability_secondary_primEq < thirdLevelProcessing, .(parent, timePointYears, parent_primary, thirdLevelProcessing, availability_secondary_primEq) ])
   
   if(nrow(rank3needed) > 0){
   rank3needed[ , fourthLevelProcessing := thirdLevelProcessing - availability_secondary_primEq]
   
   secondaryFP[ rank == 3 & timePointYears %in% rank3needed$timePointYears &
                  parent_primary %in% rank3needed$parent_primary, fourthLevelProcessing := rank3needed$fourthLevelProcessing ]
   secondaryFP[ rank == 3 & timePointYears %in% rank3needed$timePointYears &
                  parent_primary %in% rank3needed$parent_primary, 
                rank3Processing := ifelse(availability_secondary_primEq > fourthLevelProcessing, 
                                          fourthLevelProcessing*extraction_rate, availability_secondary_primEq*extraction_rate)]
   
   
   
  
   # Where there is need of fourth secondary parent
               
   rank4needed <- unique(secondaryFP[rank == 3 & rank3Processing != 0 & availability_secondary_primEq < fourthLevelProcessing, .(parent, timePointYears, parent_primary, fourthLevelProcessing, availability_secondary_primEq) ])
   
   if(nrow(rank4needed) > 0 ){
   rank4needed[ , fifthLevelProcessing := fourthLevelProcessing - availability_secondary_primEq]
   
   secondaryFP[ rank == 4 & timePointYears %in% rank4needed$timePointYears &
                  parent_primary %in% rank4needed$parent_primary, fifthLevelProcessing := rank4needed$fifthLevelProcessing ]
   secondaryFP[ rank == 4 & timePointYears %in% rank4needed$timePointYears &
                  parent_primary %in% rank4needed$parent_primary, 
                rank4Processing := ifelse(availability_secondary_primEq > fifthLevelProcessing, 
                                          fifthLevelProcessing*extraction_rate, availability_secondary_primEq*extraction_rate)]   
   }
  
   }
  
   }
   
   
   
  processingLevelsComputed <-  melt(secondaryFP, 
        id.vars = c("parent",
                    "rank", "geographicAreaM49_fi",
                    "timePointYears", "parent_primary",
                    "secondLevelProcessing", "extraction_rate",
                    "availability_secondary", "availability_secondary_primEq"),
        measure.vars = c("rank1Processing", "rank2Processing", "rank3Processing", "rank4Processing"),
        value.name = "foodProcessingSecondary",
        variable.name = "levelOfProcessing")
  
   
   processingLevelsComputed <- processingLevelsComputed[foodProcessingSecondary != 0]
   
   processingLevelsComputed <- processingLevelsComputed[ , .(geographicAreaM49_fi, parent, timePointYears, 
                                                             availability_secondary, foodProcessingSecondary)]
   
   setnames(processingLevelsComputed, c("parent", "availability_secondary", "foodProcessingSecondary"), 
            c("ics", "availability", "processing"))
   
   
   data131full <- rbind(data131[ , secondLevelProcessing := NULL], processingLevelsComputed)
   
   data131full[, measuredElement:="5023"]
   setnames(data131full, "processing", "Value")
 
   return(data131full)
   
   #----
  # # Processing in terms of secondary equivalent with, as parent, secondary ICS
  # toDeviate[measuredElement=="secondLevelProcessing", measuredElement:="5023"]
  # setnames(toDeviate, "parent_secondary", "ics")
  # 
  # ## Take the processing part with primary equivalent
  # data131 <- melt(data131,
  #               id.vars = c("geographicAreaM49_fi",  "ics", "timePointYears", "availability"),#colnames(data_compute131[,1:3]),
  #               measure.vars = "processing",
  #               value.name = "Value" ,
  #               variable.name = "measuredElement", variable.factor = FALSE)
  # 
  # data131 <- data131[!is.na(Value)]
  # data131[measuredElement=="processing", measuredElement:="5023"]
  # 
  # ## Build the dataset containing all the food processing (131- component), both for primary
  # ## and secondary parent.
  # 
  # data131 <- rbind(data131, toDeviate)
  # 
  # # Take data without any food processing (NOT EVEN THE OFFICIAL ONES?)
  # 
  #  dataPlus131 <- merge(SUAinput, data131, by = c( "geographicAreaM49_fi", "ics", "timePointYears", "availability"), suffixes = c("_data", "_131"))
  # 
  # # Here put flag comparison
  # 
  # # flags31 <- combineFlag(data = dataPlus131, flagObservationStatusVar = flagObservationStatus_data, flagMethodVar = flagMethod_data)
  # # flags31 <- data.table(flag = flags31, row = 1:nrow(flagsInput))
  # # # Only keep protected flags
  # # flagsProt31 <- flagsInput[flagsInput %in% getProtectedFlag() ]
  # # 
  # # dataPlus131 <- dataPlus131[!row & measuredElement_data == '5023', Value_data := Value_131 ]
  #  dataPlus131 <- dataPlus131[is.na(Value_data) & measuredElement_data == '5023', Value_data := Value_131 ]
  #  setnames(dataPlus131, c("measuredElement_data", "Value_data"), c("measuredElement", "Value"))
  # # 
  # 
  # dataNo131 <- SUAinput[measuredElement!="5023",]
  # dataPlus131 <- dataPlus131[,.(geographicAreaM49_fi,timePointYears,  measuredElement, ics, Value, availability) ]
  # data <- rbind(dataNo131, data131)
  
  
  
}

