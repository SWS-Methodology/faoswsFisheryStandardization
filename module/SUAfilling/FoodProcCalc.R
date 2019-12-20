#-- Food processing calculation ----

foodProcessingComputation <- function(SUAinput, treeNewER){
  
  #-- Primary processing ----
  
  # Tree only with primary
  treePrimary <- treeNewER[parent %in%  primary]
  treePrimary[, child:=as.character(child)]
  
  data_compute131 <- copy(SUAinput)
  setnames(data_compute131, "measuredItemFaostat_L2", "child")
  data_compute131tree <- merge(data_compute131, treePrimary, by=c("geographicAreaM49_fi", "timePointYears", "child"))
  
  # sum all the inputs
  data_compute131tree[ measuredElementSuaFbs=="5302" , processing:=sum(Value, na.rm = TRUE), by=c("geographicAreaM49_fi",
                                                                                        "timePointYears",
                                                                                        "parent")]
  
  # processing is unique for each primary parent so take only these values
  # data_compute131processing = Total input 5302 by parent
  setkey(data_compute131tree)
  data_compute131processing <- unique(data_compute131tree[!is.na(processing),.(geographicAreaM49_fi, parent,timePointYears, processing)])
  setnames(data_compute131processing, "parent", "measuredItemFaostat_L2")
  
  ## The total 'food processing' (input to cover) has been just computed for any Primary Parent, 
  ## we have to compare the 'new component' based on the sum of all the child-input
  ## with the actual primary availability, in order to be sure that the new availabily
  ## (computed including the just computed food processing in the primary SUA line) does not produce
  ## a negative imbalance.
  
  data131 <- merge(SUAinput, data_compute131processing, by=c( "geographicAreaM49_fi","measuredItemFaostat_L2","timePointYears"), all.x = TRUE)
  
  # take only the processing to check availability
  setkey(data131)
  data131 <- unique(data131[!is.na(processing),.(geographicAreaM49_fi, measuredItemFaostat_L2, timePointYears, availability, processing)])
  
  ## SeconLevelProcessing variable is computed to evaluate which primary availabilities are lower than the 
  ## food processing. 
  # secondLevelProcessing < 0 quantity left to cover (Not enough primary for all processing)
  # secondLevelProcessing > 0 primary availability exceeding processing
  data131[, secondLevelProcessing := availability-processing]
  
  # If not enough availability then the processing imputable to primary parent is put equal to the availability 
  data131[secondLevelProcessing < 0, processing := availability]
  
  # Primary measuredItemFaostat_L2 from which not enough availability to cover processing
  secondLevelProcessing <- data131[secondLevelProcessing < 0]
  setnames(secondLevelProcessing, "measuredItemFaostat_L2", "parent")
  problems <- list()
  
  #-- Secondary processing  ----
  
  if(nrow(secondLevelProcessing) > 0) {
  
  # merge by parent so that secondary parents can be found
  # toDeviate has dimensions: (parent_primary, geographicAreaM49_fi, timePointYears,
  #   availability, processing, secondLevelProcessing, parent_secondary, weight, extraction_rate, rank)
  
  toDeviate <- merge(secondLevelProcessing, treeNewER[weight != FALSE], by=c("geographicAreaM49_fi", "timePointYears", "parent"), allow.cartesian = TRUE)
  setnames(toDeviate, c("parent","child"), c("parent_primary","parent_secondary"))
  
  # # tree only with possible secondary parents
  # secondary <- treeNewER[!parent %in% primary, unique(parent)]
  # 
  # # Keep a child only if it can also be a (secondary) parent
  # toDeviate <- toDeviate[parent_secondary %in% secondary]
  
  # compare imbalance with rank 1 parent availability
  toDeviate2proc <- merge(toDeviate, SUAinput, by.x = c("geographicAreaM49_fi", "timePointYears",  "parent_secondary"),
                          by.y = c("geographicAreaM49_fi", "timePointYears",  "measuredItemFaostat_L2"), suffixes = c('_primary','_secondary'))
  
  # Make sure primary availability covers at least the rank 1 child
  rank1availCheck <- toDeviate2proc[parent_secondary %in%  unique(treeNewER[rank == 1 ]$parent) &
                                      measuredElementSuaFbs == '5302' & 
                                      Value > availability_primary ]
  
  if(nrow(rank1availCheck) > 0){
    msg1 <- paste('Not enough primary availability for groups: ', paste(unique(rank1availCheck$parent_primary), collapse = ","),
                  'for years: ', paste(unique(rank1availCheck$timePointYears), collapse = ","), 
                  'and country', paste(unique(rank1availCheck$geographicAreaM49_fi), collapse = ","))
    message(msg1)
    rank1availCheck
    
  }               
  
  msg1 <- ifelse(nrow(rank1availCheck) > 0, msg1, 'Primary products cover at least first rank children (see commodity tree).')
  toDeviate2proc2 <- unique(toDeviate2proc[, .( geographicAreaM49_fi, timePointYears, parent_secondary, parent_primary, 
                                                availability_primary, processing, secondLevelProcessing, weight, 
                                                extraction_rate, availability_secondary)])
  problems$primary <- list(msg = msg1, tab = rank1availCheck[ , .(geographicAreaM49_fi,
                                                                  timePointYears,
                                                                  parent_secondary,
                                                                  parent_primary,
                                                                  availability_primary,
                                                                  Value)])
  # If availability for secondary products was negative then production has been artificially increased so
  # secondary availability is 0 and not negative
  #toDeviate2proc2[availability_secondary < 0, availability_secondary := 0]
  # Check if enough secondary availability to cover unbalance
  toDeviate2proc2[extraction_rate != 0 & !is.na(extraction_rate), availability_secondary_primEq := availability_secondary/extraction_rate ]
  toDeviate2proc2[parent_secondary %in% unique(treeNewER$parent), availability_secondary_primEq_tot := sum(availability_secondary_primEq, na.rm = TRUE), by = list(geographicAreaM49_fi,
                                                                                                       timePointYears,
                                                                                                       parent_primary) ]
  checkingTab <- unique(toDeviate2proc2[ , .(geographicAreaM49_fi, timePointYears, parent_primary, availability_primary, 
                                             secondLevelProcessing, availability_secondary_primEq_tot)])
  
  # -- General insufficiency ----
  insufficiency <- checkingTab[availability_secondary_primEq_tot < (-1)*secondLevelProcessing ]
  
  if(nrow(insufficiency) > 0){
    
    problem <- 'PROBLEM! Not enough secondary availability to cover production!'
    msg2 <- list(problem,insufficiency)

  }
  msg2 <- ifelse(nrow(insufficiency) > 0, msg2, 'Total secondary availability can cover remaining secondary production. Level by level availability to be checked.')
  
  problems$secondaryTot <- list(msg = msg2, tab = insufficiency[ , .(geographicAreaM49_fi,
                                                                     timePointYears,
                                                                     parent_primary,
                                                                     AvailableQuantity = (availability_secondary_primEq_tot),
                                                                     Quantity2cover = (secondLevelProcessing)*(-1))])
##############################################################  
#-- NEW PART ----  
  # Put unrealistic values to primary products for which there is not enough availability to cover production
  data131covered <- merge(data131, insufficiency[ , .(geographicAreaM49_fi, timePointYears, parent_primary, availability_primary)],
        by.x = c('geographicAreaM49_fi', 'measuredItemFaostat_L2', 'timePointYears'),
        by.y = c('geographicAreaM49_fi', 'parent_primary', 'timePointYears'),
        all = TRUE)
  data131covered[!is.na(availability_primary), processing := -9999]
  
  # Exclude the products that cannot be covered
  toDeviateCovered <- merge(toDeviate2proc2[, .(geographicAreaM49_fi,
                                   timePointYears,
                                   parent_primary,
                                   parent_secondary, 
                                   secondLevelProcessing,
                                   extraction_rate,
                                   availability_secondary, 
                                   availability_secondary_primEq)], 
               insufficiency[ , .(geographicAreaM49_fi, timePointYears, parent_primary, availability_primary)], 
               by = c('geographicAreaM49_fi', 'timePointYears', 'parent_primary'), all = TRUE)
  
  toDeviateCovered <- toDeviateCovered[!is.na(availability_primary)]
  toDeviateCovered[ , availability_primary := NULL]
  setkey(toDeviateCovered)
  toDeviateCovered <- unique(toDeviateCovered)
  #######################################################################
  # Consider all ICS product that can be parent
  secondaryFP <-  merge(unique(treeNewER[, .(parent, rank)]), toDeviateCovered,
                        by.x = c("parent"), 
                        by.y = c("parent_secondary"))
  
  #-- Rank 1 secondary parent ----
  
  # transform in secondary equivalent the excess of processing not covered by the primary
  # secondaryFP[, secondLevelProcessing:= (secondLevelProcessing * extraction_rate)*(-1)]
  secondaryFP[, secondLevelProcessing:= (secondLevelProcessing)*(-1)]
  setkey(secondaryFP)
  secondaryFP <- unique(secondaryFP)
  
  # Maximum level of processing is 4 so creating space for it
  secondaryFP[ , ':=' (rank1Processing = 0, thirdLevelProcessing = 0, 
                       rank2Processing = 0, fourthLevelProcessing = 0, 
                       rank3Processing = 0, fifthLevelProcessing = 0, rank4Processing = 0)]
  
  
  #-- Rank 1 secondary parent ----
  
  # Calculation of first secondary parent food processing already in secondary equivalent
  # rank1Processing = quantity of input covered by rank 1 parent
  secondaryFP[ rank == 1, rank1Processing := ifelse(availability_secondary_primEq > secondLevelProcessing,
                                                    secondLevelProcessing*extraction_rate , availability_secondary) ]
  
  # Where there is need of second secondary parent
  rank2needed <- unique(secondaryFP[rank == 1 & availability_secondary_primEq < secondLevelProcessing, .(geographicAreaM49_fi, parent, timePointYears, parent_primary, secondLevelProcessing, availability_secondary_primEq) ])
  
 
  #-- Rank 2 secondary parent ----
  
  if(nrow(rank2needed) > 0){
    rank2needed[ , thirdLevelProcessing := secondLevelProcessing - availability_secondary_primEq]

    # Merge to put the right third level processing
    secondaryFPneeded <- merge(secondaryFP[ rank == 2], rank2needed, 
      by = c("geographicAreaM49_fi", "timePointYears", "parent_primary"), all = TRUE, suffixes = c('_available', '_needed'))

secondaryFPneeded[ is.na(thirdLevelProcessing_needed), thirdLevelProcessing_needed := 0 ]

unavailable <- secondaryFPneeded[is.na(parent_available)]

if(nrow(unavailable) > 0){
  msg3 <- paste('Not enough product to cover processing for primary group(s)', paste(unique(unavailable$parent_primary), collapse = ", "),
                "at level 2, for country", paste(unique(unavailable$geographicAreaM49_fi), collapse = ", ") )
  message(msg3)
}
msg3 <- ifelse(nrow(unavailable) > 0, msg3, 'Enough rank 2 secondary parent to cover rank 3 children (see commodity tree).')

problems$secondary <- list(msg = msg3, tab = unavailable[ , .(geographicAreaM49_fi,
                                                              timePointYears,
                                                              parent_primary,
                                                              parent_needed,
                                                              Quantity2cover = (thirdLevelProcessing_needed))] )
  
secondaryFP2 <- copy(secondaryFPneeded)
secondaryFP2[is.na(parent_available), availability_secondary_primEq_available := 0]


secondaryFP2 <- secondaryFP2[ ,rank2Processing := ifelse(availability_secondary_primEq_available > thirdLevelProcessing_needed, 
                                           thirdLevelProcessing_needed *extraction_rate, 
                                           availability_secondary_primEq_available*extraction_rate)]

secondaryFP2 <- secondaryFP2[  , .(geographicAreaM49_fi, timePointYears, parent_primary, parent_available, rank,
                                   secondLevelProcessing_available, extraction_rate, availability_secondary,
                                   availability_secondary_primEq_available, rank1Processing,
                                   rank2Processing, fourthLevelProcessing, rank3Processing, fifthLevelProcessing, rank4Processing,
                                   thirdLevelProcessing_needed)]
setnames(secondaryFP2, names(secondaryFP2), gsub("_available", "", names(secondaryFP2)))
setnames(secondaryFP2, 'thirdLevelProcessing_needed', 'thirdLevelProcessing')
    secondaryFP2 <- rbind(secondaryFP2, secondaryFP[rank != 2, ])   
    secondaryFP <- secondaryFP2
    # Where there is need of third secondary parent
    
    rank3needed <- unique(secondaryFP2[rank == 2 & availability_secondary_primEq < thirdLevelProcessing, .(geographicAreaM49_fi, parent, timePointYears, parent_primary, thirdLevelProcessing, availability_secondary_primEq) ])
    
    #-- Rank 3 secondary parent ----
    
    if(nrow(rank3needed) > 0){
      rank3needed[ , fourthLevelProcessing := thirdLevelProcessing - availability_secondary_primEq]
      
      tertiaryFPneeded <- merge(secondaryFP2[ rank == 3], rank3needed, 
                                 by = c("geographicAreaM49_fi", "timePointYears", "parent_primary"), all = TRUE, suffixes = c('_available', '_needed'))
      
      tertiaryFPneeded[is.na(fourthLevelProcessing_needed), fourthLevelProcessing_needed := 0 ]
      
      unavailable3 <- tertiaryFPneeded[is.na(parent_available)]
      
      if(nrow(unavailable3) > 0){
        msg4 <- paste('Not enough product to cover processing for primary group(s)', paste(unique(unavailable3$parent_primary), collapse = ", "),
                      "at level 3, for country ", paste(unique(unavailable3$geographicAreaM49_fi ), collapse = ", "))
        message(msg4)
      }
      msg4 <- ifelse(nrow(unavailable3) > 0, msg4, 'Enough rank 3 secondary parent to cover rank 4 children (see commodity tree).')
    
      problems$tertiary <- list(msg = msg4, tab = unavailable3[ , .(geographicAreaM49_fi,
                                                                    timePointYears,
                                                                    parent_primary,
                                                                    parent_needed,
                                                                    Quantity2cover = (fourthLevelProcessing_needed))] )
      
      secondaryFP3 <- copy(tertiaryFPneeded)
      secondaryFP3[is.na(parent_available), availability_secondary_primEq_available := 0]
      
      secondaryFP3 <- secondaryFP3[ ,rank3Processing := ifelse(availability_secondary_primEq_available > fourthLevelProcessing_needed, 
                                                               fourthLevelProcessing_needed *extraction_rate, 
                                                               availability_secondary_primEq_available*extraction_rate)]
      
      secondaryFP3 <- secondaryFP3[  , .(geographicAreaM49_fi, timePointYears, parent_primary, parent_available, rank,
                                         secondLevelProcessing, extraction_rate, availability_secondary,
                                         availability_secondary_primEq_available, rank1Processing, thirdLevelProcessing_available,
                                         rank2Processing, rank3Processing, fifthLevelProcessing, rank4Processing,
                                         fourthLevelProcessing_needed)]
      setnames(secondaryFP3, names(secondaryFP3), gsub("_available", "", names(secondaryFP3)))
      setnames(secondaryFP3, 'fourthLevelProcessing_needed', 'fourthLevelProcessing')
      secondaryFP3 <- rbind(secondaryFP3, secondaryFP2[rank != 3, ])   
      secondaryFP <- secondaryFP3
      
      # Where there is need of fourth secondary parent
      
      rank4needed <- unique(secondaryFP3[rank == 3 & availability_secondary_primEq < fourthLevelProcessing, .(geographicAreaM49_fi, parent, timePointYears, parent_primary, fourthLevelProcessing, availability_secondary_primEq) ])
     
      #-- Rank 4 secondary parent ----
      if(nrow(rank4needed) > 0 ){
        rank4needed[ , fifthLevelProcessing := fourthLevelProcessing - availability_secondary_primEq]
        
        quaternaryFPneeded <- merge(secondaryFP3[ rank == 4], rank4needed, 
                                  by = c("geographicAreaM49_fi", "timePointYears", "parent_primary"), all = TRUE, suffixes = c('_available', '_needed'))
        
        quaternaryFPneeded[ is.na(fifthLevelProcessing_needed), fifthLevelProcessing_needed := 0 ]
        
        unavailable4 <- quaternaryFPneeded[is.na(parent_available)]
        
        if(nrow(unavailable4) > 0){
          msg5 <- paste('Not enough product to cover processing for primary group(s)', paste(unique(unavailable4$parent_primary), collapse = ", "),
                        "at level 4, for country ", paste(unique(unavailable4$geographicAreaM49_fi), collapse = ", "))
          message(msg5)
        }
        msg5 <- ifelse(nrow(unavailable4) > 0, msg5, 'Enough rank 4 secondary parent to cover remaining children (see commodity tree).')
        
        problems$quaternary <- list(msg = msg5, tab =unavailable4[ , .(geographicAreaM49_fi,
                                                                       timePointYears,
                                                                       parent_primary,
                                                                       parent_needed,
                                                                       Quantity2cover = (fifthLevelProcessing_needed))])
        
        secondaryFP4 <- copy(quaternaryFPneeded)
        secondaryFP4[is.na(parent_available), availability_secondary_primEq_available := 0]
        
        secondaryFP4 <- secondaryFP4[ ,rank4Processing := ifelse(availability_secondary_primEq_available > fifthLevelProcessing_needed, 
                                                                 fifthLevelProcessing_needed *extraction_rate, 
                                                                 availability_secondary_primEq_available*extraction_rate)]
        
        secondaryFP4 <- secondaryFP4[  , .(geographicAreaM49_fi, timePointYears, parent_primary, parent_available, rank,
                                           secondLevelProcessing, extraction_rate, availability_secondary,
                                           availability_secondary_primEq_available, rank1Processing, thirdLevelProcessing,
                                           rank2Processing, fourthLevelProcessing_available, rank3Processing, 
                                           fifthLevelProcessing_needed, rank4Processing)]
        setnames(secondaryFP4, names(secondaryFP4), gsub("_available", "", names(secondaryFP4)))
        setnames(secondaryFP4, 'fifthLevelProcessing_needed', 'fifthLevelProcessing')
        secondaryFP4 <- rbind(secondaryFP4, secondaryFP3[rank != 4, ])   
        secondaryFP <- secondaryFP4
        
        
        rank5needed <- unique(secondaryFP4[rank == 4 & availability_secondary_primEq < fifthLevelProcessing,  ])
        
      }
      
    }
    
  }
  
  #-- Organising data ----
  
  processingLevelsComputed <-  melt(secondaryFP, 
                                    id.vars = c("parent",
                                                "rank", "geographicAreaM49_fi",
                                                "timePointYears", "parent_primary",
                                                "secondLevelProcessing", "extraction_rate",
                                                "availability_secondary", "availability_secondary_primEq"),
                                    measure.vars = c("rank1Processing", "rank2Processing", "rank3Processing", "rank4Processing"),
                                    value.name = "foodProcessingSecondary",
                                    variable.name = "levelOfProcessing")
  
   NotCovered <- processingLevelsComputed[ , .(geographicAreaM49_fi, parent_primary, timePointYears, secondLevelProcessing, extraction_rate, foodProcessingSecondary)]
   setkey(NotCovered)
   NotCovered <- unique(NotCovered)
   if(nrow(NotCovered) > 0){
   NotCovered[ , foodProcessingSecondary_primEq := foodProcessingSecondary/extraction_rate ]
   NotCovered[ , c('secondLevelProcessing',  'foodProcessingSecondary_primEq') := list(secondLevelProcessing,
                                                                                sum(foodProcessingSecondary_primEq)),
               by = c('geographicAreaM49_fi', 'parent_primary', 'timePointYears')]
  NotCovered[ , UncoveredQuantity := secondLevelProcessing - foodProcessingSecondary_primEq]
  NotCovered <- NotCovered[secondLevelProcessing > foodProcessingSecondary_primEq]
   }
   
  if(nrow(NotCovered) > 0){
  NotCovered <- NotCovered[ , c('secondLevelProcessing',  'foodProcessingSecondary', 'extraction_rate', 'foodProcessingSecondary_primEq') := NULL]
  setkey(NotCovered)
  NotCovered <- unique(NotCovered)
  problems$NotCovered <- NotCovered
  } else {
    problems$NotCovered <- data.table()
   }
  
  processingLevelsAssigned <- processingLevelsComputed[foodProcessingSecondary != 0]
  
  processingLevelsAssigned <- processingLevelsAssigned[ , .(geographicAreaM49_fi, parent, timePointYears, 
                                                            availability_secondary, foodProcessingSecondary)]
  
  setnames(processingLevelsAssigned, c("parent", "availability_secondary", "foodProcessingSecondary"), 
           c("measuredItemFaostat_L2", "availability", "processing"))
  
  data131full <- rbind(data131[ , secondLevelProcessing := NULL], processingLevelsAssigned)
  } else {
    
    data131full <- data131[ , secondLevelProcessing := NULL]
    
  } 
  
  
  data131full[, measuredElementSuaFbs:="5023"]
  setnames(data131full, "processing", "Value")
  
  
  
  return(list(result = data131full,
              problems = problems
              )
         )
  
}

