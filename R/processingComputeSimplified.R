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



processingComputeSimplified =function(data ,tree){
  
  
  ## the principal ingredient to compute the foodProcessing is the extraction rates
  ## for those items that already have a protected figures for 31 the default extractio rate 
  ## in the commodity tree has to be updated:
  
  dataProd=data[measuredElement=="51"]
  dataInput=data[measuredElement=="31"]
  #dataProcessing=data[measuredElement=="131"]
  
  computeExtractionRate=merge(dataProd, dataInput, by=c("geographicAreaM49_fi", "timePointYears", "ics"), suffixes = c("_prod","_input"))
  computeExtractionRate[, extraction_rate_C:=Value_prod/Value_input]
  computeExtractionRate = computeExtractionRate[,.(geographicAreaM49_fi, timePointYears,  ics, extraction_rate_C)]
  setnames(computeExtractionRate,"ics","child")
  
  tree = merge(tree ,computeExtractionRate, by=c("geographicAreaM49_fi", "timePointYears",  "child" ))
  tree[!is.na(extraction_rate_C) & parent %in% primary, extraction_rate:=extraction_rate_C]
  tree[,extraction_rate_C:=NULL]
  
  ### Compute the input (element 31) for each children
  data1=copy(data)
  #keep only the availability which is important to compute the ShareDownUp
  data1=unique(data1[,.(geographicAreaM49_fi, timePointYears, ics, measuredElement,Value)])
  setnames(data1, c("Value","measuredElement"), c("Value_child", "measuredElement_child"))
  setnames(data1,"ics" ,"child")
  
  ## I consider that the only commodity that may be play the role of parent are the PRIMARY
  treePrimary=tree[parent %in%  primary]
  treePrimary[, child:=as.character(child)]
  
  data1=merge(data1, treePrimary, by=c("child","timePointYears","geographicAreaM49_fi"), allow.cartesian = TRUE)
  data1[measuredElement_child=="51", input:=Value_child/extraction_rate]
  
  data_compute31= melt(data1,
              id.vars = colnames(data1[,1:3]),
              measure.vars = colnames(data1[,9]),
              value.name = "Value" ,
              variable.name = "measuredElement", variable.factor = FALSE)
  
  
  
  data_compute31[measuredElement=="input",measuredElement:="31"]
  ###################################################################In theory this part could be skippe because I have already updated the extraction
  ###################################################################rates in the tree, protected and computed will be always the same
  
  ##  If the "input" - 31 has been manually set (or is protected) it is kept and it is used to compute the extraction rate intead 
  protectedInput = data[measuredElement=="31"]
  setnames(data_compute31, "child", "ics")
  
  data_compute31 = merge(protectedInput, data_compute31, by=c("geographicAreaM49_fi","ics", "timePointYears","measuredElement"),
        suffixes = c("_protected", "_computed"))
  
  data_compute31[is.na(Value_protected) & !is.na(Value_computed), Value_protected:=Value_computed]
  data_compute31[, Value_computed:=NULL]
  data_compute31[!duplicated(data_compute31)]
  data=data[measuredElement!="31"]
  data_compute31=data_compute31[!is.na(Value_protected)]
  setnames(data_compute31, "Value_protected", "Value")
  
  ###################################################################
  ###################################################################
  data=rbind(data, data_compute31)
  data[measuredElement=="31", availability:=NA]
  
  
  data_compute131=copy(data)
  setnames(data_compute131, "ics", "child")
  data_compute131=merge(data_compute131, treePrimary, by=c("geographicAreaM49_fi","child", "timePointYears"))
  
  data_compute131[ measuredElement=="31" , processing:=sum(Value, na.rm = TRUE), by=c("geographicAreaM49_fi",
                                                               "timePointYears",
                                                               "parent")]
  

  data_compute131=data_compute131[,.(geographicAreaM49_fi, parent,timePointYears, processing)]
  
  data_compute131=data_compute131[!is.na(processing)]
  data_compute131=data_compute131[!duplicated(data_compute131)]
  

  setnames(data_compute131, "parent", "ics")
  
  ## The 'food processing' has been just computed for anly Primary Parent, 
  ## we have to compare the 'new component' based on the sum of all the child-input
  ## with the actual primary availability, in order to be sure that the new availabily
  ## (computed including the nw food processing in the primary SUA line) does not produce
  ## a negative unbalance.
  
  data_compute131=merge(data, data_compute131, by=c( "geographicAreaM49_fi","ics","timePointYears"), all.x = TRUE)
 
  #protected_131=data[!is.na(Value) & measuredElement=="131"]
  #protected_131=protected_131[,.(geographicAreaM49_fi,  ics ,timePointYears ,measuredElement,  Value)]
  #protected_131=protected_131[!duplicated(protected_131)]
  
  #merge( data_compute131,protected_131,by=c( "geographicAreaM49_fi",  "ics", "timePointYears", "measuredElement"), suffixes = c("","_protected131"))
  
  data_compute131= unique(data_compute131[,.(geographicAreaM49_fi  ,ics ,timePointYears,availability ,processing)])
  #data_compute131=data_compute131[!is.na(availability)]
  
  ## SeconLevelProcessing is computed to evaluate which primary availabilities are lower than the 
  ## fodd processing.
  
  data_compute131[, secondLevelProcessing:=availability-processing]
  
  data_compute131[secondLevelProcessing<0, processing:=processing + secondLevelProcessing ]
  
  
  ##
  secondLevelProcessing=data_compute131[secondLevelProcessing<0]
  setnames(secondLevelProcessing, "ics", "parent")
  toDeviate = merge(secondLevelProcessing,tree, by=c("parent", "geographicAreaM49_fi","timePointYears"))
  setnames(toDeviate, c("parent","child"), c("parent_primary","parent_secondary"))
  toDeviate=toDeviate[,.(parent_secondary, geographicAreaM49_fi, timePointYears, availability ,processing ,secondLevelProcessing)]
  secondary=tree[!parent %in% primary, unique(parent)]
  toDeviate = toDeviate[parent_secondary %in% secondary]
  
  setnames(tree, "child", "parent_secondary")
  toDeviate = merge(toDeviate , tree, by=c("geographicAreaM49_fi", "parent_secondary", "timePointYears"))
  
  toDeviate[, secondLevelProcessing:= (secondLevelProcessing * extraction_rate)*(-1)]
  toDeviate = toDeviate[,.(geographicAreaM49_fi, parent_secondary, timePointYears, secondLevelProcessing)]
  
  ##
  toDeviate= melt(toDeviate,
                       id.vars = colnames(toDeviate[,1:3]),
                       measure.vars = colnames(toDeviate[,4]),
                       value.name = "Value" ,
                       variable.name = "measuredElement", variable.factor = FALSE)
  
  
  toDeviate[measuredElement=="secondLevelProcessing", measuredElement:="131"]
  setnames(toDeviate, "parent_secondary", "ics")
 
  ## 
  data_compute131= melt(data_compute131,
                        id.vars = colnames(data_compute131[,1:3]),
                        measure.vars = "processing",
                        value.name = "Value" ,
                        variable.name = "measuredElement", variable.factor = FALSE)
  
  
  data_compute131[measuredElement=="processing", measuredElement:="131"]
  
  ## Build the dataset containing all the food processing (131- component), both for primary
  ## and secondary parent.
  
  data_compute131=rbind(data_compute131,toDeviate )
  
  data = data[measuredElement!="131",]
  data = data[,.(geographicAreaM49_fi,timePointYears,  measuredElement, ics, Value) ]
  data=rbind(data, data_compute131)
 
  return(data)
  
  
  
  
}
