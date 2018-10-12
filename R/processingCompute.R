#'processingCompute
#'
#' This function has been created to compute the food processing.
#' 
#' @param data data.table datataset containing SUA 
#' @param tree data.table dataset containing the commodity tree
#' @return Returns a dataset enlarged with the foodProcessing 
#' 
#' @export



processingCompute =function(data ,tree){
  
  
  ### Compute the ShareDownUp
  ### processingLevel = findProcessingLevel(commodityTree, from = "parent", to = "child")
  data1=copy(data)
  #keep only the availability which is important to compute the ShareDownUp
  data1=unique(data1[,.(geographicAreaM49_fi, timePointYears, ics, availability)])
  setnames(data1,"ics" ,"parent")
  data1[, parent:=as.character(parent)]
  data1=merge(data1, tree, by="parent", allow.cartesian = TRUE)
  
  #### We are currently working with commodityTree0 this means that all weights are 1.
  data1[, avChildEq:=availability*extraction_rate]
  data1[, shareDownUpDEN:=sum(avChildEq, na.rm = TRUE), by=c("child", "timePointYears", "geographicAreaM49_fi") ]
  
  data1[, shareDownUp:=avChildEq/shareDownUpDEN ]
  data1[, child:=as.character(child)] 
  
  data2=copy(data)
  
  setnames(data2, "ics", "child")
  data2[, child:=as.character(child)]
  data2=data2[measuredElement=="51",.(geographicAreaM49_fi,child, measuredElement, timePointYears,Value)]
  
  
  SUA_processing=merge(data1,  data2,
                       by= c("geographicAreaM49_fi" ,"child", "timePointYears"),
                       suffixes = c("_parent", "_child") )
  
  SUA_processing[, foodProcessing:=((Value*weight)/extraction_rate)*shareDownUp]

  ## This is to populate the element: 31
  SUA_input= SUA_processing[,.(geographicAreaM49_fi,child,timePointYears,foodProcessing)]
  
  
  SUA_processing[,foodProcessing:=sum(foodProcessing, na.rm = TRUE), by=c("geographicAreaM49_fi",
                                                                          "timePointYears",
                                                                          "parent")]
  
  SUA_processing=SUA_processing[,c("geographicAreaM49_fi",
                                   "timePointYears",
                                   "parent","foodProcessing"), with=FALSE]
  
  SUA_processing=unique(SUA_processing )
  SUA_processing[,measuredElement:="131"]
  setnames(SUA_processing, c("parent", "foodProcessing"), c("ics", "Value"))
  
  SUA_input=unique(SUA_input )
  SUA_input[,measuredElement:="31"]
  setnames(SUA_input, c("child", "foodProcessing"), c("ics", "Value"))
  
  data[,availability:=NULL]
  data[,flagObservationStatus:=NULL]
  data[,flagMethod:=NULL]
  
  return(rbind(data, SUA_processing,SUA_input ))
  
  
}