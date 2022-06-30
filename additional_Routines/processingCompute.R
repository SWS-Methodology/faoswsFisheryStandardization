##'processingCompute
##'
##' This function has been created to compute the food processing.
##' 
##' @param data data.table datataset containing SUA 
##' @param tree data.table dataset containing the commodity tree
##' @return Returns a dataset enlarged with the foodProcessing 
##' 
##' @export



processingCompute =function(data ,tree){
  
  
  ### Compute the ShareDownUp
  ### processingLevel = findProcessingLevel(commodityTree, from = "parent", to = "child")
  data1=copy(data)
  #keep only the availability which is important to compute the ShareDownUp
  data1=unique(data1[,.(geographicAreaM49_fi, timePointYears, ics, availability)])
  setnames(data1,"ics" ,"parent")
  data1[, parent:=as.character(parent)]
  tree[, parent:=as.character(parent)]
  
  data1=merge(data1, tree, by=c("parent","timePointYears","geographicAreaM49_fi"), allow.cartesian = TRUE)
  
  #### We are currently working with commodityTree0 this means that all weights are 1.
  data1[, avChildEq:=availability*extraction_rate]
  data1[, shareDownUpDEN:=sum(avChildEq, na.rm = TRUE), by=c("child", "timePointYears", "geographicAreaM49_fi") ]
  
  data1[, shareDownUp:=avChildEq/shareDownUpDEN ]
  data1[, child:=as.character(child)] 
  
  data2=copy(data)
  
  setnames(data2, "ics", "child")
  data2[, child:=as.character(child)]
  data2=data2[measuredElement=="51",.(geographicAreaM49_fi,child, measuredElement, timePointYears,Value)]
  setnames(data2, "Value", "childProd")
  
  
  SUA_processing=merge(data1,  data2,
                       by= c("geographicAreaM49_fi" ,"child", "timePointYears"),
                       suffixes = c("_parent", "_child") )
  
  SUA_processing[, foodProcessing:=((childProd*weight)/extraction_rate)*shareDownUp]
  
  SUA_processing[(foodProcessing>availability & shareDownUp==1), childStock:=availability-foodProcessing]
  SUA_processing[, childStock:=childStock*extraction_rate]
  SUA_processing[, childProd:=childProd+childStock]
  SUA_processing[!is.na(childStock), foodProcessing:=((childProd*weight)/extraction_rate)*shareDownUp]
#####################################################################################################
  
  
  SUA_processingChild=SUA_processing[,.(geographicAreaM49_fi,timePointYears,child,childProd,childStock)]
  SUA_processingChild=SUA_processingChild[!duplicated(SUA_processingChild)]
  SUA_processingChild= melt(SUA_processingChild,
                            id.vars = colnames(SUA_processingChild[,1:3]),
                            measure.vars = colnames(SUA_processingChild[,4:5]),
                            value.name = "Value" ,
                            variable.name = "measuredElement")
  SUA_processingChild[,measuredElement:=ifelse(measuredElement=="childProd", "51","71")]
  setnames(SUA_processingChild,"child","ics")
  
  
  SUA_processingParent=SUA_processing[,.(geographicAreaM49_fi,timePointYears,parent,foodProcessing)]
  SUA_processingParent= melt(SUA_processingParent,
                            id.vars = colnames(SUA_processingParent[,1:3]),
                            measure.vars = colnames(SUA_processingParent[,4]),
                            value.name = "Value" ,
                            variable.name = "measuredElement")
  SUA_processingParent[,measuredElement:="131"]
  setnames(SUA_processingParent,"parent","ics")
  SUA_processingParent[,Value:=sum(Value, na.rm = TRUE), by=c("geographicAreaM49_fi",
                                                              "timePointYears",
                                                              "ics")]
  SUA_processingParent=SUA_processingParent[!duplicated(SUA_processingParent)]
  #####################################################################################################  
  
  
  
  SUA_processing = rbind(SUA_processingChild,SUA_processingParent)
  
  
  data=merge(data, SUA_processing, c("geographicAreaM49_fi", "timePointYears" ,"ics", "measuredElement") ,
             suffixes = c("_old","_new"),all.x=TRUE)
  
  data[, Value_old:=ifelse(!is.na(Value_new), Value_new, Value_old)]
  setnames(data, "Value_old", "Value")
  
  data = data[,.(geographicAreaM49_fi,timePointYears,  measuredElement, ics, Value) ]
  return(data)
  
####  ## This is to populate the element: 31
####  SUA_stock= SUA_processing[,.(geographicAreaM49_fi,child,timePointYears,stockC)]
####  
####  
####  SUA_processing[,foodProcessing:=sum(foodProcessing, na.rm = TRUE), by=c("geographicAreaM49_fi",
####                                                                          "timePointYears",
####                                                                          "parent")]
#### 
####
####  
####  SUA_processing=SUA_processing[,c("geographicAreaM49_fi",
####                                   "timePointYears",
####                                   "parent","foodProcessing"), with=FALSE]
####  
####  SUA_processing=unique(SUA_processing )
####  SUA_processing[,measuredElement:="131"]
####  setnames(SUA_processing, c("parent", "foodProcessing"), c("ics", "Value"))
####  
####  SUA_stock=unique(SUA_stock )
####  SUA_stock[,measuredElement:="31"]
####  setnames(SUA_stock, c("child", "stockC"), c("ics", "Value"))
####  
####  data[,availability:=NULL]
####  data[,flagObservationStatus:=NULL]
####  data[,flagMethod:=NULL]
####  
####  return(rbind(data, SUA_processing,SUA_stock ))
  
  
}