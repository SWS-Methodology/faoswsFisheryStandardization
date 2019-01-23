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



commodityTree=fread("data/commodityTree.csv")
factors=fread("data/factors.csv")
primary = c("1501", "1514", "1527","1540","1553", "1562", "1579",  "1570", "1587")


## Weight=0 is for co-products that do not have to be standardized
commodityTree[,weight:=as.numeric(weight)]
commodityTree[!weight %in% c(0), weight:=1]
factors[, child:=as.character(child)]
commodityTreeP=commodityTree[parent %in% primary,]


commodityTreeP=merge(commodityTreeP, factors, by="child")
commodityTreeNA=commodityTree[!parent %in% primary]
commodityTreeNA[,extractionRate:=NA]
commodityTreeTCF=rbind(commodityTreeNA, commodityTreeP)


commodityTreeTCF[,extractionRate:=extractionRate/100]


for(i in which(is.na(commodityTreeTCF$extractionRate) * commodityTreeTCF$weight!=0) ){
  
  p= commodityTreeTCF[i,parent]
  c= commodityTreeTCF[i,child]
  f1=commodityTreeTCF[parent %in% primary & child==p , extractionRate]
  f1=f1[!is.na(f1)]
  f2=commodityTreeTCF[parent %in% primary & child==c , extractionRate]
  f2=f2[!is.na(f2)]
  if(length(f1)>0 & length(f2)>0) {
    commodityTreeTCF[i, extractionRate:=(1/(f1/f2))]
  }  
  
}

write.csv(commodityTreeTCF,"commodityTreeTCF_SWS.csv" , row.names = FALSE)

