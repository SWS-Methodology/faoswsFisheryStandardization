
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
  library(ggplot2)
  library(dplyr)
  library(tidyr)
})




pathToSave=paste0(directory, "/plot")

#Get Old SUA
FBS_old=fread("C:/Users/ROSA/Desktop/Fisheries/batch0_SUA_pilotCounties/FBS/FBS_fishstatJ.csv", header = TRUE)

FBS_old=melt( FBS_old,
              id.vars = colnames(FBS_old)[c(1:4)],
              measure.vars = colnames(FBS_old)[c(5:18)],
              variable.name = "timePointYears",
              value.name = "Value"
)
FBS_old[,Value:=as.numeric(Value)]


FBS_old[,Status:="OLD"]


FBS_new=fread(paste0(directory,"/FBS/FBS.csv"), header = TRUE)
itemLabel=fread(file.path(pathToSave, paste0("label.csv")), header = TRUE, sep=",")
itemLabel[,ics:=as.character(ics)]

FBS_new=merge(FBS_new, itemLabel, by="ics")

FBS_new[,Status:="NEW"]


FBS_new = FBS_new[measuredElement %in% c("51", "91", "61","141", "151", "111", "101")]
data=rbind(FBS_new, FBS_old)




data[measuredElement=="Imports", measuredElement:="ImportQty"]
data[measuredElement=="Exports", measuredElement:="ExportQty"]
data[measuredElement=="Non-food uses", measuredElement:="OtherUtil"]
data[measuredElement=="Total food supply", measuredElement:="Food"]
data[measuredElement=="calories", measuredElement:="TotCal"]
data[measuredElement=="fats", measuredElement:="TotFats"]
data[measuredElement=="proteins", measuredElement:="TotProt"]
data[measuredElement=="Stock variations", measuredElement:="Stock"]


data[measuredElement=="51", measuredElement:="Production"]
data[measuredElement=="61", measuredElement:="ImportQty"]
data[measuredElement=="91", measuredElement:="ExportQty"]
data[measuredElement=="101", measuredElement:="Feed"]
data[measuredElement=="121", measuredElement:="Waste"]
data[measuredElement=="151", measuredElement:="OtherUtil"]
data[measuredElement=="131", measuredElement:="foodProc"]
data[measuredElement=="141", measuredElement:="Food"]
data[measuredElement=="261", measuredElement:="TotCal"]
data[measuredElement=="281", measuredElement:="TotFats"]
data[measuredElement=="271", measuredElement:="TotProt"]
data[measuredElement=="71", measuredElement:="Stock"]
data[measuredElement=="111", measuredElement:="Baiting"]



country=c("203","300","40")

for (geo in 1:3){
  
  countryIter=country[geo]  

  dataIter=data[geographicAreaM49_fi==countryIter]

  dataIter=dataIter[,Value:=as.numeric(Value)]

  dataIter=dataIter[,timePointYears:=as.numeric(timePointYears)]




pnp=list()

pdf(file.path(pathToSave, paste0(countryIter,"_FBS.pdf")),
    paper = "a4",width=9, height=15)    


for(item in 1: length(unique(dataIter[!is.na(Value),ics]) ) ) {
  
  currentItem=unique(dataIter[!is.na(Value),ics])[item]
  
  
  pnp[[item]]=ggplot(dataIter[geographicAreaM49_fi==countryIter &  ics==currentItem], aes(x=timePointYears, y=Value)) + 
    geom_line(aes(linetype=Status,color=Status,size=Status)) + 
    scale_x_continuous(breaks=2000:2016) +
    scale_linetype_manual(values=c("solid","dotdash","dotdash","solid","solid")) +
    scale_colour_manual(values = c("red","blue")) +
    scale_size_manual(values=c(0.8,0.8)) +
    theme(axis.title =element_text(size=5),
          axis.text.y = element_text(size=5),
          axis.text.x = element_text(size=4,angle = 50, hjust = 1),
          legend.text = element_text(size=6),
          strip.text.x = element_text(size = 7),
          legend.position = "top",
          panel.grid.major = element_line(colour = "grey80", linetype = "longdash",size= 0.2 ), 
          panel.grid.minor = element_line(colour="white",size=0), 
          panel.background = element_rect(fill="white")) +
    facet_wrap(~measuredElement+label+ics, ncol = 3,scales = "free")
  print(pnp[[item]])
  
  dev.off()  
}

dev.off()
}
