
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


#Get Old SUA
#greece=fread("data/SUA_standard/GreeceCleaned.csv" , header = TRUE)
#austria=fread("data/SUA_standard/AustriaCleaned.csv" , header = TRUE)
#chechia=fread("data/SUA_standard/CzechiaCleaned.csv" , header = TRUE)
#chinaOLD=fread("data/SUA_standard/ChinaCleaned.csv" , header = TRUE)

files=list.files("data/localRun/SUA_standard")

sua_old=list()

for(file in seq_along(files)){
  
  currentFile=files[file]
  data= fread(paste0("data/localRun/SUA_standard/",currentFile), header = TRUE) 
  data=data[,1:37]
  sua_old[[file]]=data
  }

sua_old= rbindlist(sua_old)

sua_old=melt( sua_old,
              id.vars = colnames(sua_old)[c(1:3)],
              measure.vars = colnames(sua_old)[c(4:37)],
              variable.name = "timePointYears",
              value.name = "Value",
              variable.factor = FALSE
)
sua_old[,Status:="OLD"]
sua_old=sua_old[!is.na(ics)]

sua_old=sua_old[!grepl("_flag", timePointYears)]
sua_old[,timePointYears:=as.integer(timePointYears)]
sua_old[,Value:=as.numeric(Value)]


#Get New SUA

files_new=list.files(paste0(directory,"/SUA_balanced"))

sua_new=list()

for(file in seq_along(files_new)){
  
  currentFile=files_new[file]
  data= fread(paste0(directory,"/SUA_balanced/",currentFile), header = TRUE) 
  sua_new[[file]]=data
}

sua_new= rbindlist(sua_new)


#sua_new=rbind(greece,austria,chechia)
sua_new[,availability:=NULL]
sua_new[,Status:="NEW"]
sua_new[,V1:=NULL]

nutrient=melt(sua_new, id.vars = colnames(sua_new)[1:4],
              measure.vars= colnames(sua_new)[6:8],
              value.name = "Value")

nutrient[,measuredElement:=NULL]
nutrient=nutrient[Value!=0,]
nutrient[,Value:=Value/100]

setnames(nutrient,"variable","measuredElement")
nutrient[measuredElement=="calories", measuredElement:="261"]
nutrient[measuredElement=="proteins", measuredElement:="271"]
nutrient[measuredElement=="fats", measuredElement:="281"]



sua_new=sua_new[,1:5]
sua_new=rbind(sua_new, nutrient)
sua_new[,Status:="NEW"]

data=rbind(sua_new, sua_old)
data[,timePointYears:=as.numeric(as.character(timePointYears))]


countries=c("300", "203" , "40", "246", "156", "792","643","268")


for (geo in 1: length(countries)){
  
countryIter=countries[geo]  

dataIter=data[geographicAreaM49_fi==countryIter]
dataIter = dataIter[measuredElement %in% c("51","71", "91", "61","101", "121","151","131", "141", "261", "281", "271")]

dataIter[measuredElement=="51", measuredElement:="Production"]
dataIter[measuredElement=="61", measuredElement:="ImportQty"]
dataIter[measuredElement=="91", measuredElement:="ExportQty"]
dataIter[measuredElement=="101", measuredElement:="Feed"]
dataIter[measuredElement=="121", measuredElement:="Waste"]
dataIter[measuredElement=="151", measuredElement:="OtherUtil"]
dataIter[measuredElement=="131", measuredElement:="foodProc"]
dataIter[measuredElement=="141", measuredElement:="Food"]
dataIter[measuredElement=="261", measuredElement:="TotCal"]
dataIter[measuredElement=="281", measuredElement:="TotFats"]
dataIter[measuredElement=="271", measuredElement:="TotProt"]
dataIter[measuredElement=="71", measuredElement:="Stock"]




pathToSave=paste0(directory, "/plot")

itemLabel=fread("C:/Users/ROSA/Desktop/Fisheries/batch0_SUA_pilotCounties/plot/label.csv", header = TRUE, sep=",")
itemLabel[,ics:=as.character(ics)]
dataIter=merge(dataIter, itemLabel, by="ics")

pnp=list()

pdf(file.path(pathToSave, paste0(countryIter,"_SUA.pdf")),
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
      
      
  }
dev.off()

}
dev.off()

##Comparison OLD - VECCHIO


#sua_old[, ics:=as.character(ics)]
#sua_old[, measuredElement:=as.character(measuredElement)]
#sua_old[, geographicAreaM49_fi:=as.character(geographicAreaM49_fi)]
#sua_new[,  measuredElement:=as.character(measuredElement)]
#sua_old[,timePointYears:=as.integer(as.character((timePointYears)))]



#check=merge(sua_old, sua_new, by=c("ics", "geographicAreaM49_fi", "timePointYears", "measuredElement"), suffixes = c("_old","_new" ))
#check[,diff:=Value_old-Value_new]



#check[timePointYears=="2015"& diff>100,]

