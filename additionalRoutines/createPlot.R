
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
greece=fread("data/SUA_standard/GreeceCleaned.csv" , header = TRUE)
austria=fread("data/SUA_standard/AustriaCleaned.csv" , header = TRUE)
chechia=fread("data/SUA_standard/CzechiaCleaned.csv" , header = TRUE)

sua_old=rbind(greece,austria,chechia)

sua_old=melt( sua_old,
              id.vars = colnames(sua_old)[c(1:3)],
              measure.vars = colnames(sua_old)[c(4:20)],
              variable.name = "timePointYears",
              value.name = "Value"
)
sua_old[,Status:="OLD"]
sua_old=sua_old[!is.na(ics)]

#Get New SUA


greece=fread(paste0(directory,"/SUA_balanced/SUA_balanced_300.csv" ), header = TRUE)
austria=fread(paste0(directory,"/SUA_balanced/SUA_balanced_40.csv" ), header = TRUE)
chechia=fread(paste0(directory,"/SUA_balanced/SUA_balanced_203.csv") , header = TRUE)

sua_new=rbind(greece,austria,chechia)
sua_new[,availability:=NULL]
sua_new[,Status:="NEW"]
sua_new[,V1:=NULL]

nutrient=melt(sua_new, id.vars = colnames(sua_new)[1:4],
                                 measure.vars= colnames(sua_new)[6:8],
              value.name = "Value")
              
nutrient[,measuredElement:=NULL]
nutrient=nutrient[Value!=0,]
setnames(nutrient,"variable","measuredElement")
nutrient[measuredElement=="calories", measuredElement:="261"]
nutrient[measuredElement=="proteins", measuredElement:="271"]
nutrient[measuredElement=="fats", measuredElement:="281"]



sua_new=sua_new[,1:5]
sua_new=rbind(sua_new, nutrient)
sua_new[,Status:="NEW"]

data=rbind(sua_new, sua_old)
data[,timePointYears:=as.numeric(as.character(timePointYears))]
data=data[geographicAreaM49_fi=="300"]
data = data[measuredElement %in% c("51", "91", "61","101", "121","151","131", "141", "261", "281", "271")]

pathToSave=paste0(directory,"/plot")



pnp=list()


for(item in 1: length(unique(data[!is.na(Value),ics]) ) ) {

  currentItem=unique(data[is.na(Value),ics])[item]
    
pdf(file.path(pathToSave, paste0(currentItem,"test.pdf")),
      paper = "a4",width=9, height=15)    
  

data[,measuredElement:=as.character(measuredElement)]

element=unique(data[!is.na(Value) & ics==currentItem, measuredElement])
pags = 12
nump <- seq(1,length(element),pags)
if(length(element)/pags==length(element)%/%pags){
  lgp=length(element)/pags}else{
    lgp=length(element)%/%pags+1
  }

for(i in 1:lgp){
if(!is.na(nump[i+1])){
  
pnp[[i]]=ggplot(data[geographicAreaM49_fi=="300" & measuredElement%in%element[nump[i]:nump[i+1]-1] & ics==currentItem], aes(x=timePointYears, y=Value)) + 
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
  facet_wrap(~measuredElement, ncol = 3,scales = "free")
print(pnp[[i]])

    }else{
      if(!is.na(nump[i])){
      
      pnp[[i]]=ggplot(data[geographicAreaM49_fi=="300" & measuredElement %in% element[nump[i]:length(element)] & ics==currentItem], aes(x=timePointYears, y=Value)) + 
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
        facet_wrap(~measuredElement, ncol = 3,scales = "free")
  
  print(pnp[[i]])
  } }

}
    
dev.off()

}



dev.off()

