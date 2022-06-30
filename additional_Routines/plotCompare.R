plotCompare = function(data,geoVar, elVar,elVector,itemVar,  directory, title,status,geomPoint){
  
  library(ggplot2)
 
  
  country=unique(data[, get(geoVar)])
  #country=c("156")
  
  
  for (geo in 1: length(country)){
    
    countryIter=country[geo]  
    
    dataIter=data[get(geoVar)==countryIter]
    dataIter = dataIter[get(elVar) %in% elVector]
    
    
    dir.create(paste0(directory,"/plot"), recursive = TRUE)
    pathToSave=paste0(directory, "/plot")
    
    #itemLabel=fread("C:/Users/ROSA/Desktop/Fisheries/batch0_SUA_pilotCounties/plot/label.csv", header = TRUE, sep=",")
    #itemLabel[,ics:=as.character(ics)]
    #dataIter=merge(dataIter, itemLabel, by="ics")
    
    pnp=list()
    
    pdf(file.path(pathToSave, paste0(countryIter,"_", title, ".pdf")),
        paper = "a4",width=9, height=15)    
    
   
    for(item in 1: length(unique(dataIter[!is.na(Value),get(itemVar)]) ) ) {
      
      currentItem=unique(dataIter[!is.na(Value),get(itemVar)])[item]
      
      pnp[[item]]=ggplot(dataIter[get(geoVar)==countryIter &  get(itemVar)==currentItem], aes(x=timePointYears, y=Value)) + 
        geom_line(aes(linetype=get(status),color=get(status),size=get(status))) + 
        geom_point( aes(shape = get(geomPoint)))+
        scale_x_continuous(breaks=unique(dataIter[,timePointYears])) +
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
        facet_wrap(~get(elVar)+get(itemVar), ncol = 3,scales = "free")
      print(pnp[[item]])
      
      
    }
    dev.off()
    
  }
  
}