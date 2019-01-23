
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

if(CheckDebug()){
  
  SETTINGS = ReadSettings("sws.yml")
  
  ## If you're not on the system, your settings will overwrite any others
  R_SWS_SHARE_PATH = SETTINGS[["share"]]
  
  ## Define where your certificates are stored
  SetClientFiles(SETTINGS[["certdir"]])
  
  ## Get session information from SWS. Token must be obtained from web interface
  GetTestEnvironment(baseUrl = SETTINGS[["server"]],
                     token = SETTINGS[["token"]])
  
}



files=list.files(path = "C:/Users/ROSA/Favorites/Github/sws_project/faoswsFisheryStandardization/data/localRun/SUA_standard", pattern = NULL, all.files = FALSE,
           full.names = FALSE, recursive = FALSE,
           ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

for(i in seq_along(files)){
  
currentFile=files[i]

commodityTree = ReadDatatable("fisheries_commodity_tree")

lev=findProcessingLevel(commodityTree, from="parent", to="child", aupusParam = list(itemVar="parent"))
commodityTreeLev0=commodityTree[parent %in% lev[processingLevel==0, parent],]

primary = c("1501", "1514", "1527","1540","1553", "1562", "1579",  "1570", "1587")

## from the already existing SUA tables I get the information about the information about 
## those items which has been used as input (items that have a food-processing component)

data=fread(paste0("C:/Users/ROSA/Favorites/Github/sws_project/faoswsFisheryStandardization/data/localRun/SUA_standard/",currentFile), header = TRUE)

data=data[!is.na(geographicAreaM49_fi)]
data=data[!is.na(ics)]


currentCountry=unique(data[,geographicAreaM49_fi])

data=melt( data,
  id.vars = colnames(data)[c(1:3)],
  measure.vars = colnames(data)[c(4:37)],
  variable.name = "timePointYears",
  value.name = "Value"
)


data=data[!grepl("flag", timePointYears),]

data=data[!is.na(ics)]
exist_131=data[measuredElement=="131" & !is.na(Value) & Value!=0]
setnames(exist_131, "ics","parent")
exist_131[,parent:=as.character(parent)]
filterParent=exist_131[,.(geographicAreaM49_fi,parent,timePointYears)]


# Ensure that the connection withy primary items are kept.
primaryParent=data.table(parent=primary)
# The following merge gives the intersection between all primary-parent and all the other "parent" commodities 
# obtained from the already existing SUA tables.
timePointYears = data.table(timePointYears=as.character(c(2000:2016)))
geographicAreaM49_fi=data.table(geographicAreaM49_fi=currentCountry)


primaryParent=data.table(merge.data.frame( primaryParent, timePointYears))
primaryParent=data.table(merge.data.frame( primaryParent, geographicAreaM49_fi))
filterParent=rbind(primaryParent, filterParent)
filterParent=filterParent[!duplicated(filterParent)]


filterParent[,geographicAreaM49_fi:=as.character(geographicAreaM49_fi)]
filterParent[,timePointYears:=as.character(timePointYears)]
commodityTree[,geographicAreaM49_fi:=as.character(geographicAreaM49_fi)]



commodityTree=data.table(merge.data.frame(commodityTree, geographicAreaM49_fi))
commodityTree=data.table(merge.data.frame(commodityTree, timePointYears))

commodityTreeFinal=commodityTree[filterParent, ,on=.(geographicAreaM49_fi, parent, timePointYears)]

write.csv(commodityTreeFinal,
          paste0("C:/Users/ROSA/Favorites/Github/sws_project/faoswsFisheryStandardization/data/localRun/commodityTree/tree_", currentCountry, ".csv"),
          row.names = FALSE)


}