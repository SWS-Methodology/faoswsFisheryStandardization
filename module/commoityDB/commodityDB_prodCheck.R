suppressMessages({
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(rhandsontable)
library(readr)
library(readxl)
library(plotly)
library(data.table)
library(datasets)
library(utils)
library(DT)
library(shinydashboard)
library(faosws)
library(faoswsModules)
  
})

##Compare imputations with Export quantities:

commodityDB_quantityImputed
countries=commodityDB_quantityImputed[,unique(geographicAreaM49)]

#Map the ISSCFC on the ISCAP groups
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

ISSCFC_items=unique(commodityDB_quantityImputed[,measuredItemISSCFC])
fishery_item_mapping=ReadDatatable("fishery_item_mapping")
fishery_primary_mapping=ReadDatatable("fishery_primary_mapping")



#Pull prod data from SWS
##Get Global Production Data

if(CheckDebug()){
  
  SETTINGS = ReadSettings("sws1.yml")
  
  ## If you're not on the system, your settings will overwrite any others
  R_SWS_SHARE_PATH = SETTINGS[["share"]]
  
  ## Define where your certificates are stored
  SetClientFiles(SETTINGS[["certdir"]])
  
  ## Get session information from SWS. Token must be obtained from web interface
  GetTestEnvironment(baseUrl = SETTINGS[["server"]],
                     token = SETTINGS[["token"]])
  
  sessionKey = swsContext.datasets[[1]]
  sessionKey@sessionId
  
}
keyDim=c("geographicAreaM49_fi", "fisheriesAsfis", "measuredElement", "timePointYears")

KeyGlobal = DatasetKey(domain = "Fisheries", dataset = "fi_global_production", dimensions = list(
  #geographicAreaM49_fi = Dimension(name = "geographicAreaM49_fi", keys = GetCodeList("Fisgheries", "fi_global_production","geographicAreaM49_fi" )[,code]),
  geographicAreaM49_fi = Dimension(name = "geographicAreaM49_fi", keys = as.character(countries)),
  fisheriesAsfis = Dimension(name = "fisheriesAsfis", keys = GetCodeList("Fisheries", "fi_global_production","fisheriesAsfis" )[,code]),
  fisheriesCatchArea = Dimension(name = "fisheriesCatchArea", keys = GetCodeList("Fisgheries", "fi_global_production","fisheriesCatchArea" )[,code]),
  measuredElement = Dimension(name = "measuredElement", keys = c("FI_001")),
  #timePointYears = Dimension(name = "timePointYears", keys = GetCodeList("Fisgheries", "fi_global_production","timePointYears" )[,code] )
  timePointYears = Dimension(name = "timePointYears", keys = as.character(c(2000:2016) ))
))


globalProduction=GetData(KeyGlobal)

globalProduction=globalProduction[,sum(Value, na.rm = TRUE), by=c("geographicAreaM49_fi",
                                                                  "fisheriesAsfis",
                                                                  "measuredElement",
                                                                  "timePointYears")]
setnames(globalProduction, "V1", "Value")

## Work on flags: aggragate observationFlag!!
#globalProduction[,flagObservationStatus:=""]
#globalProduction[,flagMethod:="c"]

#globalProduction=globalProduction[!duplicated(globalProduction)]


#primaryMapping = fread("data/PrimaryProduction_mapping.csv")
setnames(fishery_primary_mapping, "alphacode","fisheriesAsfis")

globalProductionMapping = merge(
  globalProduction,
  fishery_primary_mapping,
  by = c( "fisheriesAsfis"),
  all.x = TRUE)

globalProductionMapping[, timePointYears:=as.character(timePointYears)]

setnames(globalProductionMapping,"geographicAreaM49_fi", "geographicAreaM49")

setnames(fishery_item_mapping, "isscfc", "measuredItemISSCFC")
commodityDB_quantityImputedISSCAAP=commodityDB_quantityImputed=merge(fishery_item_mapping,commodityDB_quantityImputed, by="measuredItemISSCFC", all.y = TRUE )
commodityDB_quantityImputedISSCAAP[, timePointYears:=as.character(timePointYears)]
commodityDB_quantityImputedISSCAAP[, geographicAreaM49:=as.character(geographicAreaM49)]


test=merge(commodityDB_quantityImputedISSCAAP, globalProductionMapping, by=c("isscaap","geographicAreaM49","timePointYears"),suffixes = c("_processed","_primary"))
test[, primaryEq:=Value_processed*conversion_factors]
test=test[,.(geographicAreaM49, timePointYears, measuredItemISSCFC,conversion_factors,
             flagObservationStatus, flagMethod ,fisheriesAsfis, Value_primary, primaryEq)]

ui <- function(request){ navbarPage(HTML('<a href="javascript: window.location.assign(window.location.href.replace(/\\?.*/, \'\'))" tile = "XXX"><strong>Commodity DB- production of processed and preserved items</strong></a>'),
                                    id = 'main',
                                    tabPanel("Primary Production",
                                             tags$head(tags$script(src = "http://mongeau.net/js.cookie.js")),
                                             
                                             
                                             sliderInput(inputId = "timePointYears",
                                                         label="Choose the year",
                                                         value=max(as.numeric(as.character(test$timePointYears))), 
                                                         min=min(as.numeric(as.character(test$timePointYears))), 
                                                         max=max(as.numeric(as.character(test$timePointYears)))),
                                             
                                             
                                             selectInput("geo",
                                                         "Choose a country",
                                                         c("300", "203", "156")),
                                             
                                             uiOutput("ISSCFC_tab1"),
                                             
                                             dataTableOutput("dataComparison"),
                                             
                                             plotOutput("barPrimary")
                                             
                                             
                                             
                                    ),
                                    
                                    tabPanel("Check for exports",
                                             tags$head(tags$script(src = "http://mongeau.net/js.cookie.js")),
                                             
                                             selectInput("geo_tab2",
                                                         "Choose a country",
                                                         exportCompare[, unique(geographicAreaM49)]),
                                             
                                             uiOutput("ISSCFC_tab2"),
                                             #selectInput("ISSCFC",
                                             #            "ISSCFC", exportCompare[geographicAreaM49==input$geo,unique(measuredItemISSCFC)]),
                                             
                                            
                                             
                                             #dataTableOutput("dataComparisonTrade"),
                                             plotOutput("chartTrade")
                                             
                                             
                                    )
                                    
                                    
                                    
                                    )
  
}


server <- function(input, output){
  
  # Plot to evaluate the composition of each single 
  output$ISSCFC_tab1 <- renderUI({
    
    
    selectInput("ISSCFC1",
                "ISSCFC", test[geographicAreaM49==input$geo,unique(measuredItemISSCFC)])
  })
  
  
  
  
  
  
  output$dataComparison = renderDT(test[geographicAreaM49==input$geo & timePointYears== input$timePointYears &
                              measuredItemISSCFC %in%  input$ISSCFC1])
  
  
  
  output$barPrimary = renderPlot(ggplot(test[timePointYears==input$timePointYears &
                                                    geographicAreaM49==input$geo &
                                                 measuredItemISSCFC %in% input$ISSCFC1,],
                                          aes(x=fisheriesAsfis, y=Value_primary, primaryEq)) +
                                     ggtitle("Parent availability")+
                                     geom_bar(stat="identity")+
                                   facet_wrap(~(measuredItemISSCFC), ncol = 3,scales = "free") )


  output$ISSCFC_tab2 <- renderUI({
  
  
  selectInput("ISSCFC2",
              "ISSCFC", exportCompare[geographicAreaM49==input$geo_tab2,unique(measuredItemISSCFC)])
  })
  
  output$chartTrade = renderPlot(ggplot(exportCompare[geographicAreaM49==input$geo_tab2 &  measuredItemISSCFC==input$ISSCFC2], aes(x=timePointYears, y=Value)) + 
    geom_line(aes(linetype=variable,color=variable,size=variable)) + 
    geom_point( aes(shape = varableCompared))+
    scale_x_continuous(breaks=unique(exportCompare[,timePointYears])) +
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
   facet_wrap(~(measuredItemISSCFC), ncol = 3,scales = "free") )
  
  
  
  
}



shinyApp(ui=ui, server=server)


