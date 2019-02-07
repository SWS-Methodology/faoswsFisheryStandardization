# get old SUA
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


#get new SUA

greece=fread("C:/Users/ROSA/Desktop/Fisheries/batch0_SUA_pilotCounties/300.csv" , header = TRUE)
austria=fread("C:/Users/ROSA/Desktop/Fisheries/batch0_SUA_pilotCounties/40.csv" , header = TRUE)
chechia=fread("C:/Users/ROSA/Desktop/Fisheries/batch0_SUA_pilotCounties/203.csv" , header = TRUE)

sua_new=rbind(greece,austria,chechia)
sua_new[,availability:=NULL]
sua_new[,Status:="NEW"]

data=rbind(sua_new, sua_old)
data[,timePointYears:=as.numeric(as.character(timePointYears))]


sua_old[,geographicAreaM49_fi:=as.character(geographicAreaM49_fi)]
sua_old[,ics:=as.character(ics)]
sua_old[,measuredElement:=as.character(measuredElement)]

tableData=merge( sua_new[,.(geographicAreaM49_fi , ics, measuredElement, timePointYears ,      Value)],
                 sua_old[,.(geographicAreaM49_fi , ics, measuredElement, timePointYears ,      Value)],
                 by=c("timePointYears","geographicAreaM49_fi" ,"ics", "measuredElement"), 
                 suffixes = c("_new", "_old"))

#ggplot(data[geographicAreaM49_fi=="300" & measuredElement=="51" & ics=="1504"], aes(x=timePointYears, y=Value)) + 
#  geom_line(aes(linetype=Status,color=Status,size=Status)) + 
#  scale_x_continuous(breaks=2000:2016) +
#  scale_linetype_manual(values=c("solid","dotdash","dotdash","solid","solid")) +
#  scale_colour_manual(values = c("red","blue")) +
#  scale_size_manual(values=c(0.8,0.8)) +
#  theme(axis.title =element_text(size=5),
#        axis.text.y = element_text(size=5),
#        axis.text.x = element_text(size=4,angle = 50, hjust = 1),
#        legend.text = element_text(size=6),
#        strip.text.x = element_text(size = 7),
#        legend.position = "top",
#        panel.grid.major = element_line(colour = "grey80", linetype = "longdash",size= 0.2 ), 
#        panel.grid.minor = element_line(colour="white",size=0), 
#        panel.background = element_rect(fill="white")) 

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

  
  ui <- function(request){ navbarPage(HTML('<a href="javascript: window.location.assign(window.location.href.replace(/\\?.*/, \'\'))" tile = "XXX"><strong>SWS</strong></a>'),
                                      id = 'main',
                                      tabPanel("Charts",
                                               tags$head(tags$script(src = "http://mongeau.net/js.cookie.js")),
                                               
                                               selectInput("ics","Choose an ICS code", unique(data$ics)),
                                               
                                               selectInput("geo","Choose a country",unique(data$geographicAreaM49_fi)),
                                               
                                               plotOutput("prod"),
                                               plotOutput("foodProc"),
                                               plotOutput("import"),
                                               plotOutput("export")
                                               
                                      ),
                                      tabPanel("Table",
                                               
                                               sliderInput(inputId = "timePointYear",
                                                           label="Choose the year",
                                                           value=max(as.numeric(as.character(data$timePointYears))), 
                                                           min=min(as.numeric(as.character(data$timePointYears))), 
                                                           max=max(as.numeric(as.character(data$timePointYears)))),
                                               
                                               dataTableOutput('dataComparison')
                                      )
                                               
                                              
                                      
  
  )
  
  }

#server <- function(input, output){
#  output$hist <- renderPlot({hist(rnorm(100),main = "My hist")})
#}


## Use  input values with input$
## REACTIVITY automatically 

server <- function(input, output){
  
  # Plot to evaluate the composition of each single 
  output$prod = renderPlot(ggplot(data[geographicAreaM49_fi==input$geo & measuredElement=="51" & ics==input$ics], aes(x=timePointYears, y=Value)) + 
                             geom_line(aes(linetype=Status,color=Status,size=Status))+
                             ggtitle("Production")+ 
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
                                   panel.background = element_rect(fill="white")) 
  );
  output$foodProc = renderPlot(ggplot(data[geographicAreaM49_fi==input$geo & measuredElement=="131" & ics==input$ics], aes(x=timePointYears, y=Value)) + 
                                 geom_line(aes(linetype=Status,color=Status,size=Status))+
                                 ggtitle("Food processing")+  
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
                                       panel.background = element_rect(fill="white")) 
  );
  output$import = renderPlot(ggplot(data[geographicAreaM49_fi==input$geo & measuredElement=="61" & ics==input$ics], aes(x=timePointYears, y=Value)) + 
                               geom_line(aes(linetype=Status,color=Status,size=Status)) +
                               ggtitle("Import")+ 
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
                                     panel.background = element_rect(fill="white")) 
  );
  output$export = renderPlot(ggplot(data[geographicAreaM49_fi==input$geo & measuredElement=="91" & ics==input$ics], aes(x=timePointYears, y=Value)) + 
                               geom_line(aes(linetype=Status,color=Status,size=Status)) +
                               ggtitle("Export")+  
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
                                     panel.background = element_rect(fill="white")) 
  );
  
  
  output$dataComparison = renderDT(tableData[geographicAreaM49_fi==input$geo & timePointYears %in% input$timePointYear &
                                         ics %in%  input$ics ,]) 
  

}


shinyApp(ui=ui, server=server)
