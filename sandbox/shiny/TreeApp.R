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

commodityTree=list()
pilotCountry=c("300", "203", "40")

for(i in 1:3){
  
currentCountry  = pilotCountry[i]
commodityTree[[i]]=fread(paste0("data/TreeCountrySpecific/tree_",currentCountry,".csv")  )

}

commodityTree=rbindlist(commodityTree)
primary=c("1501" ,"1514" ,"1527" ,"1540" ,"1553" ,"1562" ,"1579", "1570", "1587")


greece=fread("C:/Users/ROSA/Desktop/Fisheries/batch0_SUA_pilotCounties/300.csv" , header = TRUE)
austria=fread("C:/Users/ROSA/Desktop/Fisheries/batch0_SUA_pilotCounties/40.csv" , header = TRUE)
chechia=fread("C:/Users/ROSA/Desktop/Fisheries/batch0_SUA_pilotCounties/203.csv" , header = TRUE)

sua_new=rbind(greece,austria,chechia)

ui <- function(request){ navbarPage(HTML('<a href="javascript: window.location.assign(window.location.href.replace(/\\?.*/, \'\'))" tile = "XXX"><strong>SWS trade</strong></a>'),
                                    id = 'main',
                                    tabPanel("Commodity tree",
                                             tags$head(tags$script(src = "http://mongeau.net/js.cookie.js")),
                                        
                                             sliderInput(inputId = "timePointYears",
                                                         label="Choose the year",
                                                         value=max(as.numeric(as.character(commodityTree$timePointYears))), 
                                                         min=min(as.numeric(as.character(commodityTree$timePointYears))), 
                                                         max=max(as.numeric(as.character(commodityTree$timePointYears)))),
                                             
                                             selectInput("ics",
                                                         "Choose an ICS code",
                                                         primary),
                                             
                                             selectInput("geo",
                                                         "Choose a country",
                                                         c("300","40", "203")),
                                             
                                             
                                             
                                             plotOutput("availability"),
                                             plotOutput("prod"),
                                          
                                             dataTableOutput('tree')
                                             
                                    ))
  
  }

#server <- function(input, output){
#  output$hist <- renderPlot({hist(rnorm(100),main = "My hist")})
#}


## Use  input values with input$
## REACTIVITY automatically 

server <- function(input, output){
  
  # Plot to evaluate the composition of each single 
  output$tree = renderDT(commodityTree[timePointYears==input$timePointYears &
                                       geographicAreaM49_fi==input$geo &
                                       parent %in% getChildren(commodityTree, "parent" , "child", input$ics) ,]) ;
  
  
  output$availability = renderPlot(ggplot(sua_new[timePointYears==input$timePointYears &
                                                        geographicAreaM49_fi==input$geo &
                                                        ics %in% getChildren(commodityTree, "parent", "child" ,input$ics)],
                                                aes(x=ics, y=availability)) +
                                                ggtitle("Parent availability")+
                                                geom_bar(stat="identity"));
  
  output$prod = renderPlot(ggplot(sua_new[timePointYears==input$timePointYears &
                                        geographicAreaM49_fi==input$geo &
                                        ics %in% getChildren(commodityTree, "parent", "child" ,input$ics) & measuredElement=="51"],
                                        aes(x=ics, y=Value)) +
                                        ggtitle("Child production")+
                                        geom_bar(stat="identity"))
  
  
}



shinyApp(ui=ui, server=server)