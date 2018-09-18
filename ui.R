library(shiny)
library(readxl)
library(highcharter)
library(gmapsdistance)
library(leaflet)
library(shinythemes)
library(ggmap)
setwd("/home/rstudio/DataThon/")
StopIDandRegion <- read_excel("StopIDandRegion.xlsx")
stopnameandPOstID <- read_excel("StopIDAndcordinateswithPOSTcode.xlsx")
Postcode <- unique(stopnameandPOstID$PostCode)
SuburbandRegion <-  read_excel("RegionvsSuburb.xlsx")
for(i in 1:nrow(SuburbVsRouteID))
{
  
  if(is.na(SuburbVsRouteID$`Suburb Name`[i]))
  {
    SuburbVsRouteID$`Suburb Name`[i] <- Value
  }
  else
  {
    Value <- SuburbVsRouteID$`Suburb Name`[i]
  }
}

for(i in 1:nrow(SuburbandRegion))
{
  
  if(is.na(SuburbandRegion$RegionName[i]))
  {
    SuburbandRegion$RegionName[i] <- Value
  }
  else
  {
    Value <- SuburbandRegion$RegionName[i]
  }
}
SuburbandRegion <- na.omit(SuburbandRegion)
SuburbandRegion
Regions <- unique(SuburbandRegion$RegionName)
Suburbs <- unique(SuburbandRegion$SuburbName)
shinyUI(fluidPage(theme = shinytheme("flatly"),
                  
                  navbarPage(title=div(img(src="http://www.datasciencemelbourne.com/datathon/wp-content/uploads/2017/02/datathonLogoPB.png", width = 100, height =25, position = "right"), 
                                       "PT Dashboard (Team DataGeek)"),
                             tabPanel("Customer", 
                                      sidebarLayout(
                                        sidebarPanel(
                                          selectInput("Region", "Select Region",
                                                      sort(Regions)),
                                          uiOutput("Suburbs"),
                                          selectInput("tmode", "Select Mode Of Transport",
                                                      c("Tram" = "3",
                                                        "Train" = "2",
                                                        "Bus" = "1"), multiple=TRUE, selectize=TRUE),
                                          uiOutput("RouteID"),
                                          checkboxInput("betn","Select Destination Stop Details", value = FALSE, width = NULL),
                                          uiOutput("DestSuburbs"),
                                          uiOutput("DestRoute"),
                                          width = 3
                                        ),
                                        mainPanel(
                                          fluidPage(
                                            tabsetPanel(
                                              tabPanel( "Traffic",
                                                        fluidRow(
                                                          column(12,
                                                                 fluidRow(
                                                                   column(6, 
                                                                          highchartOutput("container2", height   = "300px")),
                                                                   column(6,
                                                                          highchartOutput("container3", height   = "300px"))
                                                                 )
                                                          )
                                                        ),
                                                        fluidRow(
                                                          column(12,
                                                                 ".",
                                                                 fluidRow(
                                                                   column(12, 
                                                                          highchartOutput("container4", height   = "290px"))
                                                                 )
                                                          )
                                                        )
                                                        
                                              ),
                                              tabPanel("Forecast", highchartOutput("forecast", height = "650px", width = "1000px")
                                              )
                                            )
                                            ,
                                            width = 9
                                          )
                                        )
                                      )
                             ),
                             tabPanel("Management",
                                      sidebarLayout(
                                        sidebarPanel(
                                          selectInput("Region_input", "Select Region",
                                                      sort(Regions)),
                                          
                                          uiOutput("Suburb_input"),
                                          #checkboxGroupInput("tmode1", "Select mode of transport",
                                          #                  c("Tram" = "3",
                                          #                   "Train" = "2",
                                          #                  "Bus" = "1")),
                                          uiOutput("RouteID_input"),
                                          width = 3
                                          
                                        ),
                                        mainPanel(
                                          fluidPage(
                                            tabsetPanel(
                                              tabPanel( "Traffic",
                                                        fluidRow(
                                                          column(12,
                                                                 fluidRow(
                                                                   column(6,highchartOutput("container6", height   = "400px")), 
                                                                   
                                                                   column(6,highchartOutput("container7", height   = "400px"))
                                                                 )
                                                          )
                                                        ),
                                                        fluidRow(
                                                          column(12,
                                                                 ".",
                                                                 fluidRow(
                                                                   column(12,highchartOutput("container8", height   = "200px", width = "1000px"))
                                                                 )
                                                          )
                                                        )
                                                        
                                              ),
                                              tabPanel("Congestion Hotspots", leafletOutput("MapPlot1", height = "600px")
                                              ),
                                              tabPanel("Forecast", highchartOutput("forecast1", height = "650px", width = "1000px")
                                              ),
                                              tabPanel("Data",
                                                       DT::dataTableOutput("mytable")
                                              )
                                            )
                                            ,
                                            width = 9
                                          )
                                          
                                          
                                        )
                                      )
                             )
                  )
                  
))


