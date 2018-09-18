library(highcharter)
library(shiny)
library(readxl)
library(highcharter)
library(gmapsdistance)
library(leaflet)
library(tidyr)
library(DT)
setwd("/home/rstudio/DataThon/")
StopnameCount <- read_xlsx("StopNameshortandCountWeekdays.xlsx")
DayAndStopname <- read_xlsx("HOurly_touch_on.xlsx")
ModeandStop <-  read_excel("ModevsStopnameShort.xlsx")
SubModeCount <- read_xlsx("ModeVsSuburb.xlsx")
forecastData <- read.csv("forecastData.csv")
CardvsSuburb <- read_xlsx("CardGroupvsSuburb.xlsx")
SuburbvsCardtype <- read_xlsx("SuburbvsCardType.xlsx")
SuburbVsRouteID <- read_xlsx("SuburbVsRouteID.xlsx")
SuburbNamevsVechicleID <- read.csv("SuburbNamevsVechicleID.csv")
suburbVsStopVsYear <- read.csv("suburbVsStopVsYear.csv")
SuburbModeYeardsCount <- read.csv("SuburbModeYeardsCount.csv")
SuburbCardGroup <- read.csv("SuburbCardGroup.csv")
population <- read_xlsx("Population Data.xlsx")
##Source: http://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/3218.02016-17?OpenDocument

names(StopnameCount) <- c("Stop Name Short","Mon","Tue","Wed","Thur","Fri","Sat","Sun")

colnames(forecastData)
forecastData<-forecastData[,-1]

for(i in 1:nrow(ModeandStop))
{
  if(is.na(ModeandStop$Mode[i]))
  {
    ModeandStop$Mode[i] <- Value
  }
  else
  {
    Value <- ModeandStop$Mode[i]
  }
}
shinyServer(function(input, output,session){
  output$container3 <- renderHighchart({
    suburb2 <- input$Suburb
    PlotData <- SubModeCount[SubModeCount$`Suburb Name` == suburb2,]
    x <- colnames(PlotData[, 2:4])
    y <- as.numeric(PlotData[, 2:4])
    x <- factor(x, levels = c("2", "1", "3"), labels = c("Train", "Bus", "Tram"))
    hc <- highchart() %>%
      hc_add_series_labels_values(x, y, name = "Pie",
                                  colorByPoint = TRUE, type = "pie",showInLegend = TRUE)%>%
      hc_add_theme(hc_theme_elementary())%>% 
      hc_title(text = paste("Preference Of Transport Service In", suburb2)) %>%
      hc_subtitle(text = "Transport Service Options Preferred")
  
    })
  output$container8 <- renderHighchart({
    StopName4 <- input$RouteID_input
    PlotData <- DayAndStopname[DayAndStopname$`Stop Name Short` == StopName4,]
    y <- as.numeric(PlotData[, 2:24])
    x <- c("1AM","2AM","3AM","4AM","5AM","6AM","7AM","8AM","9AM","10AM","11AM","12PM","1PM","2PM","3PM","4PM","5PM","6PM","7PM","8PM","9PM","10PM","11PM")
    hc <- highchart() %>%
        hc_add_series(data = round(y, 4), 
                      x= x,
                      type = "line",
                      name = paste("Average Passenger Traffic At",StopName4),
                      color = "#ADA1EA", showInLegend = TRUE)%>%
        hc_xAxis(categories = x,
                 tickmarkPlacement = "on",
                 opposite = TRUE)%>%
        hc_add_theme(hc_theme_elementary())%>% 
        hc_title(text = paste("Average Hourly Passenger Traffic At", StopName4))
  })
  
  output$container4 <- renderHighchart({
    StopName <- input$Stop
    PlotData <- DayAndStopname[DayAndStopname$`Stop Name Short` == StopName,]
    y <- as.numeric(PlotData[, 2:24])
    x <- c("1AM","2AM","3AM","4AM","5AM","6AM","7AM","8AM","9AM","10AM","11AM","12PM","1PM","2PM","3PM","4PM","5PM","6PM","7PM","8PM","9PM","10PM","11PM")
    if(input$betn == TRUE)
    {
    StopName2 <- input$DestRoute
    PlotData2 <- DayAndStopname[DayAndStopname$`Stop Name Short` == StopName2,]
    y2 <- as.numeric(PlotData2[, 2:24])
    hc <- highchart() %>%
      hc_add_series(data = round(y, 4), 
                    x= x,
                    type = "line",
                    name = "Average Origin Passenger Traffic",
                    color = "#ADA1EA", showInLegend = TRUE)%>%
      hc_xAxis(categories = x,
               tickmarkPlacement = "on",
               opposite = TRUE)%>%
      hc_add_theme(hc_theme_elementary())%>% 
      hc_title(text = paste("Average Hourly Passenger Traffic")) %>%
      hc_subtitle(text = "The Pattern Of Traffic Over The Day") %>%
      hc_add_series(data = round(y2, 4), 
                    x= x,
                    type = "line",
                    name = "Average Destination Passenger Traffic",
                    color = "#FDA1EA", showInLegend = TRUE)
    }else{
      hc <- highchart() %>%
        hc_add_series(data = round(y, 4), 
                      x= x,
                      type = "line",
                      name = paste("Average Passenger Traffic At",StopName),
                      color = "#ADA1EA", showInLegend = TRUE)%>%
        hc_xAxis(categories = x,
                 tickmarkPlacement = "on",
                 opposite = TRUE)%>%
        hc_add_theme(hc_theme_elementary())%>% 
        hc_title(text = paste("Average Hourly Passenger Traffic At", StopName)) %>%
        hc_subtitle(text = "The Pattern Of Traffic Over The Day")
    }
    
  })
  
  output$container2 <- renderHighchart({
    StopName <- input$Stop
    PlotData <- StopnameCount[StopnameCount$`Stop Name Short` == StopName,]
    x <- colnames(PlotData[, 2:8])
    y <- as.numeric(PlotData[, 2:8])
    if(input$betn == TRUE)
    {
    stopname2 <- input$DestRoute 
    PlotData2 <- StopnameCount[StopnameCount$`Stop Name Short` == stopname2,]
    y1 <- as.numeric(PlotData2[, 2:8])
    hc <- highchart() %>%
      hc_add_series(data = round(y, 4), 
                    x= x,
                    type = "column",
                    name = "Average Origin Passenger Traffic",
                    color = "#ADA1EA", showInLegend = TRUE)%>%
      hc_xAxis(categories = x,
               tickmarkPlacement = "on",
               opposite = TRUE) %>%
      hc_add_theme(hc_theme_elementary())%>% 
      hc_title(text = paste("Average Weekly Passenger Traffic")) %>%
      #hc_subtitle(text = "Analyzing the Pattern Of Traffic Over The Week")%>%
      hc_add_series(data = round(y1, 4), 
                    x= x,
                    type = "column",
                    name = "Average Destination Passenger Traffic",
                    color = "#FDA1EA", showInLegend = TRUE)
    }
    else(
      hc <- highchart() %>%
        hc_add_series(data = round(y, 4), 
                      x= x,
                      type = "column",
                      name = paste("Average Passenger Traffic At",StopName),
                      color = "#ADA1EA", showInLegend = TRUE)%>%
        hc_xAxis(categories = x,
                 tickmarkPlacement = "on",
                 opposite = TRUE) %>%
        hc_add_theme(hc_theme_elementary())%>% 
        # modified by Shailesh for dynamic labels at the histogram output
        hc_title(text = paste("Average Weekly Passenger Traffic At", StopName))
          #hc_subtitle(text = "Analyzing The Pattern of Traffic Over The Week")
        #end
      
    )
    
    
  }) 
  
  output$forecast <- renderHighchart({
    StopName <- input$Stop
    PlotData <- forecastData[forecastData$Stop.Name.Short== StopName,]
    x <- colnames(PlotData[, 2:length(PlotData)])
    y <- as.numeric(PlotData[, 2:length(PlotData)])
    if(input$betn == TRUE)
    {
    stopname2 <- input$DestRoute
    PlotData2 <- forecastData[forecastData$Stop.Name.Short== stopname2,]
    y2 <- as.numeric(PlotData2[, 2:length(PlotData)])
    hc <- highchart() %>%
      hc_add_series(data = round(y, 4), 
                    x= x,
                    type = "line",
                    name = "Average Origin Passenger Traffic",
                    color = "#ADA1EA", showInLegend = TRUE)%>%
      hc_xAxis(categories = x,
               tickmarkPlacement = "on",
               opposite = FALSE) %>%
      hc_add_theme(hc_theme_elementary())%>% 
      hc_title(text = paste("Average Origin Passenger Traffic At",StopName ," And Destination Passenger Traffic At", stopname2," Over The Years")) %>%
      hc_subtitle(text = "Traffic Pattern For Each Week Of Year")%>%
      hc_add_series(data = round(y2, 4), 
                    x= x,
                    type = "line",
                    name = "Average Destination Passenger Traffic",
                    color = "#FDA1EA", showInLegend = TRUE)
    }
    else
    {
      hc <- highchart() %>%
        hc_add_series(data = round(y, 4), 
                      x= x,
                      type = "line",
                      name = "Average Origin Passenger Traffic",
                      color = "#ADA1EA", showInLegend = TRUE)%>%
        hc_xAxis(categories = x,
                 tickmarkPlacement = "on",
                 opposite = FALSE) %>%
        hc_add_theme(hc_theme_elementary())%>% 
        hc_title(text = paste("Average Passenger Traffic At", StopName," Over The Years")) %>%
        hc_subtitle(text = "Traffic Pattern For Each Week Of Year")
      }
  })
  
  output$forecast1 <- renderHighchart({
    #StopName <- input$Stop
    StopName4 <- input$RouteID_input
    PlotData <- forecastData[forecastData$Stop.Name.Short== StopName4,]
    x <- colnames(PlotData[, 2:length(PlotData)])
    y <- as.numeric(PlotData[, 2:length(PlotData)])
      hc <- highchart() %>%
        hc_add_series(data = round(y, 4), 
                      x= x,
                      type = "line",
                      name = paste("Average Passenger Traffic At",StopName4),
                      color = "#ADA1EA", showInLegend = TRUE)%>%
        hc_xAxis(categories = x,
                 tickmarkPlacement = "on",
                 opposite = FALSE) %>%
        hc_add_theme(hc_theme_elementary())%>% 
        hc_title(text = paste("Average Passenger Traffic At", StopName4," Over The Years")) %>%
        hc_subtitle(text = "Traffic Pattern For Each Week Of Year")
    
  })
  
  
  output$container6 <- renderHighchart({
    Suburb2 <-input$Suburb_input
    PlotData <- SuburbModeYeardsCount[SuburbModeYeardsCount$Suburb.Name == Suburb2,]
    PlotData1 <- SubModeCount[SubModeCount$`Suburb Name` == Suburb2,]
    x_pie <- colnames(PlotData1[, 2:4])
    y_pie <- as.numeric(PlotData1[, 2:4])
    x_pie <- factor(x_pie, levels = c("2", "1", "3"), labels = c("Train", "Bus", "Tram"))
    x <- c("2015","2016","2017","2018")
    y1 <- as.numeric(PlotData[PlotData$Mode == 1, 3:length(PlotData)])
    y2 <- as.numeric(PlotData[PlotData$Mode == 2, 3:length(PlotData)])
    y3 <- as.numeric(PlotData[PlotData$Mode == 3, 3:length(PlotData)])
    hc <- highchart() %>%
      hc_add_series_labels_values(x_pie, y_pie, name = "Total Contribution Of Service",
                                  colorByPoint = TRUE, type = "pie",showInLegend = FALSE)%>%
      hc_add_theme(hc_theme_elementary())%>%
      hc_add_series(data = round(y1, 4),x= x,type = "line",
                    name = "Bus",color = "#F0A1EA", 
                    showInLegend = TRUE)%>%
      hc_xAxis(categories = x,
               tickmarkPlacement = "on",
               opposite = TRUE) %>%
      hc_add_theme(hc_theme_elementary())%>% 
      hc_title(text = paste("Yearly Public Transport Traffic At", Suburb2))
    hc <- hc %>% hc_add_series(data = round(y2, 4),x= x,type = "line",
                               name = "Train",color = "#AEE1EA", 
                               showInLegend = TRUE)%>%
      hc_add_series(data = round(y3, 4),x= x,
                    type = "line", name = "Tram",color = "#DDD1EA",
                    showInLegend = TRUE)%>%
      hc_plotOptions(
        pie = list(
          colorByPoint = TRUE, center = c('10%', '12%'),
          size = 100, dataLabels = list(enabled = FALSE)
        ))
    })
  output$container7 <- renderHighchart({
    Suburb2 <-input$Suburb_input
    PlotData <- SuburbCardGroup[SuburbCardGroup$Suburb.Name == Suburb2,]
    x <- c("2015", "2016", "2017", "2018")
    y1 <- as.numeric(PlotData[PlotData$MI.Card.Group == "Full Fare", 3:length(PlotData)])
    y2 <- as.numeric(PlotData[PlotData$MI.Card.Group == "Other", 3:length(PlotData)])
    y3 <- as.numeric(PlotData[PlotData$MI.Card.Group == "Other Concession", 3:length(PlotData)])
    y4 <- as.numeric(PlotData[PlotData$MI.Card.Group == "Student", 3:length(PlotData)])
    y5 <- as.numeric(PlotData[PlotData$MI.Card.Group == "Tertiary", 3:length(PlotData)])
    k <- y1+y2+y3+y4+y5
    hc <- highchart() %>%
      hc_add_series(data = round(y1*100/k, 2),x= x,type = "column",
                    name = "Full Fare",color = "#F0A1EA", 
                    showInLegend = TRUE)%>%
      hc_xAxis(categories = x,
               tickmarkPlacement = "on",
               opposite = TRUE) %>%
      hc_add_theme(hc_theme_elementary())%>% 
      hc_title(text = paste("Yearly Percentge of Myki Card Categories At ", Suburb2))
    hc <- hc %>% hc_add_series(data = round(y2*100/k, 2),x= x,type = "column",
                               name = "Other",color = "#AEE1EA", 
                               showInLegend = TRUE)%>%
      hc_add_series(data = round(y3*100/k, 2),x= x,
                    type = "column", name = "Other Concession",color = "#DDD1EA",
                    showInLegend = TRUE)%>%
      hc_add_series(data = round(y4*100/k, 2),x= x,
                    type = "column", name = "Student",color = "#CCC1EA",
                    showInLegend = TRUE)%>%
      hc_add_series(data = round(y5*100/k, 2),x= x,
                    type = "column", name = "Tertiary",color = "#1EB1EA",
                    showInLegend = TRUE)%>% 
      hc_plotOptions(column = list(
        dataLabels = list(enabled = FALSE),
        stacking = "normal",
        enableMouseTracking = TRUE)) %>%
      hc_yAxis(
        title = list(text = "Percentage of Card Category"),
        labels = c(0:100), max = 100)
      })
  
  
  output$Suburbs <- renderUI({
    Region1 <- input$Region
    k1 <-SuburbandRegion[SuburbandRegion$RegionName == Region1, ]
    selectInput("Suburb", 
                "Select  Suburb",
                k1$SuburbName
    )
  })
  output$RouteID <- renderUI({
    Suburb1 <- input$Suburb
    unique(ModeandStop$`Stop Name Short`)
    k4 <-SuburbandRegion[SuburbandRegion$SuburbName == Suburb1,]
    k3 <-stopnameandPOstID[stopnameandPOstID$PostCode == k4$PostCode,]
    k5 <-ModeandStop[ModeandStop$`Stop Name Short` %in% k3$StopNameShort, ]
    tp <- input$tmode
    k5 <-k5[k5$Mode %in% tp, ]
    selectInput("Stop", 
                paste("Select Origin Stop:" ),
                na.omit(k5$`Stop Name Short`))
  })
  
  output$Suburb_input <- renderUI({
    Region1 <- input$Region_input
    mv1 <-SuburbandRegion[SuburbandRegion$RegionName == Region1, ]
    selectInput("Suburb_input", 
                "Select Suburb",
                mv1$SuburbName)
  })
  output$RouteID_input <- renderUI({
    Suburb1 <- input$Suburb_input
    print(Suburb1)
    mv2 <-SuburbandRegion[SuburbandRegion$SuburbName == Suburb1,]
    mv3 <-stopnameandPOstID[stopnameandPOstID$PostCode == mv2$PostCode,]
    mv4 <-ModeandStop[ModeandStop$`Stop Name Short` %in% mv3$StopNameShort, ]
    selectInput("RouteID_input", 
                paste("Select Stop:" ),
                na.omit(mv4$`Stop Name Short`)
    )
  }) 
  
  output$DestSuburbs <- renderUI({
    mv1 <-SuburbandRegion
    if(input$betn==TRUE)
    {
      selectInput("DestSuburb", 
                "Select Destination Suburb",
                mv1$SuburbName)
      }
  })
  
  output$DestRoute <- renderUI({
    Suburb1 <- input$DestSuburb
    unique(ModeandStop$`Stop Name Short`)
    k4 <-SuburbandRegion[SuburbandRegion$SuburbName == Suburb1,]
    k3 <-stopnameandPOstID[stopnameandPOstID$PostCode == k4$PostCode,]
    k5 <-ModeandStop[ModeandStop$`Stop Name Short` %in% k3$StopNameShort, ]
    tp <- input$tmode
    k5 <-k5[k5$Mode %in% tp, ]
    if(input$betn==TRUE)
    {
    selectInput("DestRoute", 
                paste("Select Destination Stop:" ),
                na.omit(k5$`Stop Name Short`))
    }
      })
  
  output$MapPlot1 <- renderLeaflet({
    Suburb1 <- input$Suburb_input
    sub_traffic <- suburbVsStopVsYear
    names(sub_traffic) <- c("Suburb","Stop_Name","Lat","Long","2015","2016","2017","2018")
    sub_traffic <- gather(sub_traffic, Year, count, '2015':'2018', factor_key=TRUE)
    sub_traffic <- sub_traffic[sub_traffic$Suburb == Suburb1, ]
    sub_traffic1 <- na.omit(sub_traffic)
    pal <- colorNumeric("Darkred", sub_traffic1$count)
    sub_traffic <- sub_traffic1[sub_traffic$count > mean(sub_traffic1$count),]
    map <-leaflet(sub_traffic) %>% addTiles() %>%
      addCircles(lng = ~Long, lat = ~Lat, weight = 1,
                 radius = ~sqrt(count) * 0.5, popup = ~ paste(Stop_Name, "| Average count:", count), group = "circles",color =  ~pal(count),opacity = ~count
      )%>% addProviderTiles("OpenStreetMap")%>%
      addLayersControl(overlayGroups = c("Stops with more than average touch ons"))
  })
  
  output$mytable = DT::renderDataTable({
    Suburb1 <-input$Suburb_input
    suburbVsStopVsYear[suburbVsStopVsYear$Suburb.Name == Suburb1, c(1,2,5,6,7,8)]
  })

})

