
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(lubridate)
library(leaflet)
library(leaflet.extras)
library(EnvStats)
library(DT)
library(ggplot2)
library(dplyr)
library(plotly)
library(sf)
library(sp)
library(rgeos)
library(rgdal)

library(dataRetrieval)



# |-------------- Data reading & processing ------------|



US_states <- read.csv('US_StateCodes.csv') 

month_names <- c('Jan', 
                 'Feb', 
                 'Mar', 
                 'Apr',
                 'May', 
                 'June', 
                 'July',
                 'Aug', 
                 'Sep', 
                 'Oct', 
                 'Nov', 
                 'Dec'
)


# |-------------- Setting up the UI ------------|

header <- dashboardHeader(title = "WQP Data Pull")

sidebar <- dashboardSidebar(
  ##### Selecting Data - This downloads from the WQP when changed and may lag.###
  
  # select date range that goes from 4 years ago to today
  dateRangeInput('dateRange',
                 label = 'Date Range',
                 start = Sys.Date() - 2190, end = Sys.Date()
  ),
  
  pickerInput(
    inputId = "states",
    label = "State",
    choices = US_states$state,
    selected = "OR",
    options = list(
      `actions-box` = TRUE,
      size = 10,
      `selected-text-format` = "count > 3"),
    multiple = FALSE),
  
  # Narrowing Data by HUC8 code
  uiOutput('huc_selection')
)

body <- dashboardBody(
  
  ##### Adjusting Visible Data ####
  fluidRow(
    column(6, sidebarLayout(
      sidebarPanel(width = 12, id = 'data_filtering',
                   h3('For reporting select a minimum of 4 years.'),
                   uiOutput('year_selection'),
                   h4('Select months you apply water to the havestable portion of the crop.'),
                   pickerInput(
                     inputId = "months",
                     label = "Months",
                     choices = month_names,
                     selected = month_names,
                     options = list(
                       `actions-box` = TRUE,
                       size = 10,
                       `selected-text-format` = "count > 3"),
                     multiple = TRUE),
                   fluidRow(
                     column(5, offset = 0,
                            uiOutput('sample_selection')
                            # numericInput('sample_num', "Min. sample # in each year.",
                            #              value = 0))
                     # fluidRow(
                     #   column(5, offset = 0,
                     #          br(),
                     #          actionButton('minsample', 'Enter')

                     )
                   
                   )), mainPanel(width =0))
    )),
  ##### Adding/Removing Markers from maps ####
    # column(6, sidebarLayout(
    #   sidebarPanel(width = 10, id = 'location_input',
    #                h3('Add Water Source To Map'),
    #                textInput('location_name', 'Source', value = NULL),
    #                fluidRow(
    #                  column(5, offset = 0,
    #                         numericInput('lat', 'Latitude', value = NULL)),
    #                  fluidRow(
    #                    column(5, offset = 0,
    #                           numericInput('lon', 'Longitude', value = NULL)
    #                    )
    #                  ),
    #                  column(6,
    #                         actionButton('marker', 'Add Location'),
    #                         actionButton('clear_markers', 'Clear Locations')
    #                  )
    #                )), mainPanel(width =0))
    # )),
  ##### Individual Panels ####
  tabsetPanel(type = "tabs", id = 'tabs', 
              ##### Selected State Data - reactive to narrowing of visable data ####
              tabPanel(value = "data", title = "Water Quality Data",
                       br(),
                       uiOutput('state_download_button'),
                       br(),
                       uiOutput('DataDisplay')
              ),
              
              ##### Station Information ######
              tabPanel(value = "maptab", title = "Sampling Station Information",  
                       fluidRow(
                         column(6, span(textOutput('instructions'), style="font-size: 15px;font-weight:bold"),
                                        uiOutput('MapDisplay')
                                        ),
                         ##### Adding/Removing Markers from maps ####
                         column(6, br(),sidebarLayout(
                           sidebarPanel(width = 10, id = 'location_input',
                                        h3('Add Water Source To Map'),
                                        textInput('location_name', 'Source', value = NULL),
                                        fluidRow(
                                          column(5, offset = 0,
                                                 numericInput('lat', 'Latitude', value = NULL)),
                                          fluidRow(
                                            column(5, offset = 0,
                                                   numericInput('lon', 'Longitude', value = NULL)
                                            )
                                          ),
                                          column(6,
                                                 actionButton('marker', 'Add Location'),
                                                 actionButton('clear_markers', 'Clear Locations')
                                          )
                                        )), mainPanel(width =0)),
                           uiOutput('highvalue'),
                           actionButton('thresholdvalue', 'Enter')
                         )),
                       br(), 
                       span(textOutput('station'), style="font-size: 20px;font-weight:bold"),
                       span(textOutput('station_name'), style="font-size: 15px;font-weight:bold"),
                       span(textOutput('GM'), style="font-size: 15px;font-weight:bold"),
                       span(textOutput('STV'), style="font-size: 15px;font-weight:bold"),
                       span(textOutput('n_ecoli'), style="font-size: 15px;font-weight:bold"),
                       br(),
                       uiOutput('download_button'),
                       br(),
                       uiOutput('stationdata'),
                       br(),
                       plotlyOutput("Stationplotly"))
              # ,
              
              ##### High Events ######
              # tabPanel(value = "hightab", title = 'High E. coli Events', 
              #          uiOutput('highvalue'),
              #          span(textOutput('instructions_high'), style="font-size: 15px;font-weight:bold"),
              #          br(),
              #          plotlyOutput("highevents"), 
              #          br(),
              #          textOutput('plotlyclick'),
              #          br(),
              #          span(textOutput('selected_event'), style="font-size: 15px;font-weight:bold"),
              #          uiOutput('reset'), 
              #          br(), 
              #          leafletOutput('highMap'),
              #          br(), 
              #          uiOutput('csv'), 
              #          br(),
              #          DT::dataTableOutput('selected_day_data')
              #)
              #####
  )

)


ui <- dashboardPage(header, sidebar, body)

server <- function(input, output) {
  
  # find state code based on selected state 
  statecode <- reactive({ 
    code <- US_states$code[US_states$state == input$states] 
    if(code < 10){
      # puts single digit codes into appropriate format for WQP dataset 
      code <- paste0("0", code)
    }
    return(code)
    })

  # Rules for adjusting E. coli in dataset 
  # 1. If a test listed a detection limit with no result or the result was listed below the detection limit, a value of half the detection limit was inserted.
  # Detection Types
  # "Minimum Reporting level"
  # "Lower Reporting Limit"
  # "Elevated Detection Limit"
  # "Upper Reporting Limit"
  
  # 2. If a result was reported as above the quantification limit the result was replaced with twice that of the detection limit.
  
  # 3. If no result or limit was reported, the sample was removed from the set.
  # detection_test <- detection_test[!(is.na(test$ResultMeasureValue) & is.na(test$Results)),]
  # detection_test
  
  # r = result, dl = detection limit
  
  ##### This function corrects reported E.coli using the above rules 1-3 ####
  detection_limit <- function(Result, DetectionValue, DetectionType){
    if (grepl("Minimum", DetectionType, fixed = TRUE) | grepl("Lower", DetectionType, fixed = TRUE)){
      if (is.na(Result) | Result < DetectionValue){
        return(0.5*DetectionValue)
      }else{return(Result)}
    }
    else if (grepl("Elevated", DetectionType, fixed = TRUE) | grepl("Upper", DetectionType, fixed = TRUE)){
      if (is.na(Result) | Result > DetectionValue){
        return(2*DetectionValue)
      }else{return(Result)}
    }else{return(Result)}
    
  }



# merging and cleaning data
  all_data <- eventReactive(input$states,{
    # site data for a state 
    site_data <- whatWQPsites(statecode=paste("US", statecode(), sep = ":"), characteristicName="Escherichia coli",
                             startDateLo = input$dateRange[1],
                             startDateHi = input$dateRange[2])
    
    # if there is no data for that state a df is produced with only one col called "NA." 
    # if this is the case it stops trying to process the data
    # if(length(names(site_data)) > as.integer(1)){
    if("NA." %in% names(site_data)){
       return(NULL)
    }else {
      # finish processing the data
      site_data <- site_data %>%
        # numbering station locations (finds locations that have been named multiple times)
        mutate(LatitudeMeasure = trunc(as.numeric(LatitudeMeasure)*10^4)/10^4,
               LongitudeMeasure = trunc(as.numeric(LongitudeMeasure)*10^4)/10^4) %>%
        mutate(latlon = LatitudeMeasure+LongitudeMeasure) %>%
        group_by(latlon) %>%
        arrange(latlon) %>%
        mutate(station_num = group_indices()) %>%
        ungroup()
      
      ecoli_data <- readWQPdata(statecode=paste("US", statecode(), sep = ":"), characteristicName="Escherichia coli",
                                startDateLo = input$dateRange[1],
                                startDateHi = input$dateRange[2]) %>%
        filter(ActivityMediaSubdivisionName == "Surface Water")
      
      # merges the station info into the reported Ecoli results
      return(merge(site_data, ecoli_data, by = "MonitoringLocationIdentifier", all.y = TRUE))
    }    
  })
  
  
  cleaned_data <- eventReactive(input$states,{
    if(nrow(all_data()) == 0 ){
      return()
    }else{
      cleaned_data <- all_data()%>%
        select(StationID = MonitoringLocationIdentifier,
               StationName = MonitoringLocationName,
               ActivityIdentifier,
               station_num,
               date = ActivityStartDate,
               time = ActivityStartTime.Time,
               ReportedEcoli = ResultMeasureValue,
               lat = LatitudeMeasure,
               lon = LongitudeMeasure,
               HUC8 = HUCEightDigitCode,
               DetectionValue = DetectionQuantitationLimitMeasure.MeasureValue,
               DetectionType = DetectionQuantitationLimitTypeName)%>%
        mutate(date = as.Date(date), lon = -abs(lon)) %>%
        rowwise() %>%
        # convert the results into only the numeric value reported (cleans out any extra characters)
        mutate(ReportedEcoli = as.numeric(gsub("[^0-9.]", "",  ReportedEcoli))) %>%
        # uses detection limit rules to adjust results
        mutate(Ecoli = if(is.na(DetectionValue) | is.na(DetectionType)){
          ReportedEcoli
          }else{
          detection_limit(ReportedEcoli, DetectionValue, DetectionType)}
            )
       # return the data frame that has all NA values removed  
      return(cleaned_data[!is.na(cleaned_data$Ecoli),])     
    }
    
  })
  
  # creates a list of all HUC codes that are available in the pulled data
  huc_available <- eventReactive(input$states,{
    if(nrow(all_data())== 0){
      return(NULL)
    }else{
      cleaned_data() %>% select(HUC8) %>% unique()}
    })

  # Creates a drop down menu with the available huc codes
  output$huc_selection <- renderUI({
    if(is.null(huc_available())){
      return()
    }else{
      pickerInput(
        inputId = "huc8",
        label = "Filter by watershed HUC8 code.",
        choices = huc_available()$HUC8,
        selected = NULL,
        options = list(
          `actions-box` = TRUE,
          size = 10,
          `selected-text-format` = "count > 3"),
        multiple = TRUE)
    }
  })
  
  # pulls the range of year given the data that was pulled from WQP site
  years <- reactive({
    c(year(input$dateRange[1]): year(input$dateRange[2]))})
  
  # generates a slider to allow for narrowing of years after the data is downloaded 
  output$year_selection <- renderUI({
    sliderInput("year", "Year:",
                min = years()[1],
                max = years()[length(years())],
                value=range(years()),
                sep = "", 
                step = 1)
    
  })
  
  samples <- reactive({
    if(nrow(all_data()) == 0){
      return()
    }else{
    num_samples <- cleaned_data() %>% 
      group_by(station_num, year(date)) %>%
      count()
    return(num_samples$n)
    }
  })
  
  output$sample_selection <- renderUI({
    sliderInput("sample_num", "Minimum number of samples in each year", 
                min = 1, 
                max = max(samples()), 
                value = 1, 
                step = 1.0)
  })
  
  # gives the month number
  month_number <- reactive({
    which(month_names %in% input$months)
    
  })
  

  # narrows the pulled data by HUC, year, and month
  narrowed_data <- reactive({
    if(is.null(cleaned_data()) | is.null(input$year) | is.null(month_number())){
      return()
      }else if (is.null(input$huc8)){
      return(cleaned_data() %>% filter(year(date) %in% c(input$year[1]:input$year[2]),
                          month(date) %in% month_number()))
        }else{
          return(cleaned_data() %>% filter(HUC8 %in% input$huc8,
                            year(date) %in% c(input$year[1]:input$year[2]),
                            month(date) %in% month_number()))
    }
  })

  # selected_data with defined number of samples in selected time frame 
  selected_data_nsamples <- reactive({
    # | nrow(narrowed_data())==0
    if(is.null(narrowed_data())){
      return()
    }
    # return nothing if there is no stations with that number of samples 
    else if (nrow(narrowed_data() %>%
                  group_by(station_num, year(date)) %>%
                  filter(n()>= input$sample_num))<1){
      return()
    }
    else{
      return(narrowed_data() %>%
        group_by(station_num, year(date)) %>%
        filter(n()>= input$sample_num) %>%
        ungroup())
    }
  })
  
  ##### Water Quality Data Tab ######

  # what to display on the first tab 
  output$DataDisplay <- renderUI({
    if(nrow(all_data()) == 0){
      textOutput("nodata")
    }else {
      DT::dataTableOutput('selected_data')
    }
  })
  
  # 
  output$nodata <- renderText({
    "No data found for selected state."
  })
  
  # generation of a download button that will allow users to obtain csv file of selected data
  output$state_download_button<- renderUI({
    if(nrow(all_data()) == 0){
      return()
    }else{
      downloadButton('statedata', paste('Download Selected ', input$states, ' Data'))
    }
  })

  
  # downloading a csv of state information
  output$statedata <- downloadHandler(
    filename = function(){
      paste(input$state,".csv")
    },
    content = function(file){
      write.csv(narrowed_data(), file)
    }
  )
  
  output$selected_data <- DT::renderDataTable({
    if(is.null(selected_data_nsamples())){
      return()
    }else{
      TableData <- selected_data_nsamples() %>%
        select(HUC8,
               StationID,
               StationName,
               ActivityID = ActivityIdentifier,
               Latitude = lat,
               Longitude = lon,
               Date = date,
               TestingTime = time,
               ReportedEcoli,
               DetectionValue,
               DetectionType,
               CorrectedEcoli = Ecoli)
    }
  })
  
  ##### Sampling Station Information Tab ####
  
  # this will ensure that the map is not rendered until the tab is clicked 
  observeEvent(input$tabs, {
      if(input$tabs == "maptab") {
  
  ##### Text Output and Map ####
        
  output$highvalue<- renderUI({
          numericInput('threshold', "To see only stations that have high test results enter a E.coli threshold value (CFU/100 ml).",
                       value = 0)
        })
  
  output$MapDisplay <- renderUI({
    if(nrow(all_data()) == 0){
      textOutput("nodata")
    }else {
      leafletOutput("station_map")
    }
  })

  output$instructions <- renderText({
    if(is.null(selected_data_nsamples())){
      "No data found for selected state."}
    else{
      'Click on a station for a station report.'
    }

  })


  # map of all the stations in the selected watershed
  output$station_map <- renderLeaflet({

    if(nrow(all_data()) == 0 | nrow(cleaned_data()) == 0){
      return()
    }else{
      # using the cleaned data as a base without using the narrowed or selected data
      cleaned_data() %>%
        select(StationID, station_num, lat, lon, HUC8) %>%
        unique() %>%
        leaflet() %>%
        addProviderTiles('Esri.NatGeoWorldMap') %>%
        addCircleMarkers(lat = ~lat, lng=~lon,
                         label = ~StationID,
                         layerId = ~station_num,
                         radius = 3,
                         group = 'original') %>%
        addResetMapButton()
    }
  })

  ##### Events to Observe in the Station Information Tab ####

  # stations in selected watershed that comply with the user inputs (will not re-render the map only changes the markers)
  observeEvent(list(input$year, month_number(), input$sample_num, input$huc8, input$tabs, input$thresholdvalue),
               {
                 # resenting the station information asssigned to selecting a station 
                 click$clickedMarker <- NULL
                 
                 # if no stations exist that have the required number of samples produce a blank map 
                if(is.null(selected_data_nsamples())){
                   leafletProxy("station_map", data = all_data()) %>%
                     clearGroup('original') %>%
                     clearGroup('Stations Meeting Criteria') 
                 }else{
                   if(is.null(input$threshold)){
                     map_data <- selected_data_nsamples() %>%
                       select(StationID, station_num, lat, lon, HUC8 ) %>%
                       unique()
                   }else{
                     map_data <- selected_data_nsamples() %>%
                       filter(Ecoli >= input$threshold) %>%
                       select(StationID, station_num, lat, lon, HUC8 ) %>%
                       unique()
                   }
                   leafletProxy("station_map", data = map_data) %>%
                     clearGroup('original') %>%
                     clearGroup('Stations Meeting Criteria') %>%

                     addCircleMarkers(map_data,
                                      lng = ~lon,
                                      lat = ~lat,
                                      label = ~StationID,
                                      clusterId = 'stations',
                                      layerId = ~station_num,
                                      radius = 3,
                                      color = 'blue',
                                      group = 'Stations Meeting Criteria') 

               }
               })


  input_point <- reactive({
    data.frame(Longitude = input$lon,
               Latitude =input$lat,
               names = input$location_name)
  })

  observeEvent(input$marker,{
    
    stationmap <- leafletProxy("station_map")
    
    if(is.na(input$lat) | is.na(input$lon)) {
      stationmap
      
    }else{
      
      location <- input_point()
      coordinates(location) <- ~Longitude + Latitude
      
      
      stationmap %>% addMarkers(input$lon, input$lat,
                                label = input$location_name,
                                group = 'locations',
                                popup = paste(input$lat, ',', input$lon))
    }
  })
  
  # Clearing the markers
  observeEvent(input$clear_markers, {
    
    
    stationmap <- leafletProxy("station_map")
    
    stationmap %>%
      clearGroup('locations')
    
  })
  
  
  ##### Selecting a Stations (click) #####

  # initation of station selection variable
  click <-  reactiveValues(clickedMarker=NULL)

  # assigning a station when it is clicked
  observeEvent(input$station_map_marker_click,
               {if(input$station_map_marker_click$id == 'location'){
                 return()
               }else{
                 click$clickedMarker <- input$station_map_marker_click$id}
               })

  # clearing that informtion when looking at a new map
  observeEvent(list(input$states, input$search), {
    click$clickedMarker <- NULL
  })

  ##### Caclulations and Text outputs ####
  # generation of a download button that will allow users to obtain report for the station they selected
  output$download_button <- renderUI({
    if(is.null(click$clickedMarker)){
      return()
    }else{
      downloadButton('report', 'Download Station Report')
    }
  })

  # the station number of the selected station
  selected_station <- reactive({
    if(is.null(click$clickedMarker)){return()}
    else{
      stations <- cleaned_data() %>%
        filter(station_num == click$clickedMarker) %>%
        select(StationID, StationName) %>%
        unique()


    }
  })

  # the geometric mean of all samples at the selected station in the defined time frame
  gm <- reactive({
    if(is.null(click$clickedMarker)){return()}
    else{
      stations <- selected_data_nsamples() %>%
        filter(station_num == click$clickedMarker)

      round(geoMean(stations$Ecoli), 2) 
    }

  })

  # the statistical threshold value of all samples at the selected station in the defined time frame
  stv <- reactive({
    if(is.null(click$clickedMarker)){return()}
    else{
      stations <- selected_data_nsamples() %>%
        filter(station_num == click$clickedMarker)

      round(10**(mean(log10(stations$Ecoli)) +1.282*sd(log10(stations$Ecoli))), 2)
    }
  })

  # the number of samples taken at that station in the defined time frame (also the number of samples used to calc gm and stv)
  n_samples <- reactive({
    if(is.null(click$clickedMarker)){return()}
    else{
      stations <- selected_data_nsamples() %>%
        filter(station_num == click$clickedMarker)

      nrow(stations)

    }
  })

  # ouptut text that displays the chosen station ID
  output$station <- renderText({
    if(is.null(click$clickedMarker)){return()}
    else{
      paste("Station ID: ", selected_station()$StationID)
    }
  })

  # ouptut text that displays the chosen station name
  output$station_name <- renderText({
    if(is.null(click$clickedMarker)){return()}
    else{
      paste("Station Name: ", selected_station()$StationName)
    }
  })

  # displays the GM calculation
  output$GM <- renderText({
    if(is.null(click$clickedMarker)){return()}
    else{
      paste("Geometric Mean: ", gm())
    }

  })

  # displays the STV calculation
  output$STV <- renderText({
    if(is.null(click$clickedMarker)){return()}
    else{
      paste("Statistical Threshold Value (STV): ", stv())
    }

  })

  # displays the number of samples
  output$n_ecoli <- renderText({
    if(is.null(click$clickedMarker)){return()}
    else{
      paste("Number of Samples: ", n_samples())
    }
  })
# 
  ##### Scatter Plot and Report Output ####
#   # scatter plot of all samples in the defined time range at the selected station
  output$Stationplotly <- renderPlotly({

    if(is.null(selected_data_nsamples()) |  nrow(selected_data_nsamples()) == 0 | is.null(click$clickedMarker)){
      return()}
    else{
      # station_scatter()
      p <- selected_data_nsamples() %>%
        filter(station_num == click$clickedMarker) %>%
        plot_ly(x = ~date, y = ~Ecoli,
                type = "scatter",
                mode = "markers",
                hovertemplate = paste("E. coli: %{y} <br>Date: %{x}"),
                name = 'E. coli Results') %>%
        layout(xaxis = list(type = 'date',
                            tickformat = "%d %B <br>%Y",
                            title = 'Date'),
               yaxis = list(title = 'E. coli Results (CFU/100 ml)'))
      return(p)
      }
  })
  
  # generation of a download button that will allow users to obtain csv file with station data
  output$stationdata<- renderUI({
    if(is.null(click$clickedMarker)){return()
      }else{
      downloadButton('stationcsv','Download Stations Data')
    }
  })
  
  stationdatadownload <- reactive({
    selected_data_nsamples() %>%
      filter(station_num == click$clickedMarker) %>% 
      ungroup() %>% 
     select(-station_num)
  })
  
  
  # downloading a csv of table information
  output$stationcsv <- downloadHandler(
    filename = function(){
      paste(selected_station()$StationID,"_ecoli.csv")
    },
    content = function(file){
      write.csv(stationdatadownload(), file)
    }
  )

  output$report <- downloadHandler(
    filename = ("report.pdf"),

    content = function(file){

      tempReport <- file.path(tempdir(), "USStationReport.Rmd")
      file.copy("USStationReport.Rmd", tempReport, overwrite = TRUE)

      # Set up parameters to pass to Rmd document
      params <- list(startdate = input$dateRange[1],
                     enddate = input$dateRange[2],
                     months = input$months,
                     samples = n_samples(),
                     chosensample = input$sample_num,
                     station = selected_station()$StationID,
                     gm = gm(),
                     stv = stv(),
                     station_num = click$clickedMarker,
                     data = selected_data_nsamples(),
                     name = selected_station()$StationName)

      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )

    }
  )

      }

    
  ##### High E. coli Events Tab ####
  #     

  #            
  ##### High Events texts and Scatter Plot ####
  # output$instructions_high <- renderText({
  #   if(is.null(narrowed_data()) | nrow(narrowed_data()) == 0){
  #     return()
  #   }else{
  #     'Click on a high event to see all other E. coli results from that day.'
  #   }
  # }
  # )
  # 
  # 
  # # All events that are above the defined E.coli Measurement threshold
  # high_events <- reactive({
  #   if(is.null(narrowed_data())){
  #     return()
  #   }
  #   else if (nrow(narrowed_data())<1){
  #     return()
  #   }
  #   else{
  #     return(narrowed_data() %>%
  #       filter(Ecoli>= input$threshold) %>%
  #       select(station_num, StationID, StationName, Ecoli, date, lat, lon) %>%
  #       arrange(desc(Ecoli)) %>%
  #       unique()
  #     )
  #   }
  # })
  # 
  # # Scatter plot of all of the high events (given the threshold) in the chosen time range
  # output$highevents <- renderPlotly({
  #   
  #   if(is.null(high_events())){
  #     NULL
  #   }
  #   else{
  #     high_events() %>%
  #       plot_ly(x = ~date, y = ~Ecoli,
  #               type = "scatter",
  #               key = ~station_num,
  #               text = ~StationID,
  #               hovertemplate = paste("Station ID: %{text} <br>E. coli Result: %{y} <br>Date: %{x}"),
  #               source = "click") %>%
  #       layout(xaxis = list(type = 'date',
  #                           tickformat = "%d %B <br>%Y", 
  #                           title = 'Date'), 
  #              yaxis = list(title = 'E. coli Results (CFU/100 ml)'))
  #   }
  # })
  # 
  # 
  # 
  # # station information for a selected high event in the above scatter plot
  # selected_high_event <- reactive({
  #   event_data(event="plotly_click", source = "click")
  # })
  # 
  # output$selected_event <- renderText({ 
  #   if(is.null(selected_high_event())){
  #     return()
  #   }else{
  #     paste('All E. coli measurements recorded on ', selected_high_event()$x, ' are shown on the map below. 
  #           The selected event will appear larger in size than the others. ')
  #   }
  #   })
  # 
  # # a button to reset the map below
  # output$reset <- renderUI({
  #   if(is.null(selected_high_event())){
  #     return()
  #   }else{
  #     actionButton('map_reset', "Reset Map", style='padding:4px; font-size: 15px;font-weight:bold')
  #   }
  # })
  ##### High Events Map ####
# 
#   # map showing all high events displayed in the above scatter plot
#   output$highMap <- renderLeaflet({
#     if(is.null(high_events())){
#       return()
#     }
#     else{
#       return(high_events() %>%
#         select(StationID, StationName, lat, lon, Ecoli) %>%
#         unique() %>%
#         leaflet() %>%
#         addProviderTiles('Esri.NatGeoWorldMap') %>%
#         addCircleMarkers(lat = ~lat, lng=~lon,
#                          label = ~StationName,
#                          popup = paste0('Station ID: ', high_events()$StationID),
#                          radius = 3,
#                          group = 'highstations') %>%
#         addResetMapButton()
#         )
#     }
#   })
# 
#  day_results <- reactive({
#    narrowed_data() %>%
#      filter(date == selected_high_event()$x)
#  })
# 
#  color_pal <- reactive({
#    colorNumeric(palette = c("blue", "red"), domain = c(0: max(day_results()$Ecoli)))
#  })
# 
#   # When a new high event is selected the station it was collected at will be colored red
#   observeEvent(selected_high_event(),
#                {leafletProxy("highMap", data = day_results())%>%
#                    clearGroup('highstations') %>%
#                    clearGroup('Stations') %>%
#                    clearControls() %>%
#                    addCircleMarkers(lat = ~lat, lng= ~lon,
#                                     label = paste0('E.coli Result: ', day_results()$Ecoli),
#                                     popup = paste0('Station ID: ', day_results()$StationID, '<br/>',
#                                                    'E.coli Result: ', day_results()$Ecoli),
#                                     radius = ~ifelse(station_num == selected_high_event()$key, 9, 5),
#                                     color = ~color_pal()(Ecoli),
#                                     fillOpacity = 1,
#                                     stroke = FALSE,
#                                     group = 'Stations'
#                    ) %>%
#                    addLegend(pal = color_pal(), values = c(0: max(day_results()$Ecoli)),
#                              title = 'E. coli Results (CFU/100 ml)',
#                              opacity = 1, group = 'Stations')
# 
#                })
# 
#   # clearing all highlighted stations with the rest_map button is clicked
#   observeEvent(input$map_reset,
#                {leafletProxy("highMap", data = high_events() %>%
#                                select(StationID, StationName, lat, lon) %>%
#                                unique()) %>%
#                    clearMarkers() %>%
#                    clearControls()%>%
#                    addCircleMarkers(lat = ~lat, lng=~lon,
#                                     label = ~StationName,
#                                     popup = paste0('Station ID: ', ~StationID),
#                                     radius = 3)
# 
#                })
  ##### High Events Table and Download ####
#   table_info <- reactive({
#     if(is.null(selected_high_event())){
#       return()
#     }else{
#       day_results() %>%
#         select(Date = date, HUC8, 'Station ID' = StationID,
#                'Station Name' = StationName, Lat = lat, Lon = lon,
#                'E. coli' = Ecoli)
#     }
#   })
# 
#   # generation of a download button that will allow users to obtain csv file of selected data
#   output$csv<- renderUI({
#     if(is.null(table_info())){
#       return()
#     }else{
#       downloadButton('high_event_day_report', paste('Download Data for', selected_high_event()$x))
#     }
#   })
# 
#   # The date of the event selected by the user
#   selected_day <- reactive({
#     str_replace_all(as.character(selected_high_event()$x), '-', '_')
#   })
# 
#   # downloading a csv of table information
#   output$high_event_day_report <- downloadHandler(
#     filename = function(){
#       paste(input$states, '_', selected_day(),".csv")
#     },
#     content = function(file){
#       write.csv(table_info(), file)
#     }
#   )
# 
#   output$selected_day_data <- DT::renderDataTable({
#     if(is.null(selected_high_event())){
#       return()
#     }else{
#       table_info()
#     }
#   })
# 
  })
  
  }

# Run the application
shinyApp(ui = ui, server = server)
