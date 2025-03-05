################################################################################
#                   Author: Joshua Thompson
#   O__  ----       Email:  joshua.thompson@ofwat.gov.uk
#  c/ /'_ ---
# (*) \(*) --
# ======================== Script  Information =================================
# PURPOSE: weather aggregator tool 
#
# PROJECT INFORMATION:
#   Name: weather aggregator tool
#
# HISTORY:----
#   Date		        Remarks
#	-----------	   ---------------------------------------------------------------
#	 30/05/2024    Created script                                   JThompson (JT)
#  05/03/2025    Commented 'sf' and updated local file URLs       ABunning (AB)
#===============================  Environment Setup  ===========================
#==========================================================================================


# Load libraries
library(shiny)
library(leaflet)
library(dplyr)
# library(sf)
library(wesanderson) 
library(shinycssloaders)
library(shinyjs)
library(shinyBS)
library(httr)
library(jsonlite)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
library(extrafont)
library(stringr)
library(RColorBrewer)
library(plotly)
library(DT)
library(Cairo)
library(writexl)

#'[================================================]
#'[================================================]
#'##################################################
# Load data
####################################################
#'[================================================]
#'[================================================]

# Water company data and rainfall stations 
station_sf <- readRDS("/home/OpenDataApp/station_sf_wNRW.rds") %>% filter(!stationReference %in% c("3347"))
OfwatWastewaterBoundary <- readRDS( "/home/OpenDataApp/OfwatWastewaterBoundary.rds")
area_cents.ww <- readRDS("/home/OpenDataApp/area_cents.ww.rds")


#'[================================================]
#'[================================================]
#'##################################################
# Create UI
####################################################
#'[================================================]
#'[================================================]

ui <- fluidPage(
  useShinyjs(),
  # initial Ofwat logo loader 
  tags$head(
    tags$style(HTML("
      #initial_loader {
        position: fixed;
        width: 100%;
        height: 100%;
        background: white;
        display: flex;
        flex-direction: column;
        justify-content: center;
        align-items: center;
        z-index: 10000;
        text-align: center;
      }
      #initial_loader img {
        width: 400px;
        height: auto;
      }
      .loading-bar {
        width: 400px;
        margin-top: 20px;
        height: 20px;
        background-color: #f3f3f3;
        border: 1px solid #ccc;
        border-radius: 5px;
        overflow: hidden;
        position: relative;
      }
      .loading-bar div {
        position: absolute;
        width: 100%;
        height: 100%;
        background-color: #003296;
        animation: loading 2s ease-in-out infinite;
      }
      @keyframes loading {
        0% { left: -100%; }
        50% { left: 0; }
        100% { left: 100%; }
      }
    "))
  ),
  
  div(id = "initial_loader",
      img(src = "Ofwat_logo.png", alt = "Loading..."),
      div(class = "loading-bar",
          div()
      )
  ),
  
  # set up UI style 
  tags$link(rel = "stylesheet", type = "text/css", href = "css/krub.css"),
  tags$head(HTML("<title>Open Data Aggregator Tool</title> <link rel='icon' type='image/gif/png' href='Ofwat_logo.png'>"),
            tags$style(HTML(
              "
              .navbar{background-color: #FFFFFF !important; padding-left: 0px; margin-left:0px; padding-right: 0px; margin-right:0px;padding-top: 0px; margin-top:0px;}
              .navbar-default .navbar-brand:hover {color: blue;}
              .navbar { background-color: gray;}
              .navbar-default .navbar-nav > li > a {color:black;}
              .navbar-default .navbar-nav > .active > a,
              .navbar-default .navbar-nav > .active > a:focus,
              .navbar-default .navbar-nav > .active > a:hover {color: white;background-color: #505250;}
              .navbar-default .navbar-nav > li > a:hover {color: white;background-color:#505250;text-decoration:underline;}
              .butt{background-color:#505250; color:#FFFFFF; border-color:#080808;}
              .btn-file{background-color:#505250; color:#FFFFFF; border-color:#080808;}
              .action-button{background-color:#505250; color:#FFFFFF; border-color:#080808;}
              .radio input{background-color:#505250; color:#FFFFFF; border-color:#080808;}
              input[type='radio'] {filter: saturate(0);}
              input[type='checkbox'] {filter: saturate(0);}
              * {font-family: 'Krub', sans-serif;}
              /*html {overflow:   scroll;}
              ::-webkit-scrollbar {width: 0px; background: transparent; */}
              "
            )), 
            tags$script('
      function copyToClipboard() {
        var textArea = document.createElement("textarea");
        textArea.value = "joshua.thompson@ofwat.gov.uk";
        document.body.appendChild(textArea);
        textArea.select();
        document.execCommand("Copy");
        document.body.removeChild(textArea);
        alert("Email address copied to clipboard!");
      }
    ')),
  
  # define nheader components 
  titlePanel(
    fluidRow(style = "background-color:#ffffff; padding-top: 0px; margin-top:0px; padding-bottom: 20px; margin-bottom:0px",
             column(9,h2(("Open Rainfall Data Aggregator Tool"),style="font-size:26px;font-style:bold; font-weight: 600; color:black;"),
                    p("A tool to aggregate open rainfall data by company area.",style="font-size:18px;font-style:normal; font-weight: 400; color:black;"),
                    a(actionButton(inputId = "email2", label = "   Help!",icon = icon("envelope", lib = "font-awesome"),
                                   style = "background-color:#505250; color:#FFFFFF; border-color:#080808",
                                   onclick = "copyToClipboard();")),
                    a(actionButton(inputId = "github1", label = "  Developer",icon = icon("github", lib = "font-awesome"),
                                   style = "background-color:#505250; color:#FFFFFF; border-color:#080808"),href="https://github.com/joshuajdthompson",target="_blank")),
             column(3, tags$a(img(src='Ofwat_logo.png', align = "right", width = '300px', height = 'auto', style="padding: 0px")))
    )),
  navbarPage("",
             #'*================================================*
             ###################################################
             # Rainfall
             ###################################################
             #'*================================================*
             tabPanel("Rainfall", value = "1", fluid = TRUE,icon=icon("cloud-showers-heavy"),
                      sidebarLayout(
                        sidebarPanel(
                          selectizeInput(inputId = "company", 
                                         label = "Select Company:",
                                         choices = unique(station_sf$COMPANY),
                                         selected = NULL,
                                         multiple = TRUE, 
                                         options = list(maxItems = 1)),
                          selectizeInput("site_selected", "Selected sites:", choices = NULL, multiple = TRUE, options = list(plugins = list('remove_button'))),
                          div(
                            style = "display: flex; align-items: center;",
                            actionButton("apinplot_button", "Generate API call and plot"),
                          ),
                          br(),
                          conditionalPanel(
                            condition = "input.apinplot_button > 0",
                            h4(strong("Download data"), style = "font-size: 22px; font-style: normal; font-weight: 400; color: black;"),
                            radioButtons("dataFormat", "Choose data format:", choices = c("Aggregated by company area" = "aggregated", "Individual gauges in company area" = "raw")),
                            downloadButton("downloadData", "Download data", class = "butt")
                          )
                        ),
                        mainPanel(
                          h4(strong("Map of company areas and rainfall gauges"),style="font-size:22px;font-style:normal; font-weight: 400; color: black;"),
                          leafletOutput("mainmap")%>% withSpinner(color="black"), 
                          br(), 
                          conditionalPanel(
                            condition = "input.apinplot_button > 0",
                            h4(strong("Rainfall plot"), style = "font-size: 22px; font-style: normal; font-weight: 400; color: black;"),
                            plotOutput("rainplot")
                          )
                        )
                      )
                      
             )
  )
)

#'[================================================]
#'[================================================]
#'##################################################
# Create server logic
####################################################
#'[================================================]
#'[================================================]
server <- function(input, output, session) {
  
  # hide initial loader
  observe({
    invalidateLater(3000, session)
    removeUI(selector = "#initial_loader")
  })
  
  # set up reactives to hold data values 
  data_values <- reactiveValues(rainfall = NULL, monthly_rainfall = NULL, temperature = NULL, monthly_temp = NULL,
                                discharge = NULL, monthly_discharge = NULL,annual_rainfallsewer = NULL, rainfallsewer = NULL)
  
  #'*================================================*
  ###################################################
  # Rainfall
  ###################################################
  #'*================================================*
  
  #######################
  # update dropdowns
  #######################
  observeEvent(input$company, {
    area=unique(OfwatWastewaterBoundary %>%
                  dplyr::filter(COMPANY==input$company) %>%
                  pull(AreaServed))
    updateSelectizeInput(session,
                         "areaServed",
                         selected = NULL,
                         choices = area)
  }) 
  
  #===================================================
  # company Map
  #===================================================
  output$mainmap <-renderLeaflet({
    leaflet() %>%
      setView(lng = -0.8556470749832616, lat = 52.55857268224709, zoom = 6) %>%
      addProviderTiles(providers$CartoDB.Positron)%>%
      addPolygons(data = OfwatWastewaterBoundary, 
                  fill = TRUE,
                  color = "black", 
                  weight =1,
                  fillColor = "lightgrey",
                  fillOpacity = 0.35,
                  popup = paste("Company: ", OfwatWastewaterBoundary$COMPANY, "<br>"),
                  highlightOptions = highlightOptions(color = "navy",
                                                      opacity = 0.8,
                                                      weight = 5,
                                                      bringToFront = FALSE),
                  group = "companies")%>%
      addCircles(data = station_sf,
                 radius = 1500,
                 fillColor = "red",
                 fillOpacity = 1,
                 color = "black",
                 weight = 1,
                 stroke = T,
                 popup = paste("Rainfall gauge name: ", station_sf$stationName),
                 highlightOptions = highlightOptions(color = "black",
                                                     opacity = 1.0,
                                                     weight = 3,
                                                     bringToFront = TRUE),
                 group = "gauges") %>%
      addMiniMap(
        tiles = providers$CartoDB.Positron,
        position = 'topright', 
        width = 200, height = 200,
        toggleDisplay = FALSE,
        aimingRectOptions = list(color = "red", weight = 1, clickable = FALSE),
        zoomLevelOffset=-3)
    
  })
  
  # create a proxy of the map
  mainmapProxy <- leafletProxy("mainmap")
  
  observe({
    if(!is.null(input$company)){
      
      #get the selected comany polygons
      selected_area <- OfwatWastewaterBoundary %>% filter(COMPANY==input$company)#%>% head(1)
      selected_gauges <- station_sf %>% filter(COMPANY==input$company)
      
      #remove any previously highlighted polygons
      mainmapProxy %>% clearGroup(group="highlighted_polygon") %>% 
        clearGroup(group="highlighted_points") %>% 
        showGroup("companies") %>% showGroup("gauges")
      
      # get centroid lat and long of selected area polygon
      selected_area_centroids <- area_cents.ww %>% filter(COMPANY==input$company) %>% head(1) 
      
      #add a slightly thicker polygon on top of the selected one
      mainmapProxy %>% 
        addPolylines(stroke=TRUE, weight = 5,color="black",data=selected_area,group="highlighted_polygon") %>% 
        addCircles(stroke=TRUE, radius = 1500, weight = 5,color="black",fillColor = "black",
                   data=selected_gauges,group="highlighted_points",
                   popup = paste("Rainfall gauge name: ", selected_gauges$stationName)) %>% 
        setView(lng=selected_area_centroids$long,lat=selected_area_centroids$lat,zoom=8)
      
      
      #update site input
      updateSelectizeInput(session, "site_selected", choices = selected_gauges$stationName, selected = selected_gauges$stationName)
      
    }
  })
  
  # button generate API call and plot
  observeEvent(input$apinplot_button, {
    req(input$site_selected)  
    
    if(!input$company %in% c("Dŵr Cymru","Hafren Dyfrdwy")){
      showModal(modalDialog(
        title = NULL,
        size = "l",  
        tags$div(
          style = "text-align: center; font-family: 'Krub', sans-serif; font-weight: 600; font-size: 30px;",  
          tags$img(src = "ea_logo.png", alt = "EA Logo", style = "width: 400px; height: auto;"),
          br(),
          "Connecting with the Environment Agency API and aggregating data... please wait",
          br(),br(),
          div(
            class = "loader",
            style = "margin: auto; border: 5px solid #f3f3f3; border-top: 5px solid #009f41; border-radius: 80%; width: 100px; height: 100px; animation: spin 2s linear infinite;"
          ), br(),
        ),
        easyClose = FALSE,
        footer = NULL,
        style = "border: none; box-shadow: none; background-color: rgba(255, 255, 255, 0.5); padding: 0;",
        tags$head(
          tags$style(HTML("
      @keyframes spin {
        0% { transform: rotate(0deg); }
        100% { transform: rotate(360deg); }
      }
    "))
        )
      ))
      stations <- station_sf %>% filter(stationName %in% input$site_selected) %>% pull(wiskiID)
      
      rainfall <- tibble()
      
      for(i in stations){
        url.rain <- paste0("https://environment.data.gov.uk/hydrology/data/readings.json?max-date=2024-04-01&mineq-date=2012-04-01&period=86400&station.wiskiID=",as.character(i))
        
        # send API request
        response.rain <- GET(url.rain)
        
        if (http_status(response.rain)$category == "Success") {
          # parse JSON 
          data.rain <- tryCatch({
            fromJSON(content(response.rain, "text"), flatten = TRUE)
          }, error = function(e) {
            message("Error parsing JSON: ", e)
            return(NULL)
          })
          
          # check if it worked
          if (!is.null(data.rain)) {
            # extract relevant bits we are interested in
            station_data.rain <- data.rain$items
            
            # check if station data is available
            if (!is.null(station_data.rain) && length(station_data.rain) > 0) {
              
              statName <- station_sf %>% filter(wiskiID == i) %>% pull(stationName)
              # make tibble
              station_df.rain <- tibble(
                date = as.Date(station_data.rain$date),
                rain_mm = station_data.rain$value,
                station = statName,
                wiskiID = i,
              ) %>% filter(complete.cases(.))
              
            } else {
              message("No data available for the specified station and date range.")
            }
          } else {
            message("Failed to parse JSON response.")
          }
        } else {
          message("Error: API request failed with status ", http_status(response.rain)$reason)
        }
        rainfall <- bind_rows(rainfall,station_df.rain) 
        
        
      }
      
      # save data to the reactiveValues
      data_values$rainfall <- rainfall
      print(head(data_values$rainfall))
      
      # plot
      output$rainplot <- renderPlot({
        monthly_rainfall <- rainfall %>% filter(complete.cases(.))  %>%
          mutate(year = year(date), month = month(date)) %>%
          group_by(year, month,station) %>%
          summarize(total_rain_mm = sum(rain_mm, na.rm = TRUE), .groups = 'drop') %>%
          ungroup() %>%
          group_by(year, month) %>%
          summarize(average_rain_mm = mean(total_rain_mm, na.rm = TRUE), .groups = 'drop') %>%
          mutate(date = as.Date(paste(year, month, "01", sep = "-"))) %>% 
          filter(complete.cases(.))
        
        # update the reactiveValues
        data_values$monthly_rainfall <- monthly_rainfall
        print(head(data_values$monthly_rainfall))
        
        ggplot(monthly_rainfall %>% mutate(Title = str_wrap(paste0('Average monthly rainfall (mm) observed for ', input$company, ifelse(!is.na(input$areaServed),paste0(' in the ', input$areaServed , ' region'),"")),width=50)), aes(x = date, y = 1, fill = average_rain_mm)) +
          facet_wrap(~Title,ncol=1) +
          geom_tile() +
          scale_x_date(expand = c(0, 0), date_breaks = "1 years", date_labels = "%Y") +
          scale_y_continuous(expand = c(0, 0)) +
          scale_fill_gradientn(colours = brewer.pal(9, "Blues"),
                               guide = guide_colorbar(
                                 barheight = unit(3, units = "mm"),
                                 barwidth = unit(50, units = "mm"),
                                 title.vjust = 1)) +
          labs(x = "Year", y = "",fill = "Monthly Rainfall (mm)") +
          theme(text = element_text(family = "Krub"), 
                axis.text.x = element_text(angle = 0, vjust = 0, hjust=0.5,colour="black"),
                strip.background = element_rect(fill="black"),
                strip.text = element_text(colour = 'white',face="bold",size = 12),
                panel.background = element_blank(),
                panel.grid.major.x = element_line(size = 0.5, linetype = 'solid',
                                                  colour = "lightgrey"),
                panel.border=element_rect(colour="black",size=1,fill=NA),
                axis.text.y=element_blank(),axis.ticks.y = element_blank(),
                legend.position = "bottom")
      }, res = 128)}else{
        showModal(modalDialog(
          title = NULL,
          size = "l",  
          tags$div(
            style = "text-align: center; font-family: 'Krub', sans-serif; font-weight: 600; font-size: 30px;",  
            tags$img(src = "nrw_logo.png", alt = "NRW Logo", style = "width: 400px; height: auto;"),
            br(),
            "Connecting with the Natural Resources Wales API and aggregating data... please wait",
            br(),br(),
            div(
              class = "loader",
              style = "margin: auto; border: 5px solid #f3f3f3; border-top: 5px solid #009f41; border-radius: 80%; width: 100px; height: 100px; animation: spin 2s linear infinite;"
            ), br(),
          ),
          easyClose = FALSE,
          footer = NULL,
          style = "border: none; box-shadow: none; background-color: rgba(255, 255, 255, 0.5); padding: 0;",
          tags$head(
            tags$style(HTML("
      @keyframes spin {
        0% { transform: rotate(0deg); }
        100% { transform: rotate(360deg); }
      }
    "))
          )
        ))
        stations <- station_sf %>% filter(stationName %in% input$site_selected) %>% select(stationReference,wiskiID) %>% as_tibble()
        print(stations)
        rainfall <- tibble()
        
        for(i in 1:nrow(stations)){
          url <- "https://api.naturalresources.wales/rivers-and-seas/v1/api/StationData/historical"
          
          # Define the query parameters (location and parameter)
          params <- list(
            location = stations[i, 1] %>% pull(),
            parameter = stations[i, 2] %>% pull()
          )
          
          # Define the header with the subscription key
          headers <- c(
            "Cache-Control" = "no-cache",
            "Ocp-Apim-Subscription-Key" = "b01eba8b8603442e870d20cfe4fc6df0"
          )
          
          # send API request
          response <- GET(url, query = params, add_headers(.headers = headers))
          
          if (http_status(response)$category == "Success") {
            # parse JSON 
            parsed_data <- tryCatch({
              fromJSON(content(response, "text"))
            }, error = function(e) {
              message("Error parsing JSON: ", e)
              return(NULL)
            })
            
            # issue with rate limiting
            Sys.sleep(6)
            
            # check if it worked
            if (!is.null(parsed_data)) {
              # extract relevant bits we are interested in
              print(params)
              parameter_readings <- parsed_data$parameterReadings
              
              # Convert the data to a tibble
              rainfall_data <- tibble(location = parsed_data$location,
                                      nameEN = parsed_data$nameEN,
                                      units = parsed_data$units) %>% 
                bind_cols(as_tibble(parameter_readings))
              
              rainfall_data <- rainfall_data %>%
                mutate(time = as.POSIXct(time, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")) %>%
                mutate(date = as.Date(time)) %>%  
                group_by(location, nameEN, units, date) %>%                
                summarize(rain_mm = sum(value, na.rm = TRUE))
              
            } else {
              message("Failed to parse JSON response.")
            }
          } else {
            message("Error: API request failed with status ", http_status(response)$reason)
          }
          rainfall <- bind_rows(rainfall,rainfall_data) 
          
          
        }
        
        # save data to the reactiveValues
        data_values$rainfall <- rainfall
        print(head(data_values$rainfall))
        
        # plot
        output$rainplot <- renderPlot({
          monthly_rainfall <- rainfall %>% filter(complete.cases(.))  %>%
            mutate(year = year(date), month = month(date)) %>%
            group_by(year, month,location) %>%
            summarize(total_rain_mm = sum(rain_mm, na.rm = TRUE), .groups = 'drop') %>%
            ungroup() %>%
            group_by(year, month) %>%
            summarize(average_rain_mm = mean(total_rain_mm, na.rm = TRUE), .groups = 'drop') %>%
            mutate(date = as.Date(paste(year, month, "01", sep = "-"))) %>% 
            filter(complete.cases(.))
          
          # update the reactiveValues
          data_values$monthly_rainfall <- monthly_rainfall
          print(head(data_values$monthly_rainfall))
          
          ggplot(monthly_rainfall %>% mutate(Title = str_wrap(paste0('Average monthly rainfall (mm) observed for ', input$company, ifelse(!is.na(input$areaServed),paste0(' in the ', input$areaServed , ' region'),"")),width=50)), aes(x = date, y = 1, fill = average_rain_mm)) +
            facet_wrap(~Title,ncol=1) +
            geom_tile() +
            scale_x_date(expand = c(0, 0), date_breaks = "1 years", date_labels = "%Y") +
            scale_y_continuous(expand = c(0, 0)) +
            scale_fill_gradientn(colours = brewer.pal(9, "Blues"),
                                 guide = guide_colorbar(
                                   barheight = unit(3, units = "mm"),
                                   barwidth = unit(50, units = "mm"),
                                   title.vjust = 1)) +
            labs(x = "Year", y = "",fill = "Monthly Rainfall (mm)") +
            theme(text = element_text(family = "Krub"), 
                  axis.text.x = element_text(angle = 0, vjust = 0, hjust=0.5,colour="black"),
                  strip.background = element_rect(fill="black"),
                  strip.text = element_text(colour = 'white',face="bold",size = 12),
                  panel.background = element_blank(),
                  panel.grid.major.x = element_line(size = 0.5, linetype = 'solid',
                                                    colour = "lightgrey"),
                  panel.border=element_rect(colour="black",size=1,fill=NA),
                  axis.text.y=element_blank(),axis.ticks.y = element_blank(),
                  legend.position = "bottom")
        }, res = 128)
        
      }
    
    # hide modal
    removeModal()
    
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      if (input$dataFormat == "raw") {
        paste0("rainfall_raw",input$company,"_",Sys.Date(),".xlsx")
      } else {
        paste0("monthly_rainfall_aggregated",input$company,"_",Sys.Date(),".xlsx")
      }
    },
    content = function(file) {
      if (input$dataFormat == "raw") {
        write_xlsx(data_values$rainfall, file)
      } else {
        write_xlsx(data_values$monthly_rainfall, file)
      }
    }
  )
}

# run app 
shinyApp(ui = ui, server = server)
