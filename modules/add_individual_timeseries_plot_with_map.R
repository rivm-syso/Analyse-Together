###############################################
### Individual sensor Time Series Plot Module ###
###############################################

# Choose 1 sensor of the selection on the map and Creates a time series plot

###################################################################
### Output Module ####
#################################################################

individual_timeseries_map_output <- function(id) {

  ns <- NS(id)

  tagList(
    column(width = 4,
    leafletOutput(ns("map"))),
    column(width = 8,
           wellPanel(
                uiOutput(ns("btn_deselect_sensor")),
                br(),
                timeseries_output(ns("individual_timeseries_plot"))
                )
           )
      )


}


######################################################################
# Server Module
######################################################################

individual_timeseries_map_server <- function(id,
                                             data_stations,
                                             data_measurements,
                                             data_measurements_all,
                                             parameter,
                                             overview_component,
                                             theme_plots,
                                             change_tab,
                                             data_other,
                                             group_name_none,
                                             col_default,
                                             line_default
                                             ){

  moduleServer(id, function(input, output, session) {

    ns <- session$ns
    # Initialisation icons ----
    # Icons for the reference stations
    icons_stations <- iconList(
      lml_selected = makeIcon(iconUrl = "images/lml_selected_txt.png", iconWidth = 24, iconHeight = 16),
      lml_deselected = makeIcon(iconUrl = "images/lml_deselected_txt.png",  iconWidth = 24, iconHeight = 16))

    # Icons for the knmi stations
    icons_knmis <- iconList(
      knmi_selected = makeIcon(iconUrl = "images/knmi_selected_txt.png", iconWidth = 30, iconHeight = 16),
      knmi_deselected = makeIcon(iconUrl = "images/knmi_deselected_txt.png", iconWidth = 30, iconHeight = 16))

    # Get the locations from the stations and convert to spatialcoordinates ----
    get_locations <- reactive({
      # Check if there is data
      shiny::validate(need(!is.null(data_stations$data), "Error, no data yet."))

      # Get the location of the stations
      station_loc <- data_stations$data %>%
        dplyr::distinct(station, .keep_all = T) %>%
        dplyr::filter(lon > 0 & lat > 0)

      # Convert to spatialploints
      station_loc_coord <- SpatialPointsDataFrame(station_loc[,c('lon','lat')],station_loc)

      return(list(station_loc = station_loc, station_loc_coord = station_loc_coord))

    })

    # Generate base map ----
    output$map <- renderLeaflet({

      ns("map")

      leaflet() %>%
        # addTiles() %>%
        addProviderTiles(
          'Esri.WorldGrayCanvas' # option 1
          #'Esri.WorldTopoMap'   # option 2

        ) %>%
        addProviderTiles(
          'CartoDB.PositronOnlyLabels' # option 1
        ) %>%
        addDrawToolbar(
          targetGroup = 'Selected',
          polylineOptions = FALSE,
          markerOptions = FALSE,
          polygonOptions = FALSE,
          circleOptions = FALSE,
          circleMarkerOptions = FALSE,
          rectangleOptions = FALSE
        ) %>%

        addEasyButton(easyButton(
          icon="fa-globe", title="Back to default view",
          onClick=JS("function(btn, map){ map.setView([52.153708, 5.384214], 7)}"))) %>%
        addScaleBar(position = "bottomleft")
    })


    # Change view to centre stations
    set_view_stations <- function(){
      # Check if there is data
      data_snsrs_col <- try(isolate(get_locations()$station_loc))

      # If there are stations then zoom to them
      if(class(data_snsrs_col) != "try-error" ){
        # calculate mean location
        mean_lat <- mean(data_snsrs_col$lat)
        mean_lon <- mean(data_snsrs_col$lon)

        # create proxy of the map
        proxy <- leafletProxy('map') # set up proxy map
        proxy %>% setView(mean_lon, mean_lat, zoom = 10)
      }else{
        # Zoom to the default

        # create proxy of the map
        proxy <- leafletProxy('map') # set up proxy map
        proxy %>% setView(5.384214, 52.153708 , zoom = 7)
      }
    }

    # Add knmi stations to the map
    add_knmi_map <- function(){
      # Get the data
      data_snsrs <- try(isolate(get_locations()$station_loc))

      if(class(data_snsrs) == "try-error"){
        # clear all weather stations from the map
        proxy <- leafletProxy('map') # set up proxy map
        proxy %>%
          # Clear weather markers
          clearGroup("weather")
      }else{
        # Get the station data
        data_snsrs <- data_snsrs %>%
          dplyr::filter(station_type == "KNMI" &
                          selected)
        # Update map with new markers to show selected
        proxy <- leafletProxy('map') # set up proxy map
        proxy %>% clearGroup("weather") # Clear  markers

        # Put stations on map
        if(nrow(data_snsrs) > 0){
          proxy %>%
            addMarkers(data = data_snsrs, ~lon, ~lat,
                       icon = icons_knmis["knmi_deselected"],
                       label = lapply(as.list(data_snsrs$station), HTML),
                       layerId = ~station,
                       group = "weather")
        }
      }

    }

    # Add the sensors to the map
    add_sensors_map <- function(){
      # # Check if there is data
      data_snsrs <- try(isolate(get_locations()$station_loc))
      if(class(data_snsrs) == "try-error"){
        #Clear all sensors from the map
        proxy <- leafletProxy('map') # set up proxy map
        proxy %>%
          # Clear sensoren markers
          clearGroup("sensoren")
      }else{
        # Get the sensor data
        data_snsrs <- data_snsrs %>%
          dplyr::filter(station_type == "sensor" &
                          selected)

        # Update map with new markers to show selected
        proxy <- leafletProxy('map') # set up proxy map
        proxy %>%
          # Clear sensor markers
          clearGroup("sensoren")

        # Add the sensors to the map
        if(nrow(data_snsrs) > 0){
          proxy %>%
            addCircleMarkers(data = data_snsrs, ~lon, ~lat,
                             stroke = TRUE,
                             weight = 3,
                             label = lapply(data_snsrs$station, HTML),
                             layerId = ~station,
                             fillOpacity = 0.7,
                             radius = 5,
                             color = data_snsrs$stroke, #== stroke color
                             fillColor =  data_snsrs$col,
                             group = "sensoren"
            )
        }
      }

    }

    # Add reference stations to the map
    add_lmls_map <- function(){
      # # Check if there is data
      data_snsrs <- try(isolate(get_locations()$station_loc), silent = T)
      if(class(data_snsrs) == "try-error"){
        # Clear all reference stations from the map
        proxy <- leafletProxy('map') # set up proxy map
        proxy %>%
          # Clear reference markers
          clearGroup("reference")
      }else{

        # Get the reference stations
        data_snsrs <- data_snsrs %>%
          dplyr::filter(station_type == "ref" &
                          selected)

        # Update map with new markers to show selected
        proxy <- leafletProxy('map') # set up proxy map
        proxy %>%
          # Clear reference markers
          clearGroup("reference")

        # Add the stations to the map
        if(nrow(data_snsrs) > 0){
          proxy %>%
            addMarkers(data = data_snsrs, ~lon, ~lat,
                       icon = icons_stations["lml_deselected"],
                       label = lapply(as.list(data_snsrs$station), HTML),
                       layerId = ~station,
                       group = "reference")
        }
      }
    }

    # reactive to draw figure ----
    draw_figure <- reactive({
      # Check if there is any data selected etc.
      shiny::validate(need(!is.null(data_measurements()),
                           "Please choose data and select station."))
      # Get the name of the station
      # NB from the selected stations
      selected_station <- data_stations$data %>%
        dplyr::filter(selected & station_type == "sensor")
      indu_station_name <- selected_station$station[data_other$indu_station_index]

      # Check if a sensor is selected
      shiny::validate(need(!is.na(indu_station_name), "No station selected."))

      # Get the measurements of the station
      measurements <- data_measurements() %>%
        dplyr::filter(station == indu_station_name)

      # Change stroke colour for selected station
      data_snsrs <- try(isolate(get_locations()$station_loc))
      data_snsrs <- data_snsrs %>%
        dplyr::mutate(stroke = ifelse(station == indu_station_name, "black", col))

      # Set the updated data from the stations in the reactiveValues
      data_stations$data <- data_snsrs

      # Create timeseries plot
      timeseries_server("individual_timeseries_plot",
                        data_measurements = reactive(measurements),
                        parameter = parameter,
                        overview_component = overview_component,
                        theme_plots = theme_plots)

    })

    output$btn_deselect_sensor <- renderUI({
      # Get the name of the station
      # NB from the selected stations
      selected_station <- data_stations$data %>%
        dplyr::filter(selected & station_type == "sensor")
      indu_station_name <- selected_station$station[data_other$indu_station_index]

      # Check if a sensor is selected
      shiny::validate(need(!is.na(indu_station_name), "No station selected."))

      actionButton(
        ns("btn_deselect_sensor"),
        label = paste0("Verwijder van selectie: ",indu_station_name),
        icon = icon("trash")
        )

    })


    get_rownumber_station <- function(data_stations, selected_snsr){
      # Get the rownumber of the sensor in stations$data
      # NB from the selected stations
      selected_station <- data_stations$data %>%
        dplyr::filter(selected & station_type == "sensor") %>%
        dplyr::select(station)
      rownumber_station <- which(selected_station == selected_snsr)
      return(rownumber_station)
    }

    # Observe the clicks of an user -> store index/rownumber
    observeEvent({input$map_marker_click$id}, {

      # Get the id of the selected marker
      selected_snsr <- input$map_marker_click$id
      log_trace("indu time map module: click id {selected_snsr}")

      rownumber_station <- get_rownumber_station(data_stations, selected_snsr)

      # Store rownumber in data_other
      data_other$indu_station_index <- rownumber_station

    })

    # Observe if user pushes deselect sensor button ----
    observeEvent({input$btn_deselect_sensor}, {

      # Get the name and ID of the sensor to be deselected
      # Get the name of the station
      # NB from the selected stations
      selected_station <- data_stations$data %>%
        dplyr::filter(selected & station_type == "sensor")
      indu_station_name <- selected_station$station[data_other$indu_station_index]

      # Change the state of the sensor in data_stations
      data_stations$data <- change_state_to_deselected(data_stations$data,
                                                       indu_station_name,
                                                       group_name_none,
                                                       col_default,
                                                       line_default
                                                       )

      # Choose a new sensor to be selected, Store rownumber in data_other (default)
      data_other$indu_station_index <- 1

    })

    # Observers and ObserveEvents ----

    # Create list where to observe changes to react on
    tolisten <- reactive({
      list(change_tab(),
           data_other$indu_station_index,
           input$btn_deselect_sensor)
    })
    # Observe if new data is available-> redraw map
    observeEvent(tolisten(),{
      # Redraw the figure with the new selected station
      draw_figure()
      # Add the new situation to the map
      isolate(add_lmls_map())
      isolate(add_sensors_map())
      isolate(add_knmi_map())
      # Zoom to the stations
      set_view_stations()

    })


  })
}
