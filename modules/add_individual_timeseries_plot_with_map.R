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
            br(),
            uiOutput(ns("btn_deselect_sensor")),
            br(),
            timeseries_output(ns("individual_timeseries_plot")))
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


    # Change view to centre stations ----
    set_view_stations <- function(){
      # Check if there is data
      data_snsrs_col <- get_locations(data_stations$data)$station_loc

      # If there are stations then zoom to them
      if(!is.null(data_snsrs_col) ){
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


    # Add the sensors to the map ----
    add_sensors_map <- function(){
      # # Check if there is data
      data_snsrs <-get_locations(data_stations$data)$station_loc
      if(is.null(data_snsrs)){
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


    # Reactive to change stroke sensor ----
    set_stroke_sensor <- reactive({
      # Check if there is any data selected etc.
      shiny::validate(need(nrow(data_stations$data) > 0,
                           "No station data available"))

      # Get the name of the station
      # NB from the selected stations
      selected_station <- data_stations$data %>%
        dplyr::filter(selected & station_type == "sensor")
      indu_station_name <- selected_station$station[data_other$indu_station_index]

      # Change stroke colour for selected station
      data_snsrs <- get_locations(data_stations$data)$station_loc
      data_snsrs <- data_snsrs %>%
        dplyr::mutate(stroke = ifelse(station == indu_station_name, "black", col))

      # Set the updated data from the stations in the reactiveValues
      data_stations$data <- data_snsrs

    })

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

      # If there is no station selected, get the no sensor message of the plotting
      if(is.na(indu_station_name)){
        data_plot <- data_measurements() %>% dplyr::filter(station_type == "sensor")
        # Create timeseries plot
        timeseries_server("individual_timeseries_plot",
                          data_measurements = reactive(data_plot),
                          parameter = parameter,
                          overview_component = overview_component,
                          theme_plots = theme_plots)
      }

      # Check if a sensor is selected
      shiny::validate(need(!is.na(indu_station_name), "No station selected."))

      # Get the measurements of the station
      measurements <- data_measurements() %>%
        dplyr::filter(station == indu_station_name)

      # Set value for the y-ax
      max_yvalue <- data_other$cutoff

      # Set min and max for x-as
      zoom_in = list(
        start_slider_zoom = reactive(data_other$start_date %>% as.POSIXct()),
        end_slider_zoom = reactive(data_other$end_date %>%  as.POSIXct()))

      # Create timeseries plot
      timeseries_server("individual_timeseries_plot",
                        data_measurements = reactive(measurements),
                        parameter = parameter,
                        overview_component = overview_component,
                        theme_plots = theme_plots,
                        manual_ylim = c(0, max_yvalue),
                        zoom_in = zoom_in)
    })


    # generate button to manually delete sensor from selection ----
    output$btn_deselect_sensor <- renderUI({
      # Get the name of the station
      # NB from the selected stations
      selected_station <- data_stations$data %>%
        dplyr::filter(selected & station_type == "sensor")
      indu_station_name <- selected_station$station[data_other$indu_station_index]

      # Check if a sensor is selected
      shiny::validate(need(!is.na(indu_station_name), "No station selected. Please go back to step 1."))

      actionButton(
        ns("btn_deselect_sensor"),
        label = paste0("Verwijder van selectie: ",indu_station_name),
        icon = icon("trash")
        )

    })

    # Function to get the rownumber from the datastations of the selected sensor ----
    get_rownumber_station <- function(data_stations, selected_snsr){
      # Get the rownumber of the sensor in stations$data
      # NB from the selected stations
      selected_station <- data_stations$data %>%
        dplyr::filter(selected & station_type == "sensor") %>%
        dplyr::select(station)
      rownumber_station <- which(selected_station == selected_snsr)
      return(rownumber_station)
    }

    # Observers and ObserveEvents ----
    # Observe the clicks of an user -> store index/rownumber
    observeEvent({input$map_marker_click$id}, {

      # Get the id of the selected marker
      selected_snsr <- input$map_marker_click$id
      log_trace("indu time map module: click id {selected_snsr}")

      rownumber_station <- get_rownumber_station(data_stations, selected_snsr)

      # Store rownumber in data_other
      data_other$indu_station_index <- rownumber_station

    })


    # Observe if user pushes deselect sensor button
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


    # Create list where to observe changes to react on
    tolisten <- reactive({
      list(
           data_other$indu_station_index,
           data_other$cutoff,
           input$btn_deselect_sensor)

    })

    # Observe if new data is available-> redraw map and figure
    observeEvent(tolisten(),{
          # Get the stroke colour selected station
          set_stroke_sensor()
          # Add the new situation to the map
          isolate(add_sensors_map())
          # Redraw the figure with the new selected station
          draw_figure()
    })

    # Observe if we enter the page, then also set zoom level
    observeEvent(change_tab(),{
      tab_info <- change_tab()
      if(!purrr::is_null(tab_info)){
        # If you arrive on this tabpanel then redraw the map.
        if(tab_info == "stap2"){
          # Get the stroke colour selected station
          set_stroke_sensor()
          # Add the new situation to the map
          isolate(add_sensors_map())
          # Zoom to the stations
          set_view_stations()
          # Redraw the figure with the new selected station
          draw_figure()
        }
      }
    })


  })
}
