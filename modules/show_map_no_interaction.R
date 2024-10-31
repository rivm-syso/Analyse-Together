###############################################
#### Module for the map without interaction ####
###############################################

# This is a map module, for only visualisation, no interaction with the user
######################################################################
# Output Module ----
######################################################################

show_map_no_output <- function(id) {

  ns <- NS(id)

  tagList(
    leafletOutput(ns('map_no'))
  )

}


######################################################################
# Server Module ----
######################################################################

show_map_no_server <- function(id,
                            data_stations
) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # Initialisation icons ----
    # Icons for the reference stations
    icons_stations <- iconList(
      lml_selected = makeIcon(iconUrl = "images/ref_selected_txt.png",
                              iconWidth = 24, iconHeight = 16),
      lml_deselected = makeIcon(iconUrl = "images/ref_deselected_txt.png",
                                iconWidth = 24, iconHeight = 16))

    # Icons for the knmi stations
    icons_knmis <- iconList(
      knmi_selected = makeIcon(iconUrl = "images/knmi_selected_txt.png",
                               iconWidth = 30, iconHeight = 16),
      knmi_deselected = makeIcon(iconUrl = "images/knmi_deselected_txt.png",
                                 iconWidth = 30, iconHeight = 16))

    # Generate base map ----
    output$map_no <- renderLeaflet({
      log_trace("mod map no interaction: output$map renderleaflet")
      ns("map_no")
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

          addScaleBar(position = "bottomleft")
    })


    # Change view to centre stations
    set_view_stations <- function(){
      log_trace("mod map no interaction: set view stations")

      # Check if there is data
      data_snsrs_col <- get_locations_coordinates(data_stations())$station_loc

      # If there are stations then zoom to them
      if(!is.null(data_snsrs_col)){
        # calculate mean location
        mean_lat <- mean(data_snsrs_col$lat)
        mean_lon <- mean(data_snsrs_col$lon)

        # create proxy of the map
        proxy <- leafletProxy('map_no') # set up proxy map
        proxy %>% setView(mean_lon, mean_lat, zoom = 10)
      }else{
        # Zoom to the default

        # create proxy of the map
        proxy <- leafletProxy('map_no') # set up proxy map
        proxy %>% setView(5.384214, 52.153708 , zoom = 7)
    }
    }

    # Add knmi stations to the map
    add_knmi_map <- function(){
      # Get the data
      data_snsrs <- get_locations_coordinates(data_stations())$station_loc

      if(is.null(data_snsrs) ){
        # clear all weather stations from the map
        proxy <- leafletProxy('map_no') # set up proxy map
        proxy %>%
          # Clear weather markers
          clearGroup("weather")
      }else{
        # Get the station data
        data_snsrs <- data_snsrs %>%
        dplyr::filter(station_type == "KNMI")
        # Update map with new markers to show selected
        proxy <- leafletProxy('map_no') # set up proxy map
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
      log_trace("mod map no interaction: add sensor map")

      # # Check if there is data
      data_snsrs <- get_locations_coordinates(data_stations())$station_loc
      if(is_null(data_snsrs)){
        #Clear all sensors from the map
        proxy <- leafletProxy('map_no') # set up proxy map
        proxy %>%
          # Clear sensoren markers
          clearGroup("sensoren")
      }else{
        # Get the sensor data
        data_snsrs <- data_snsrs %>%
          dplyr::filter(station_type == "sensor")

        # Update map with new markers to show selected
        proxy <- leafletProxy('map_no') # set up proxy map
        proxy %>%
          # Clear sensor markers
          clearGroup("sensoren")

        # Add the sensors to the map
        if(nrow(data_snsrs) > 0){
          proxy %>%
          addCircleMarkers(data = data_snsrs, ~lon, ~lat,
                           stroke = TRUE,
                           weight = 2,
                           label = lapply(data_snsrs$station, HTML),
                           layerId = ~station,
                           fillOpacity = 0.7,
                           radius = 5,
                           color = "black",
                           group = "sensoren"
          )
        }
      }

    }

    # Add reference stations to the map
    add_lmls_map <- function(){

      # # Check if there is data
      data_snsrs <- get_locations_coordinates(data_stations())$station_loc
      if(is.null(data_snsrs)){
        # Clear all reference stations from the map
        proxy <- leafletProxy('map_no') # set up proxy map
        proxy %>%
          # Clear reference markers
          clearGroup("reference")
      }else{

        # Get the reference stations
        data_snsrs <- data_snsrs %>%
          dplyr::filter(station_type == "ref")

        # Update map with new markers to show selected
        proxy <- leafletProxy('map_no') # set up proxy map
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

    # Observers and ObserveEvents ----

    # Create list where to observe changes to react on
    tolisten <- reactive({
      list(data_stations())
    })
    # Observe if new data is available-> redraw map
    observeEvent(tolisten(),{

          # Add the new situation to the map
          isolate(add_lmls_map())
          isolate(add_sensors_map())
          isolate(add_knmi_map())
          # Zoom to the stations
          set_view_stations()

    })

  })

}
