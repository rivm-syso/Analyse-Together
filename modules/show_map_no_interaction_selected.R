###############################################
#### Module for the map without interaction only selected ####
###############################################

# This is a map module, for only visualisation, no interaction with the user
# Only shows the selected stations
######################################################################
# Output Module ----
######################################################################

show_map_no_select_output <- function(id) {

  ns <- NS(id)

  tagList(
    leafletOutput(ns('map_select'))
  )

}


######################################################################
# Server Module ----
######################################################################

show_map_no_select_server <- function(id,
                               data_stations,
                               change_tab
) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # Initialisation icons ----
    # Icons for the reference stations
    icons_stations <- iconList(
      lml_selected = makeIcon(iconUrl = "images/lml_selected_txt.png",
                              iconWidth = 24, iconHeight = 16))

    # Icons for the knmi stations
    icons_knmis <- iconList(
      knmi_selected = makeIcon(iconUrl = "images/knmi_selected_txt.png",
                               iconWidth = 30, iconHeight = 16))

    # Get the locations from the stations and convert to spatialcoordinates ----
    get_locations <- reactive({
      # Check if there is data
      shiny::validate(need(!is.null(data_stations()), "Error, no data yet."))

      # Get the location of the stations
      station_loc <- data_stations() %>%
        dplyr::distinct(station, .keep_all = T) %>%
        dplyr::filter(lon > 0 & lat > 0)

      # Convert to spatialploints
      station_loc_coord <- SpatialPointsDataFrame(station_loc[,c('lon','lat')],
                                                  station_loc)

      return(list(station_loc = station_loc,
                  station_loc_coord = station_loc_coord))

    })

    # Generate base map ----
    output$map_select <- renderLeaflet({

      ns("map_select")
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
      # Check if there is data
      data_snsrs_col <- try(isolate(get_locations()$station_loc))

      # If there are stations then zoom to them
      if(class(data_snsrs_col) != "try-error" ){
        # calculate mean location
        mean_lat <- mean(data_snsrs_col$lat)
        mean_lon <- mean(data_snsrs_col$lon)

        # create proxy of the map
        proxy <- leafletProxy('map_select') # set up proxy map
        proxy %>% setView(mean_lon, mean_lat, zoom = 10)
      }else{
        # Zoom to the default

        # create proxy of the map
        proxy <- leafletProxy('map_select') # set up proxy map
        proxy %>% setView(5.384214, 52.153708 , zoom = 10)
      }
    }

    # Add knmi stations to the map
    add_knmi_map <- function(){
      # Get the data
      data_snsrs <- try(isolate(get_locations()$station_loc))

      if(class(data_snsrs) == "try-error"){
        # clear all weather stations from the map
        proxy <- leafletProxy('map_select') # set up proxy map
        proxy %>%
          # Clear weather markers
          clearGroup("weather")
      }else{
        # Get the station data
        data_snsrs <- data_snsrs %>%
          # Only show the selected
          dplyr::filter(station_type == "KNMI", selected)
        # Update map with new markers to show selected
        proxy <- leafletProxy('map_select') # set up proxy map
        proxy %>% clearGroup("weather") # Clear  markers

        # Put stations on map
        if(nrow(data_snsrs) > 0){
          proxy %>%
            addMarkers(data = data_snsrs, ~lon, ~lat,
                       icon = icons_knmis["knmi_selected"],
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
        proxy <- leafletProxy('map_select') # set up proxy map
        proxy %>%
          # Clear sensoren markers
          clearGroup("sensoren")
      }else{
        # Get the sensor data
        data_snsrs <- data_snsrs %>%
          # Only show the selected
          dplyr::filter(station_type == "sensor", selected)

        # Update map with new markers to show selected
        proxy <- leafletProxy('map_select') # set up proxy map
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
                             color = data_snsrs$col,
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
        proxy <- leafletProxy('map_select') # set up proxy map
        proxy %>%
          # Clear reference markers
          clearGroup("reference")
      }else{

        # Get the reference stations
        data_snsrs <- data_snsrs %>%
          # Only show the selected
          dplyr::filter(station_type == "ref", selected)

        # Update map with new markers to show selected
        proxy <- leafletProxy('map_select') # set up proxy map
        proxy %>%
          # Clear reference markers
          clearGroup("reference")

        # Add the stations to the map
        if(nrow(data_snsrs) > 0){
          proxy %>%
            addMarkers(data = data_snsrs, ~lon, ~lat,
                       icon = icons_stations["lml_selected"],
                       label = lapply(as.list(data_snsrs$station), HTML),
                       layerId = ~station,
                       group = "reference")
        }
      }
    }

    # Observers and ObserveEvents ----

    # Create list where to observe changes to react on
    tolisten <- reactive({
      list(change_tab())
    })
    # Observe if new data is available-> redraw map
    observeEvent(tolisten(),{
        # Add the new situation to the map
        isolate(add_lmls_map())
        isolate(add_sensors_map())
        isolate(add_knmi_map())
        # Zoom to the stations
        set_view_stations()
    #     }
    # }

    })

  })

}
