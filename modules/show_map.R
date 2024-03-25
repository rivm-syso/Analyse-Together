###############################################
#### Module for the map ####
###############################################

# This is a map module, including selection and deselection
######################################################################
# Output Module ----
######################################################################

show_map_output <- function(id) {

  ns <- NS(id)

  tagList(
    leafletOutput(ns('map'))
    )

}


######################################################################
# Server Module ----
######################################################################

show_map_server <- function(id,
                            data_stations,
                            group_name,# class: "reactiveExpr" "reactive" "function"
                            tab_choice,# class: "reactiveExpr" "reactive" "function"
                            # Options for the colors
                            col_default,
                            col_select,
                            # Options for the linetype
                            line_cat,
                            line_default,
                            line_overload,
                            # default group name
                            group_name_none
                            ) {

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
        dplyr::filter(lon > 0 & lat >0)

      # Convert to spatialploints
      station_loc_coord <- SpatialPointsDataFrame(station_loc[,c('lon','lat')],station_loc)

      return(list(station_loc = station_loc, station_loc_coord = station_loc_coord))

      })

    # Generate base map ----
    output$map <- renderLeaflet({

      ns("map")

      leaflet() %>%
        setView(5.384214, 52.153708 , zoom = 7) %>%
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
          rectangleOptions = drawRectangleOptions(shapeOptions=drawShapeOptions(fillOpacity = 0
                                                                                ,color = 'black'
                                                                                ,weight = 1.5)),
          editOptions = editToolbarOptions(edit = FALSE,
                                           selectedPathOptions = selectedPathOptions())) %>%

        addScaleBar(position = "bottomleft")
    })

    # Functions ----
    # Check the state of the station (selected or not)
    check_state <- function(id_selected){
      selected <- data_stations$data %>%
        dplyr::filter(station == id_selected) %>%
        dplyr::select(selected) %>%
        pull()
      return(selected)
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

       # get the stations only knmi
        data_snsrs <- data_snsrs %>%
          dplyr::filter(station_type == "KNMI")

        # Update map with new markers to show selected
        proxy <- leafletProxy('map') # set up proxy map
        proxy %>% clearGroup("weather") # Clear  markers

        # Check if there are KNMI stations
        if(nrow(data_snsrs) == 0){
          return(NULL)
        }

        # Put selected stations on map
        data_selected <- data_snsrs %>%
          dplyr::filter(selected)

        if(nrow(data_selected) > 0){
          proxy %>%
            addMarkers(data = data_selected, ~lon, ~lat,
                       icon = icons_knmis["knmi_selected"],
                       label = lapply(as.list(data_selected$station), HTML),
                       layerId = ~station,
                       group = "weather")
        }else{
          # NB there will always be a KNMI station selected!
          # Select random station
          random_station <- data_snsrs$station[1]
          data_stations$data <- change_state_to_selected(data_stations$data,
                                                         random_station,
                                                         group_name(),
                                                         col_select(),
                                                         line_cat,
                                                         line_default,
                                                         line_overload)

          # Get all newer data info
          # Get the data
          data_snsrs <- try(isolate(get_locations()$station_loc))
          # get the stations only knmi
          data_snsrs <- data_snsrs %>%
            dplyr::filter(station_type == "KNMI")
          # Put selected stations on map
          data_selected <- data_snsrs %>%
            dplyr::filter(selected)

          # add marker to map
          proxy %>%
            addMarkers(data = data_selected, ~lon, ~lat,
                       icon = icons_knmis["knmi_selected"],
                       label = lapply(as.list(data_selected$station), HTML),
                       layerId = ~station,
                       group = "weather")
        }

        # Put deselected stations on map
        data_deselected <- data_snsrs %>% dplyr::filter(!selected)
        if(nrow(data_deselected > 0)){
          proxy %>%
            addMarkers(data = data_deselected, ~lon, ~lat,
                       icon = icons_knmis["knmi_deselected"],
                       label = lapply(data_deselected$station, HTML),
                       layerId = ~station,
                       group = "weather")
        }
      }
  }

    # Add the sensors to the map
    add_sensors_map <- function(){
      # Check if there is data
      data_snsrs_col <- try(isolate(get_locations()$station_loc))
      if(class(data_snsrs_col) == "try-error"){
        # clear all sensor stations from the map
        proxy <- leafletProxy('map') # set up proxy map
        proxy %>%
          # Clear sensor markers
          clearGroup("sensoren")
      }else{
      # Get the sensor data
      data_snsrs_col <- isolate(get_locations()$station_loc) %>%
        dplyr::filter(station_type == "sensor")

      if(nrow(data_snsrs_col)>0){
        # Update map with new markers to show selected
        proxy <- leafletProxy('map') # set up proxy map
        proxy %>% clearGroup("sensoren") # Clear sensor markers

        leafletProxy("map") %>%
          addCircleMarkers(data = data_snsrs_col, ~lon, ~lat,
                           stroke = TRUE,
                           weight = 2,
                           label = lapply(data_snsrs_col$station, HTML),
                           layerId = ~station,
                           fillOpacity = 0.7,
                           radius = 5,
                           color = data_snsrs_col$col,
                           group = "sensoren"
          )
      }
      }
    }

    # Add reference stations to the map
    add_lmls_map <- function(){
      # Check if there is data
      data_snsrs <- try(isolate(get_locations()$station_loc), silent = T)
      if(class(data_snsrs) == "try-error"){
        # Clear all reference stations from the map
        proxy <- leafletProxy('map') # set up proxy map
        proxy %>%
          # Clear reference markers
          clearGroup("reference")
      }else{

      # Get the reference stations
      data_snsrs <- isolate(get_locations()$station_loc) %>%
        dplyr::filter(station_type == "ref")

      # Update map with new markers to show selected
      proxy <- leafletProxy('map') # set up proxy map
      proxy %>% clearGroup("reference") # Clear reference markers

      # Check if there are Ref stations
      if(nrow(data_snsrs) == 0){
        return(NULL)
      }

      # Put selected stations on map
      data_selected <- data_snsrs %>% dplyr::filter(selected)

      if(nrow(data_selected > 0)){
        proxy %>%
          addMarkers(data = data_selected, ~lon, ~lat,
                     icon = icons_stations["lml_selected"],
                     label = lapply(as.list(data_selected$station), HTML),
                     layerId = ~station,
                     group = "reference")
      }else{
        # NB there will always be a reference station selected!
        # Select random station
        random_station <- data_snsrs$station[2]
        data_stations$data <- change_state_to_selected(data_stations$data,
                                                       random_station,
                                                       group_name(),
                                                       col_select(),
                                                       line_cat,
                                                       line_default,
                                                       line_overload)

        # Get all newer data info
        # Get the reference stations
        data_snsrs <- isolate(get_locations()$station_loc) %>%
          dplyr::filter(station_type == "ref")
        # Put selected stations on map
        data_selected <- data_snsrs %>%
          dplyr::filter(selected)

        # add marker to map
        proxy %>%
          addMarkers(data = data_selected, ~lon, ~lat,
                     icon = icons_stations["lml_selected"],
                     label = lapply(as.list(data_selected$station), HTML),
                     layerId = ~station,
                     group = "reference")
      }

      # Put deselected stations on map
      data_deselected <- data_snsrs %>% dplyr::filter(!selected)
      if(nrow(data_deselected > 0)){
        proxy %>%
          addMarkers(data = data_deselected, ~lon, ~lat,
                     icon = icons_stations["lml_deselected"],
                     label = lapply(data_deselected$station, HTML),
                     layerId = ~station,
                     group = "reference")
      }
      }
    }

    # Observers and ObserveEvents ----

    # Observe if tabsetpanel is changed to the visualisation tab -> redraw map
    observe({
      tab_info <- tab_choice()
      if(!purrr::is_null(tab_info)){
        # If you arrive on this tabpanel then redraw the map.
        if(tab_info == "stap1"){
          # Add the new situation to the map
          isolate(add_lmls_map())
          isolate(add_sensors_map())
          isolate(add_knmi_map())
          # Zoom to the default
          # create proxy of the map
          proxy <- leafletProxy('map') # set up proxy map
          proxy %>% setView(5.384214, 52.153708 , zoom = 7)
        }
      }
    })

    # Observe if a sensor is in de square selection -> deselect
    observeEvent({input$map_draw_deleted_features},{

      # Get the polygon: be carefull different then the selection
      rectangular_desel <- input$map_draw_deleted_features$features

      # Zoek de sensoren in de feature
      if (!is.null(rectangular_desel)){
        # Check if there is data
        data_snsrs <- get_locations()$station_loc_coord

        shiny::validate(
          need(!is.null(data_snsrs), "Error, no data selected.")
        )

        # There can be multiple features be deleted at the same time
        for(feature in rectangular_desel){
          # Find the stations inside the selected rectangle
          found_in_bounds <- findLocations_sel(shape = feature,
                                               location_coordinates = data_snsrs,
                                               location_id_colname = "station")

          # Set selected == F for all stations within rectangle
          for(id_select in found_in_bounds){
            selected <- check_state(id_select)
            if (selected == F){
              done
            }
            else {
              data_stations$data <- change_state_to_deselected(data_stations$data,
                                                               id_select,
                                                               group_name_none,
                                                               col_default,
                                                               line_default)
            }
          }
        }
      }
      else{done}

      # Add the new situation to the map
      isolate(add_lmls_map())
      isolate(add_sensors_map())
      isolate(add_knmi_map())
    })

    # Observe if a sensor is in de square selection -> select
    observeEvent({input$map_draw_new_feature},{
      # Get the rectangle feature
      rectangular_sel <- input$map_draw_new_feature

      # Zoek de sensoren in de feature
      if (!is.null(rectangular_sel)){
        # Check if there is data
        data_snsrs <- get_locations()$station_loc_coord

        shiny::validate(
          need(!is.null(data_snsrs), "Error, no data selected.")
        )

        # Find the stations insite the selected rectangle
        found_in_bounds <- findLocations_sel(shape = rectangular_sel,
                                             location_coordinates = data_snsrs,
                                             location_id_colname = "station")

        # Set selected == T for all stations within rectangle
        for(id_select in found_in_bounds){
          selected <- check_state(id_select)
          if (selected == T){
            done
          }
          else {
            data_stations$data <- change_state_to_selected(data_stations$data,
                                                           id_select,
                                                           group_name(),
                                                           col_select(),
                                                           line_cat,
                                                           line_default,
                                                           line_overload)
          }
        }}
      else{done}

      # Add the new situation to the map
      isolate(add_lmls_map())
      isolate(add_sensors_map())
      isolate(add_knmi_map())

    })

    # Observe the clicks of an user
    observeEvent({input$map_marker_click$id}, {

      # Get the id of the selected marker
      selected_snsr <- input$map_marker_click$id
      log_trace("map module: click id {selected_snsr}")

      # Check if there is clicked
      if (!is.null(click)){
        selected <- check_state(selected_snsr)
        # If stations is already selected -> deselect
        if (selected == T){
          data_stations$data <- change_state_to_deselected(data_stations$data,
                                                           selected_snsr,
                                                           group_name_none,
                                                           col_default,
                                                           line_default)
        }
        else { # If not yet selected -> select
          data_stations$data <- change_state_to_selected(data_stations$data,
                                                         selected_snsr,
                                                         group_name(),
                                                         col_select(),
                                                         line_cat,
                                                         line_default,
                                                         line_overload)
        }}
      else{done}

      # add the updated markers on the map
      isolate(add_sensors_map())
      isolate(add_lmls_map())
      isolate(add_knmi_map())
    })

    # Return ----
    return(list(map = map,
                data_stations = reactive({data_stations$data}))
           )

  })

}
