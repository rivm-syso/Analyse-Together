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
    leafletOutput(ns('map')),
    uiOutput(ns('sensormap'))
  )

}


######################################################################
# Server Module ----
######################################################################

show_map_server <- function(id,
                            data_stations,
                            # Options for the colors
                            col_cat,
                            col_default,
                            col_overload,
                            # Options for the linetype
                            line_cat,
                            line_default,
                            line_overload
                            ) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # redundant?
    beatCol <- colorFactor(palette = 'RdYlGn', domain = c(0,100), reverse = TRUE)

    # Icons for the reference stations
    icons_stations <- iconList(
      lml_selected = makeIcon(iconUrl = "images/lml_selected.png", iconWidth = 24, iconHeight = 16),
      lml_deselected = makeIcon(iconUrl = "images/lml_deselected.png",  iconWidth = 24, iconHeight = 16))

    # Icons for the knmi stations
    icons_knmis <- iconList(
      knmi_selected = makeIcon(iconUrl = "images/knmi_selected.png", iconWidth = 20, iconHeight = 20),
      knmi_deselected = makeIcon(iconUrl = "images/knmi_deselected.png", iconWidth = 20, iconHeight = 20))

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

    # Create an reactive to change with the observe, to store the clicked id
    state_station <- reactiveValues(value = "")

    # Check the state of the station (selected or not)
    check_state <- function(id_selected){
      browser()
      selected <- data_stations$data %>%
        dplyr::filter(station == id_selected) %>%
        dplyr::select(selected) %>%
        pull()
      # selected <- id_selected %in% isolate(state_station$value)
      return(selected)
    }

    # Change the clicked_id stored
    change_state_to_deselected <- function(id_selected){
      # # Set the state to the Bookkeeper as F
      # state_station$value <- isolate(state_station$value)[isolate(state_station$value) %in%
      #                                                       c(id_selected) == FALSE]

      # Get the data from the stations
      data_stns <- data_stations$data

      # Set the deselected station to select == F
      data_stns <- data_stns %>%
        dplyr::mutate(selected = ifelse(station == id_selected, F, selected),
                      # Change the color to the default
                      col = ifelse(station == id_selected, col_default, col),
                      # Change the linetype to the default
                      linetype = ifelse(station == id_selected, line_default, linetype))

      # Set the updated data from the stations in the reactiveValues
      data_stations$data <- data_stns
    }

    change_state_to_selected <- function(id_selected){
      browser()
      # Set the state to the Bookkeeper as T
      # state_station$value <- c(isolate(state_station$value), id_selected)

      # Get the data from the stations
      data_stns <- data_stations$data

      # Set the selected station to select == T
      data_stns <- data_stns %>%
        dplyr::mutate(selected = ifelse(station == id_selected, T, selected))

      # Assign colors -> sensor
      data_stns <- assign_color_stations(data_stns, col_cat, col_default, col_overload, col_station_type = "sensor")

      # Assign linetype -> reference station
      data_stns <- assign_linetype_stations(data_stns, line_cat, line_default, line_overload, line_station_type = "ref")

      # Set the updated data from the stations in the reactiveValues
      data_stations$data <- data_stns


    }

    # Generate base map ----
    output$map <- renderLeaflet({

      ns("map")
      leaflet() %>%
        setView(5.384214, 52.153708 , zoom = 7) %>%
        addTiles() %>%
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
          editOptions = editToolbarOptions(edit = FALSE, selectedPathOptions = selectedPathOptions())) %>%

        addEasyButton(easyButton(
          icon="fa-globe", title="Back to default view",
          onClick=JS("function(btn, map){ map.setView([52.153708, 5.384214], 7)}"))) %>%
        addEasyButton(easyButton(
          icon="fa-crosshairs", title="Locate Me",
          onClick=JS("function(btn, map){ map.locate({setView: true}); }"))) %>%
        addScaleBar(position = "bottomleft")

    })

    # Add knmi stations to the map
    add_knmi_map <- function(){

      data_knmi <- isolate(get_locations()$station_loc) %>%
        dplyr::filter(., grepl("KNMI",station))

      for (knmis in unique(data_knmi$station)){

        if (isTRUE(data_knmi$selected[data_knmi$station == knmis])){

          leafletProxy("map") %>%

            addMarkers(data = data_knmi[data_knmi$station == knmis,], lng = ~lon, lat = ~lat,
                       icon = icons_knmis["knmi_selected"],
                       label = ~station,
                       layerId = ~station,
                       group = "knmi_stations")
        }
        else {

          leafletProxy("map") %>%

            addMarkers(data = data_knmi[data_knmi$station == knmis,], lng = ~lon, lat = ~lat,
                       icon = icons_knmis["knmi_deselected"],
                       label = ~station,
                       layerId = ~station,
                       group = "knmi_stations")}}
    }

    # Add the sensors to the map
    add_sensors_map <- function(){

      # Check if there is data
      data_snsrs_col <- try(isolate(get_locations()$station_loc))
      shiny::validate(
        need(class(data_snsrs_col) != "try-error", "Error, no data selected.")
      )
      data_snsrs_col <- isolate(get_locations()$station_loc) %>% filter(., !grepl("KNMI|NL",station))

      # Update map with new markers to show selected
      proxy <- leafletProxy('map') # set up proxy map
      leafletProxy("map") %>%
        addCircleMarkers(data = data_snsrs_col, ~lon, ~lat,stroke = TRUE, weight = 2,
                         label = lapply(data_snsrs_col$station, HTML),
                         layerId = ~station,
                         radius = 5,
                         color = data_snsrs_col$col,
                         group = "sensoren"

        )
    }

    # Redundant?
    add_sensors_map_update_button <- function(){

      # Check if there is data
      data_snsrs_col <- try(isolate(get_locations()$station_loc), silent = T)
      shiny::validate(
        need(class(data_snsrs_col) != "try-error", "Error, no data selected.")
      )

      frame_for_map <- get_locations()$station_loc
      data_snsrs_col <- frame_for_map %>% filter(., !grepl("KNMI|NL",station))

      # Update map with new markers to show selected
      # proxy <- leafletProxy('map') # set up proxy map
      leafletProxy("map") %>%
        fitBounds(min(frame_for_map$lon), min(frame_for_map$lat), max(frame_for_map$lon), max(frame_for_map$lat)) %>%
        addCircleMarkers(data = data_snsrs_col, ~lon, ~lat,stroke = TRUE, weight = 2,
                         label = lapply(data_snsrs_col$station, HTML),
                         layerId = ~station,
                         radius = 5,
                         color = data_snsrs_col$col,
                         group = "sensoren"
        )}

    # Add reference stations to the map
    add_lmls_map <- function(){
      # Check if there is data
      data_snsrs <- try(isolate(get_locations()$station_loc), silent = T)
      shiny::validate(
        need(class(data_snsrs) != "try-error", "Not yet selected any data.")
      )

      data_snsrs <- isolate(get_locations()$station_loc) %>% filter(., grepl("LML",station_type))

      for (lmls in unique(data_snsrs$station)){

        if (isTRUE(data_snsrs$selected[data_snsrs$station == lmls])){
          # Update map with new markers to show selected

          #proxy <- leafletProxy('map') # set up proxy map
          leafletProxy("map") %>%

            addMarkers(data = data_snsrs[data_snsrs$station == lmls,], ~lon, ~lat,
                       icon = icons_stations["lml_selected"],
                       label =  ~station,
                       layerId = ~station,
                       #radius = 5,
                       #color = data_snsrs$col,
                       group = "lmls")
        }
        else {

          leafletProxy("map") %>%

            addMarkers(data = data_snsrs[data_snsrs$station == lmls,], ~lon, ~lat, icon = icons_stations["lml_deselected"],
                       label =  ~station,
                       layerId = ~station,
                       #radius = 5,
                       #color = data_snsrs$col,
                       group = "lmls")}}
    }

    remove_knmi_map <- function(){

      # Update map with new markers to show selected
      #proxy <- leafletProxy('map') # set up proxy map
      leafletProxy("map") %>%

        clearGroup(group = "knmi_stations")
    }

    # Observe if a sensor is in de square selection
    observe({

      rectangular_desel <- input$map_draw_deleted_features

      # ga dan de sensoren af en deselecteer deze een voor een
      for (id_select in isolate(get_locations()[[2]]$station)){
        change_state_to_deselected(id_select)
      }
      isolate(add_sensors_map())
      isolate(add_lmls_map())
      isolate(add_knmi_map())
    })

    observe({


      rectangular_sel <- input$map_draw_new_feature

      # Zoek de sensoren in de feature
      if (length(rectangular_sel[[1]] > 0)){
        # Check if there is data
        data_snrs <- try(isolate(get_locations()), silent = T)
        shiny::validate(
          need(class(data_snrs) != "try-error", "Error, no data selected.")
        )
        # Find the stations insite the selected rectangle
        found_in_bounds <- findLocations_sel(shape = rectangular_sel,
                                             location_coordinates = isolate(get_locations()[[2]]),
                                             location_id_colname = "station")

        for(id_select in found_in_bounds){
          selected <- check_state(id_select)
          if (selected == T){
            done
          }
          else {
            change_state_to_selected(id_select)
          }
        }}
      else{done}

      isolate(add_lmls_map())
      isolate(add_sensors_map())
      isolate(add_knmi_map())

    })

    # Observe the clicks of an user
    observeEvent({input$map_marker_click$id}, {

      click <- input$map_marker_click
      browser()
      selected_snsr <- input$map_marker_click$id
      log_trace("map module: click id {selected_snsr}")

      # Check if there is clicked
      if (!is.null(click)){
        selected <- check_state(selected_snsr)
        if (selected == T){
          change_state_to_deselected(selected_snsr)
        }
        else {
          change_state_to_selected(selected_snsr)
        }}
      else{done}

      isolate(add_sensors_map())
      isolate(add_lmls_map())
      isolate(add_knmi_map())
    })

    # Return ----
    return(list(
      map = map,
      state_station = reactive({state_station$value})))

  })

}

