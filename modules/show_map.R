###############################################
### ...Input - select sensor/project/data-range? ###
###############################################

# This is a map module
######################################################################
# Output Module
######################################################################

show_map_output <- function(id) {

  ns <- NS(id)

  tagList(
    leafletOutput(ns('map')),
    uiOutput(ns('sensormap')),
    uiOutput(ns('show_knmi'))
  )

}


######################################################################
# Server Module
######################################################################

show_map_server <- function(id, com_module, update_data) {

  moduleServer(id, function(input, output, session) {


    ns <- session$ns
    beatCol <- colorFactor(palette = 'RdYlGn', domain = c(0,100), reverse = TRUE)
    icons_stations <- iconList(
      lml_selected = makeIcon(iconUrl = "images/lml_selected.png", iconWidth = 24, iconHeight = 16),
      lml_deselected = makeIcon(iconUrl = "images/lml_deselected.png",  iconWidth = 24, iconHeight = 16))

    icons_knmis <- iconList(
      knmi_selected = makeIcon(iconUrl = "images/knmi_selected.png", iconWidth = 20, iconHeight = 20),
      knmi_deselected = makeIcon(iconUrl = "images/knmi_deselected.png", iconWidth = 20, iconHeight = 20))

    # Get the locations from the stations and convert to spatialcoordinates
    get_locations <- reactive({
      sensorloc <- com_module$station_locations() %>% dplyr::distinct(station, .keep_all = T) %>% filter(lon > 0 & lat >0)
      sensorloc_coord <- SpatialPointsDataFrame(sensorloc[,c('lon','lat')],sensorloc)

      return(list(sensorloc, sensorloc_coord))})


    # Create an reactive to change with the observe, to store the clicked id
    state_station <- reactiveValues(value = "")

    check_state <- function(id_selected){
      selected <- id_selected %in% isolate(state_station$value)
      return(selected)
    }

    # Change the clicked_id stored
    change_state_to_deselected <- function(id_selected){
      state_station$value <- isolate(state_station$value)[isolate(state_station$value) %in% c(id_selected) == FALSE]
    }

    change_state_to_selected <- function(id_selected){
      state_station$value <- c(isolate(state_station$value), id_selected)
    }

    # Generate base map ----
    output$map <- renderLeaflet({

      # data_snsrs <- try(get_locations()[[1]], silent = T)
      # print(data_snsrs)
      # shiny::validate(
      #
      #   need(class(data_snsrs) != "try-error", "Not yet selected any data.")
      # )
      #
      #data_snsrs <- try(get_locations()[[1]], silent = T) %>% filter(., !grepl("KNMI|NL",station))

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
        # %>%
        # addCircleMarkers(data = data_snsrs, ~lon, ~lat,stroke = TRUE, weight = 2,
        #                label = lapply(data_snsrs$station, HTML),
        #                layerId = ~station,
        #                radius = 5,
        #                color = data_snsrs$col,
        #                group = "sensoren"
        # )

    })


    output$show_knmi <- renderUI({

      # Create the component picker with a list of possible choices
      tagList(

        checkboxInput(
          ns("show_knmi"),
          label    = i18n$t("sel_knmi"),
          value    = FALSE
        )
      )
    })

    add_knmi_map <- function(){

      data_knmi <- get_locations()[[1]] %>% filter(., grepl("KNMI",station))

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

    add_sensors_map <- function(){

      # Check if there is data
      data_snsrs_col <- try(get_locations()[[1]], silent = T)
      shiny::validate(
        need(class(data_snsrs_col) != "try-error", "Error, no data selected.")
      )
      data_snsrs_col <- get_locations()[[1]] %>% filter(., !grepl("KNMI|NL",station))

      # Update map with new markers to show selected
      # proxy <- leafletProxy('map') # set up proxy map

      leafletProxy("map") %>%
        addCircleMarkers(data = data_snsrs_col, ~lon, ~lat,stroke = TRUE, weight = 2,
                         label = lapply(data_snsrs_col$station, HTML),
                         layerId = ~station,
                         radius = 5,
                         color = data_snsrs_col$col,
                         group = "sensoren"
        )}

    add_sensors_map_update_button <- function(){

      # Check if there is data
      data_snsrs_col <- try(get_locations()[[1]], silent = T)
      shiny::validate(
        need(class(data_snsrs_col) != "try-error", "Error, no data selected.")
      )

      data_snsrs_col <- get_locations()[[1]] %>% filter(., !grepl("KNMI|NL",station))

      # Update map with new markers to show selected
      # proxy <- leafletProxy('map') # set up proxy map
      leafletProxy("map") %>%
        fitBounds(min(data_snsrs_col$lon), min(data_snsrs_col$lat), max(data_snsrs_col$lon), max(data_snsrs_col$lat)) %>%
        addCircleMarkers(data = data_snsrs_col, ~lon, ~lat,stroke = TRUE, weight = 2,
                         label = lapply(data_snsrs_col$station, HTML),
                         layerId = ~station,
                         radius = 5,
                         color = data_snsrs_col$col,
                         group = "sensoren"
        )}

    add_lmls_map <- function(){
      # Check if there is data
      data_snsrs <- try(get_locations()[[1]], silent = T)
      shiny::validate(
        need(class(data_snsrs) != "try-error", "Not yet selected any data.")
      )

      data_snsrs <- get_locations()[[1]] %>% filter(., grepl("LML",station_type))

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


    observe({
      knmi_show <- input$show_knmi

      if (isTRUE(knmi_show)){
        add_knmi_map()
      }
      else {
        remove_knmi_map()
      }
    })

    observeEvent(update_data(),{
        add_sensors_map_update_button()
    })


    # Observe if a sensor is in de square selection
    observe({

      rectangular_desel <- input$map_draw_deleted_features

      # ga dan de sensoren af en deselecteer deze een voor een
      for(id_select in isolate(get_locations()[[2]]$station)){
        change_state_to_deselected(id_select)
      }
      isolate(add_sensors_map())
      isolate(add_lmls_map())
    })

    observe({

      rectangular_sel <- input$map_draw_new_feature

      # Zoek de sensoren in de feature
      if (length(rectangular_sel[[1]] > 0)){
        # Check if there is data
        data_snrs <- try(get_locations(), silent = T)
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

    })

    # Observe the clicks of an user
    observe({
      click <- input$map_marker_click

      selected_snsr <- click$id
      log_trace("map module: click id {selected_snsr}")

      if (length(selected_snsr >0)){
        selected <- check_state(selected_snsr)
        if (selected == T){
          change_state_to_deselected(selected_snsr)
        }
        else {
          change_state_to_selected(selected_snsr)
        }}
      else{done}

      isolate(add_sensors_map())
      #isolate(add_lmls_map())
      #isolate(add_knmi_map())
    })

    # Return start and end date
    return(list(
      map = map,
      state_station = reactive({state_station$value}),
      show_knmi = reactive({input$show_knmi})))

  })

}

