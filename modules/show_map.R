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
  verbatimTextOutput(ns('Click_text')),
  uiOutput(ns('pass_sensor')),
  )

}


######################################################################
# Server Module
######################################################################

show_map_server <- function(id, com_module, sensor) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns
    beatCol <- colorFactor(palette = 'RdYlGn', domain = c(0,100), reverse = TRUE)

    # Get the min and max of the dataset
    get_locations <- reactive({
      sensorloc <- com_module$station_locations()
      return(sensorloc)})

    # Create an reactive to change with the obeserve, to store the clicked id
    state_station <- reactiveValues(value = "SSK_LH003")
    
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
    
      #Generate base map ----
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

    # Observe if a sensor is clicked and store the id
    observe({
      click <- input$map_marker_click
      #rectangular_sel <- input$map_draw_new_feature
      #
      # Zoek de sensoren in de feature
      # found_in_bounds <- geoshaper::findLocations(shape = input$map_draw_new_feature
      #                                  ,
      #                                  location_coordinates = get_locations()$lat,
      #                                  location_id_colname = "kit_id")

      #print(found_in_bounds)
      # Ga elke sensor af en voeg deze bij de selectie
      # if (length(found_in_bounds > 0)){
      #     for(id_select in found_in_bounds){
      #       #check_selected_id(id_select)
      #       print(id_select)
      # }}
      # else{done}
      
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
      
      leafletProxy("map") %>%
        addCircleMarkers(data = get_locations(), ~lon, ~lat,
                       label = lapply(get_locations()$station, HTML),
                       layerId = ~station,
                       radius = 5,
                       color = get_locations()$col
      )
        

    })

    # Return start and end date
    return(list(
                map = map,
                state_station = reactive({state_station$value})))

  })

}

