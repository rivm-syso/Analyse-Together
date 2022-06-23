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
  uiOutput(ns('sensormap'))
  #textOutput(ns('sensormap'))
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
    
      #Generate base map ----
      output$map <- renderLeaflet({
        ns("map")
        leaflet() %>%
          setView(5.384214, 52.153708 , zoom = 7) %>%
          addTiles() %>%
          addCircleMarkers(data = get_locations(), ~lon, ~lat,
                           label = lapply(get_locations()$station, HTML),
                           layerId = ~station,
                           radius = 5,
                           color = get_locations()$col
                           ) %>%

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

          addLegend('bottomright', pal = beatCol, values = sensor$col,
                    title = 'Sensors',
                    opacity = 1) %>%

          addEasyButton(easyButton(
            icon="fa-globe", title="Zoom to Level 7",
            onClick=JS("function(btn, map){ map.setZoom(7); }"))) %>%
          addEasyButton(easyButton(
            icon="fa-crosshairs", title="Locate Me",
            onClick=JS("function(btn, map){ map.locate({setView: true}); }"))) %>%
          addScaleBar(position = "bottomleft")


      })

    # Return the chosen sensors/change the color/...? Define an output; with dataframe of clicked sensors?
    # Return this and use this in the server as input for the com_module

    output$sensormap <- renderUI({
      
      # Create the component picker with a list of possible choices
      tagList(
        
        pickerInput(
          ns("sensormap"),
          label    = "Select id",
          choices  = sensor$station
        )
      )
    })
    
      
    # Return start and end date
    return(list(sensormap = reactive({ input$sensormap })
                #))
                ,
                map = map))
      
  })
  
}

