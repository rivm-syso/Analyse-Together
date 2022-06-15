###############################################
### ...Input - select sensor/project/data-range? ###
###############################################

# This is a map module
######################################################################
# Output Module
######################################################################

show_map_output <- function(id) {
  
  ns <- NS(id)
  
  leafletOutput(ns('map'))  
  
}


######################################################################
# Server Module
######################################################################

show_map_server <- function(id, sensor) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    beatCol <- colorFactor(palette = 'RdYlGn', domain = c(0,100), reverse = TRUE)
    
      # Generate base map ----
      output$map <- renderLeaflet({
        leaflet() %>% 
          setView(5.384214, 52.153708 , zoom = 7) %>%
          addTiles() %>%
          addCircleMarkers(data = sensor, ~lon, ~lat, 
                           label = lapply(sensor$station, HTML),
                           layerId = ~station,
                           radius = 5, 
                           color = ~col
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
    
    # Return the chosen sensors/change the color/...?
    #return(map = reactive({input$map}))
    
  })
  
}

