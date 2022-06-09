###############################################
### ...Input - select sensor? ###
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
          addTiles() %>%
          addCircleMarkers(data = sensor, ~lon, ~lat, 
                           #layerId = ~Group.1, 
                           label = lapply(sensor$station, HTML),
                           radius = 5, 
                           color = ~col
                           ) %>%
          
          addLegend('bottomright', pal = beatCol, values = sensor$col,
                    title = 'Sensors',
                    opacity = 1)
      })
    
    # Return the chosen sensors/change the color/...?
    #return(map = reactive({input$map}))
    
  })
  
}

