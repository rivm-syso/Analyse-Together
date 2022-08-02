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

show_map_server <- function(id, com_module, sensor) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns
    beatCol <- colorFactor(palette = 'RdYlGn', domain = c(0,100), reverse = TRUE)
    icons_stations <- iconList(
      lml_selected = makeIcon(iconUrl = "images/lml_selected.png", iconWidth = 24, iconHeight = 16),
      lml_deselected = makeIcon(iconUrl = "images/lml_deselected.png",  iconWidth = 24, iconHeight = 16))
    
    # Get the min and max of the dataset
    get_locations <- reactive({
      sensorloc <- com_module$station_locations() %>% dplyr::distinct(station, .keep_all = T)
      sensorloc_coord <- SpatialPointsDataFrame(sensorloc[,c('lon','lat')],sensorloc)
      return(list(sensorloc, sensorloc_coord))})
    
    
    # Create an reactive to change with the obeserve, to store the clicked id
    state_station <- reactiveValues(value = "SSK_LH004")
    
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

    output$show_knmi <- renderUI({
      
      # Create the component picker with a list of possible choices
      tagList(
        
        checkboxInput(
          ns("show_knmi"),
          label    = "Show KNMI stations",
          value    = FALSE
        )
      )
    })    
    
    add_knmi_map <- function(){
      print(get_locations()[[1]])
      data_knmi <- get_locations()[[1]] %>% filter(., grepl("KNMI",station))
      #print(data_knmi)
      
      knmiicon <- makeIcon(
        iconUrl = "images/1555512.png",
        iconWidth = 20, iconHeight = 20)
      
      # Update map with new markers to show selected 
      proxy <- leafletProxy('map') # set up proxy map
      leafletProxy("map") %>%
        
        addMarkers(data = data_knmi, lng = ~lon, lat = ~lat, 
                   icon = knmiicon,
                   label = lapply(data_knmi$station, HTML),
                   layerId = ~station,
                   group = "knmi_stations"
        )
      
    }
    
    add_sensors_map <- function(){ 
      
      data_snsrs <- get_locations()[[1]] %>% filter(., !grepl("KNMI|NL",station))
      
      # Update map with new markers to show selected 
      proxy <- leafletProxy('map') # set up proxy map
      leafletProxy("map") %>%

        addCircleMarkers(data = data_snsrs, ~lon, ~lat,stroke = TRUE, weight = 2,
                         label = lapply(data_snsrs$station, HTML),
                         layerId = ~station,
                         radius = 5,
                         color = data_snsrs$col,
                         group = "sensoren"
        )}
    
    add_lmls_map <- function(){ 
      
      data_snsrs <- get_locations()[[1]] %>% filter(., grepl("LML",station_type))
      
      for (lmls in unique(data_snsrs$station)){
        
        print(lmls)
        
        if (isTRUE(data_snsrs$selected[data_snsrs$station == lmls])){
          # Update map with new markers to show selected 
          
          proxy <- leafletProxy('map') # set up proxy map
          leafletProxy("map") %>%
            
            addMarkers(data = data_snsrs, ~lon, ~lat, icon = icons_stations["lml_selected"],
                             label = lapply(data_snsrs$station, HTML),
                             layerId = ~station,
                             #radius = 5,
                             #color = data_snsrs$col,
                             group = "lmls")
        }
        else {
          
          leafletProxy("map") %>%
          
          addMarkers(data = data_snsrs, ~lon, ~lat, icon = icons_stations["lml_deselected"],
                     label = lapply(data_snsrs$station, HTML),
                     layerId = ~station,
                     #radius = 5,
                     #color = data_snsrs$col,
                     group = "lmls")}}
        }
    
    
    remove_knmi_map <- function(){

      # Update map with new markers to show selected 
      proxy <- leafletProxy('map') # set up proxy map
      leafletProxy("map") %>%
        
        clearGroup(group = "knmi_stations")
    }
    
    
    observe({
      knmi_show <- input$show_knmi
      #print(knmi_show)
      if (isTRUE(knmi_show)){
        add_knmi_map()
      }
      else {
        remove_knmi_map()
      }
    })
    
    
    # Observe if a sensor is clicked and store the id
    observe({
      
      rectangular_desel <- input$map_draw_deleted_features
      
      
      # ga dan de sensoren af en deselecteer deze een voor een
      for(id_select in isolate(get_locations()[[2]]$station)){
          #print(id_select)
          change_state_to_deselected(id_select)
          }
      isolate(add_sensors_map())  
      isolate(add_lmls_map())
    })
    
    observe({
      
      rectangular_sel <- input$map_draw_new_feature
      
      # Zoek de sensoren in de feature
      if (length(rectangular_sel[[1]] > 0)){
        found_in_bounds <- geoshaper::findLocations(shape = rectangular_sel,
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
       isolate(add_lmls_map())
    })
    
    # Return start and end date
    return(list(
                map = map,
                state_station = reactive({state_station$value}),
                show_knmi = reactive({input$show_knmi})))

  })

}

