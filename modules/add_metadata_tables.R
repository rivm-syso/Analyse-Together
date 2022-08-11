###############################################
### Metadata Module ###
###############################################

# Creates metadata tables based on selected input

###################################################################
### Output Module ####
#################################################################
metadata_output <- function(id) {
  
  ns <- NS(id)
  
  dataTableOutput(ns("meta_table"))
  
}


######################################################################
# Server Module
######################################################################
#
#

metadata_server <- function(id, com_module) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # Determine parameter that needs to be plotted
    # Get selected measurements from communication module
    metadata_table <- reactive({
      data_measurements <- com_module$selected_measurements()
      metadata_table <- data_measurements %>%  group_by(station) %>% 
        mutate(n_obs = n(),
               first_m = min(timestamp),
               last_m = max(timestamp)) %>% 
        select(station, n_obs, first_m, last_m) %>% distinct(station, .keep_all = T)
      return(metadata_table)
    })
    
    # Get selected stations from communication module
    data_merged <- reactive({
      data_stations <- com_module$station_locations() %>% select(c(station, lat, lon, station_type)) %>% dplyr::distinct(station, .keep_all = T)
      
      if (nrow(metadata_table() > 0)){
          data_merged <- left_join(metadata_table(),data_stations, by = "station")
      }
      else {data_merged <- data.frame("station" = '', "n_obs" = '')}
      return(data_merged)
    })
    
    project_or_municipality <- reactive({
      project_or_municipality <- com_module$mun_proj_select()
      return(project_or_municipality)
    })
    
    output$meta_table <- 
      try(renderDataTable({
             datatable(data_merged(),colnames = c("Number of observations" = "n_obs"),
                       caption = paste0("Table for ",unique(com_module$selected_measurements()$parameter),", within ", project_or_municipality()))
    }))
    
    
  })
  
  
}





