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
    
    # Get selected measurements from communication module
    metadata_table <- reactive({
      
      data_measurements <- com_module$all_measurements()
      timerange <- difftime(com_module$selected_time()$end_time, com_module$selected_time()$start_time, units="hours")
      
      if (nrow(data_measurements)>0){
          metadata_table <- data_measurements %>%  
                            dplyr::group_by(station) %>% 
                            dplyr::mutate(first_m = as.POSIXct(as.numeric(min(timestamp)), origin='1970-01-01') %>% format(., "%d %b %Y"),
                                          last_m = as.POSIXct(as.numeric(max(timestamp)), origin='1970-01-01') %>% format(., "%d %b %Y")) %>%
                            dplyr::group_by(station, parameter) %>% 
                            dplyr::filter((parameter != 'no') & (parameter != 'no2') &
                                          (parameter != 'o3') & (parameter != 'nh3')) %>% 
                            dplyr::summarise(first_m = first_m,
                                             last_m = last_m,
                                             n_obs = sum(!is.na(value)),
                                             max_obs = timerange) %>%
                            dplyr::distinct(station, .keep_all = T) %>% 
                            tidyr::pivot_wider(.,values_from = n_obs, names_from = parameter, names_prefix = "Number of measurements ") 
      }
      else{
        metadata_table <- data_measurements %>% 
          dplyr::mutate(n_obs = NA,
                        max_obs = timerange,
                        first_m = NA,
                        last_m = NA) %>%
          dplyr::select(station, max_obs, n_obs, first_m, last_m) %>%
          dplyr::distinct(station, .keep_all = T)
      }

      return(metadata_table)
    })
    
    # Get selected stations from communication module
    data_merged <- reactive({
      data_stations <- com_module$station_locations() %>% select(c(station, lat, lon, station_type)) %>% dplyr::distinct(station, .keep_all = T)
      data_merged <- left_join(data_stations,metadata_table(), by = "station")
      
      return(data_merged)
    })
    
    project_or_municipality <- reactive({
      project_or_municipality <- com_module$mun_proj_select()
      return(project_or_municipality)
    })
  
    # Define colour-breaks for table:
    breaks_col <- reactive({
      timerange <- difftime(com_module$selected_time()$end_time, com_module$selected_time()$start_time, units="hours")
      brks <- quantile(c(0,as.numeric(timerange)), probs = seq(.05, .95, .1), na.rm = TRUE)
      clrs <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>%
        {paste0("rgb(", ., ", 243,", .,", 0.4)")}
      
      return(list(brks,clrs))
    })
    
    output$meta_table <-
      
      renderDataTable({
        
        shiny::validate(need(nrow(com_module$all_measurements()) >0, message =  "Select one or more sensors (with data)"))
        
        # Determine parameter that needs to be plotted
        n_obs_sel <- data_merged()$station
        if(length(n_obs_sel>1)){
          
          try(datatable(data_merged(),colnames = c("Number of measurements PM2.5" = "Number of measurements pm25", 
                                                   "Number of measurements PM2.5 - kal" = "Number of measurements pm25_kal", 
                                                   "Number of measurements PM10" = "Number of measurements pm10", 
                                                   "Number of measurements PM10 - kal" = "Number of measurements pm10_kal", 
                                                   "Number of measurements Temp" = "Number of measurements temp", 
                                                   "Number of measurements RH" = "Number of measurements rh",
                                                   "Number of measurements Press" = "Number of measurements pres",
                                                   "Maximum measurements" = "max_obs", 
                                                   "First measurements" = "first_m", 
                                                   "Last measurements" = "last_m", 
                                                   "Type" = "station_type", 
                                                   "Latitude" = "lat", 
                                                   "Longitude" = "lon"),
                        caption = paste0(i18n$t("word_table")," ",
                                         ","," ", i18n$t("word_within"), " ",project_or_municipality()),
                        options = list(scrollX = TRUE, pageLength = 12, lengthChange = FALSE), class = c('row-border', 'hover'), rownames = FALSE) %>%
                formatStyle("Number of measurements PM2.5", background = "white") %>% 
                formatStyle(columns = c("Number of measurements PM2.5", 
                                        "Number of measurements PM2.5 - kal",
                                        "Number of measurements PM10",
                                        "Number of measurements PM10 - kal",
                                        "Number of measurements Temp",
                                        "Number of measurements RH",
                                        "Number of measurements Press"),
                            backgroundColor = styleInterval(cuts = breaks_col()[[1]], values = breaks_col()[[2]]))
          )
        }})      
  })   
}
