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

      shiny::validate(need(nrow(com_module$selected_measurements()) >0, message =  "Select one or more sensors (with data)"))
      data_measurements <- com_module$selected_measurements()

      timerange <- difftime(com_module$selected_time()$end_time, com_module$selected_time()$start_time, units="hours")
      metadata_table <- data_measurements %>%  group_by(station) %>%
                        mutate(n_obs = n(),
                               max_obs = timerange,
                               first_m = as.POSIXct(as.numeric(min(timestamp)), origin='1970-01-01') %>% format(., "%d %b %Y"),
                               last_m = as.POSIXct(as.numeric(max(timestamp)), origin='1970-01-01') %>% format(., "%d %b %Y")) %>%
                        select(station, max_obs, n_obs, first_m, last_m) %>%
                        distinct(station, .keep_all = T)

      return(metadata_table)
    })

    # Get selected stations from communication module
    data_merged <- reactive({

      data_stations <- com_module$station_locations() %>% select(c(station, lat, lon, station_type)) %>% dplyr::distinct(station, .keep_all = T)
      data_merged <- left_join(metadata_table(),data_stations, by = "station")

      return(data_merged)
    })

    project_or_municipality <- reactive({
      project_or_municipality <- com_module$mun_proj_select()
      return(project_or_municipality)
    })



    breaks_col <- reactive({
      brks <- quantile(data_merged()['n_obs'], probs = seq(.05, .95, .1), na.rm = TRUE)
      clrs <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>%
        {paste0("rgb(", ., ", 243,", ., ")")}

      return(list(brks,clrs))
    })

    output$meta_table <-

      renderDataTable({

        # Determine parameter that needs to be plotted
        n_obs_sel <- metadata_table()$n_obs

        if(length(n_obs_sel>1)){
          try(datatable(data_merged(),colnames = c("Number of measurements" = "n_obs", "Maximum measurements" = "max_obs", "First measurements" = "first_m", "Last measurements" = "last_m", "Type" = "station_type", "Latitude" = "lat", "Longitude" = "lon"),
                       caption = paste0(i18n$t("word_table")," ",unique(com_module$selected_measurements()$parameter),","," ", i18n$t("word_within"), project_or_municipality()),options = list(scrollX = TRUE)) %>%
            formatStyle("Number of measurements", backgroundColor = styleInterval(cuts = breaks_col()[[1]], values = breaks_col()[[2]]))
          )
        }})


  })


}





