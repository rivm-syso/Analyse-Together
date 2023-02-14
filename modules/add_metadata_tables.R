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

metadata_server <- function(id,
                            data_measurements,
                            data_stations,
                            time_period,
                            name_munproj) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # Get selected measurements from communication module
    metadata_table <- reactive({
      # Get the data measurements
      data_all <- data_measurements()

      # Check if there is data
      shiny::validate(
        need(!is_empty(data_all),'Geen sensordata beschikbaar.'),
        need(!dim(data_all)[1] == 0,'Geen sensordata beschikbaar.')
      )

      # Get the info of the stations
      data_stations_part <- data_stations() %>%
        dplyr::select(c(station, lat, lon, station_type)) %>%
        dplyr::distinct(station, .keep_all = T)

      # Get the selected time period and the number of hours
      start_time <- time_period$selected_start_date()
      end_time <- time_period$selected_end_date()
      timerange <- difftime(end_time, start_time, unit = "hours")

      # Prepare the data for the table - do some summarise
      metadata_table <- data_all %>%
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

      # Add the station information to the measurements
      metadata_table <- metadata_table %>%
        dplyr::left_join(data_stations_part, by = "station")

      return(metadata_table)
    })

    # Get the name of chosen municipality/project
    project_or_municipality <- reactive({
      project_or_municipality <- name_munproj()
      return(project_or_municipality)
    })

    # Define colour-breaks for table
    breaks_col <- reactive({
      # Get the selected time period and the number of hours
      start_time <- time_period$selected_start_date()
      end_time <- time_period$selected_end_date()
      timerange <- difftime(end_time, start_time, units="hours")
      # Set the breaks and the colours
      brks <- quantile(c(0,as.numeric(timerange)), probs = seq(.05, .95, .1), na.rm = TRUE)
      clrs <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>%
        {paste0("rgb(", ., ", 243,", .,", 0.4)")}

      return(list(brks = brks, clrs = clrs))
    })

    output$meta_table <-

      renderDataTable({
        data_for_table <- metadata_table()
        try(datatable(data_for_table,
                      colnames = c("Station" = "station",
                                   "Number of measurements PM2.5" = "Number of measurements pm25",
                                   "Number of measurements PM2.5 - kal" = "Number of measurements pm25_kal",
                                   "Number of measurements PM10" = "Number of measurements pm10",
                                   "Number of measurements PM10 - kal" = "Number of measurements pm10_kal",
                                   "Number of measurements Temp" = "Number of measurements temp",
                                   "Number of measurements RH" = "Number of measurements rh",
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
                                      "Number of measurements RH"),
                          backgroundColor = styleInterval(cuts = breaks_col()$brks, values = breaks_col()$clrs))
        )})
  })
}
