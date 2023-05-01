###############################################
#### Module for the download to computer ####
###############################################

# This is module to download the data to computer of the user
######################################################################
# Output Module ----
######################################################################
download_pc_button_output <- function(id) {

  ns <- NS(id)
  downloadButton(ns("download_pc_button"), i18n$t("btn_pc"))
}


######################################################################
# Server Module ----
######################################################################

download_pc_button_server <- function(id,
                                      data_stations,
                                      data_measurements,
                                      name_munproj,
                                      selected_start_date,
                                      selected_end_date){

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

      # Download all data ----
      output$download_pc_button <- downloadHandler(
        # geef de filename op, zou via interactieve kunnen
        filename = function(){
          # Create file name
          paste0('data_', name_munproj(),'_', selected_start_date(),
                 '_', selected_end_date(), '.csv')
        },
        # bepaal de content van de download
        content = function(file) {
          # Get the data in wide for download
          data_to_download <- data_measurements() %>%
            dplyr::filter(parameter %in% c("pm10_kal", "pm10", "pm25_kal", "pm25", "wd", "ws", "temp", "pres", "rh")) %>%
            dplyr::select(c(station, date, parameter, value)) %>%
            unique() %>%
            tidyr::pivot_wider(values_from = value, names_from = parameter)

          # Get the locations and add to data
          data_locations <- data_stations() %>%
            dplyr::select(station, lat, lon) %>%
            dplyr::mutate(lat = round(lat, 3),
                          lon = round(lon,3))
          data_to_download <- data_to_download %>%
            dplyr::left_join(data_locations, by = "station")

          # Add some explanation and source to the file
          write.table(i18n$t("expl_download_to_pc_expl"), file, row.names = FALSE)

          # Write output to user
          write.table(data_to_download, file, sep = ',', row.names = FALSE, append = T)
        }
      )
  })
}
