###############################################
### Show available data ###
###############################################

# This is a module to show a short overview of available data
######################################################################
# Output Module
######################################################################

show_availability_output <- function(id) {

  ns <- NS(id)
  tagList(
    tableOutput(ns("show_stations")),
    textOutput(ns("show_stations_text")),
    dataTableOutput(ns("show_available_data_table")),
    plotOutput(ns("show_available_data"))
  )
}

######################################################################
# Server Module
######################################################################

show_availability_server <- function(id,
                                     data_to_show,
                                     data_stations,
                                     time_period
) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # Show data availability in Calenderplot
    # output$show_available_data <- renderPlot({
    #   shiny::validate(need((nrow(data_to_show$data_all)>0), message = "No data available."))
    #
    #   data_to_plot <- data_to_show$data_all %>% dplyr::filter(parameter == "pm25_kal")
    #
    #   calendarPlot(data_to_show$data_all, pollutant = "value", breaks = c(0, 1000),
    #                labels = c("Data available"), cols = "darkgreen", remove.empty = F)
    #
    # })

    output$show_available_data <- renderPlot({
      shiny::validate(need((nrow(data_to_show$data_all)>0), message = "No data available."))

      data_to_plot <- data_to_show$data_all %>%
        dplyr::filter(parameter == "pm25") %>%
        dplyr::select(station, parameter, date) %>%
        dplyr::mutate(timestamp_table = date %>% format("%Y-%m-%d") %>% as.POSIXct(format ="%Y-%m-%d")) %>%
        dplyr::select(-date) %>%
        unique() %>%
        group_by(timestamp_table, parameter) %>%
        dplyr::mutate(number_a_day = n()) %>%
        ungroup() %>%
        # Needed for the openair plot
        dplyr::mutate(date = timestamp_table)

      max_stations <- data_stations$data %>% nrow()


      calendarPlot(data_to_plot, pollutant = "number_a_day", limits = c(0, max_stations), main = "Aantal sensoren per dag")
                   #
                   # , breaks = c(0, 0.25*max_stations, 0.5*max_stations, 75*max_stations),
                   # labels = c("geen sensoren", "25%", "50%","75%", "100%"), cols = c("grey","yellow","green","darkgreen","black"), remove.empty = F)

    })

    # Show number of stations in table
    output$show_stations <- renderTable({
      shiny::validate(need((nrow(data_to_show$data_all)>0), message = "No data available."))
      data_to_plot <- data_to_show$data_all %>%
        dplyr::filter(parameter == "pm25_kal") %>%
        dplyr::select(station) %>%
        unique() %>%
        count()

      data_total <- data_stations$data %>% nrow()

      data_to_plot <- data.frame("aantal stations PM2.5" = data_to_plot$n,
                                 "totaal aantal stations" = data_total)

    })

    # Show number of stations in text
    output$show_stations_text <- renderText({
      shiny::validate(need((nrow(data_to_show$data_all)>0), message = "No data available."))
      data_to_plot <- data_to_show$data_all %>%
        dplyr::filter(parameter == "pm25_kal") %>%
        dplyr::select(station) %>%
        unique() %>%
        count()

      data_total <- data_stations$data %>% nrow()

      paste0("Er is data beschikbaar van ", data_to_plot$n, " van de ", data_total, " stations.")

    })

    # Show data availability in table (per station)
    output$show_available_data_table <- renderDataTable({
      # Check if data available to show
      shiny::validate(need((nrow(data_to_show$data_all)>0), message = "No data available."))

      # Get the data measurements
      data_measurements <- data_to_show$data_all

      # Get the selected time period and the number of days
      start_time <- time_period$selected_start_date()
      end_time <- time_period$selected_end_date()

      diff_time <- end_time - start_time

      # Obtain unique data points
      data_measurements <- data_measurements %>%
        dplyr::select(station, date, parameter) %>%
        unique()

      # Aggregate the data depending how many days are selected
      if(diff_time > 96){
        # aggregate the measurements per month
        data_for_table <- data_measurements %>%
          dplyr::mutate(timestamp_table = date %>% format("%Y-%m-01") %>% as.POSIXct(format ="%Y-%m-%d")) %>%
          group_by(station, timestamp_table, parameter) %>%
          dplyr::summarize(aantal = n(),
                           total = 24*7*30,
                           percent = round(aantal/total*100, 0)) %>%
          ungroup()

      }else if(diff_time > 7){
        # aggregate the measurements per week
        data_for_table <- data_measurements %>%
          dplyr::mutate(timestamp_year = lubridate::year(date) ,
                        timestamp_table = as.POSIXct(paste(timestamp_year, lubridate::week(date), 1, sep="-"),format = "%Y-%W-%u") ) %>%
          group_by(station, timestamp_table, parameter) %>%
          dplyr::summarize(aantal = n(),
                           total = 24*7,
                           percent = round(aantal/total*100, 0)) %>%
          ungroup()

      }else if(diff_time > 0){
        # aggregate the measurements per day
        data_for_table <- data_measurements %>%
          dplyr::mutate(timestamp_table = date %>% format("%Y-%m-%d") %>% as.POSIXct(format ="%Y-%m-%d")) %>%
          group_by(station, timestamp_table, parameter) %>%
          dplyr::summarize(aantal = n(),
                           total = 24,
                           percent = round(aantal/total*100, 0)) %>%
          ungroup()

      }

      # Select the columns to plot etc
      data_for_table <- data_for_table %>%
        dplyr::filter(parameter == "pm25") %>%
        dplyr::select(station, timestamp_table, parameter, percent) %>%
        tidyr::pivot_wider(names_from = timestamp_table, values_from = percent)

      # Define colour-breaks for table
        brks <- quantile(c(0, 100), probs = seq(.05, .95, .3), na.rm = TRUE)
        clrs <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>%
          {paste0("rgb(", ., ", 243,", .,", 0.4)")}

      # Output for the table
      names_cols_time <- data_for_table %>% dplyr::select(-c(parameter, station)) %>% names()
      return(datatable(data_for_table, options = list(scrollX = TRUE, pageLength = 12, lengthChange = FALSE), class = c('row-border', 'hover'), rownames = FALSE) %>%
        formatStyle(names_cols_time, backgroundColor = styleInterval(brks, clrs)))

    })


  })

}
