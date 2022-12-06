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

    output$show_available_data <- renderPlot({
      shiny::validate(need((nrow(data_to_show$data)>0), message = "No data available."))

      data_to_plot <- data_to_show$data %>% dplyr::filter(parameter == "pm25_kal")

      calendarPlot(data_to_show$data, pollutant = "value", breaks = c(0, 1000),
                   labels = c("Data available"), cols = "darkgreen", remove.empty = F)

    })

    output$show_stations <- renderTable({
      shiny::validate(need((nrow(data_to_show$data)>0), message = "No data available."))
      data_to_plot <- data_to_show$data %>%
        dplyr::filter(parameter == "pm25_kal") %>%
        dplyr::select(station) %>%
        unique() %>%
        count()

      data_to_plot <- data.frame("aantal stations PM2.5" = data_to_plot$n)

    })

    output$show_available_data_table <- renderDataTable({
      # Check if data available to show
      shiny::validate(need((nrow(data_to_show$data)>0), message = "No data available."))

      # Get the data measurements
      data_measurements <- data_to_show$data

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

