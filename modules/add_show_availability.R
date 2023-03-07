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
    textOutput(ns("show_stations_text")),
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

    # Plot to visualise the number of stations available tov the total
    output$show_available_data <- renderPlot({
      # Check if data is there
      shiny::validate(need((nrow(data_to_show$data_all)>0), message = "No data available."))

      # Count the number of stations with data
      number_stations <- data_to_show$data_all %>%
        dplyr::filter(parameter == "pm25_kal") %>%
        dplyr::select(station) %>%
        unique() %>%
        count() %>% pull()

      # Get the total number of stations in this municipality/project
      data_total <- data_stations$data %>% nrow()

      # Set a label and combine data for the plot
      label_bar = paste0(number_stations, " Available in tool")
      data_plot <- data.frame(aantal = c( data_total - number_stations, number_stations),
                              col = factor(c( "#edffed", "#006400"), levels = c("#edffed", "#006400")),
                              x = c("test", "test"))

      # Render the plot
      ggplot(data_plot, aes(x = x, fill = col, y = aantal)) +
        scale_fill_identity() +
        geom_bar(stat = "identity", show.legend = F, color = "black") +
        # Add some text to the plot
        geom_label(aes(label = label_bar, x = x, y = number_stations ), nudge_y = 0, colour = "white", fill = "black", size = 4) +
        # Remove all axis and labels etc.
        theme_void()
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

    # Show data availability in table (per station per week)
    # For now redundant, but maybe interested later
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
