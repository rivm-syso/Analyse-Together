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
                                     data_stations_all,
                                     data_stations_with_data,
                                     selected_start_date,
                                     selected_end_date
) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # Plot to visualise the number of stations available tov the total
    output$show_available_data <- renderPlot({
      # Check if data is there
      shiny::validate(need((nrow(data_stations_all())>0), message = "No data available."))

      # Count the number of stations with data
      number_stations <- data_stations_with_data() %>% nrow()

      # Get the total number of stations in this municipality/project
      data_total <- data_stations_all() %>% nrow()

      # Set a label and combine data for the plot
      label_bar = paste0(number_stations, " Available in tool")
      data_plot <- data.frame(aantal = c( data_total - number_stations, number_stations),
                              col = factor(c( "#edffed", "#006400"),
                                           levels = c("#edffed", "#006400")),
                              x = c("test", "test"))

      # Render the plot
      ggplot(data_plot, aes(x = x, fill = col, y = aantal)) +
        scale_fill_identity() +
        geom_bar(stat = "identity", show.legend = F, color = "black") +
        # Add some text to the plot
        geom_label(aes(label = label_bar, x = x, y = number_stations ), nudge_y = 0,
                   colour = "white", fill = "black", size = 4) +
        # Remove all axis and labels etc.
        theme_void()
    })


    # Show number of stations in text
    output$show_stations_text <- renderText({
      shiny::validate(need((nrow(data_stations_all())>0), message = "No data available."))

      # Count the number of stations with data
      number_stations <- data_stations_with_data() %>% nrow()

      # Get the total number of stations in this municipality/project
      data_total <- data_stations_all() %>% nrow()

      paste0("Er is data beschikbaar van ", number_stations, " van de ",
             data_total, " stations.")

    })

  })

}
