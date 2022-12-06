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
    plotOutput(ns("show_available_data"))
  )
}

######################################################################
# Server Module
######################################################################

show_availability_server <- function(id,
                                     data_to_show) {

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

  })

}
