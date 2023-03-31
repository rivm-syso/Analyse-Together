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
    textOutput(ns("show_stations_text"))
  )
}

######################################################################
# Server Module
######################################################################

show_availability_server <- function(id,
                                     data_stations_all,
                                     data_stations_with_data
) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # Show number of stations in text
    output$show_stations_text <- renderText({
      shiny::validate(need((nrow(data_stations_all())>0), message = "No data available."))

      # Count the number of stations with data
      number_stations <- data_stations_with_data() %>% nrow()

      # Get the total number of stations in this municipality/project
      data_total <- data_stations_all() %>% nrow()

      paste0("Er is data beschikbaar van ", number_stations, " van de ",
             data_total, " stations. Het totaal aantal stations zijn alle stations
             die ooit in deze gemeente/project aanwezig waren. Daardoor kan het
             voorkomen dat het aantal stations voor de gekozen tijdsperiode lager ligt.")

    })

  })

}
