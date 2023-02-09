###############################################
### Individual sensor Time Series Plot Module ###
###############################################

# Choose 1 sensor of the selection and Creates a time series plot

###################################################################
### Output Module ####
#################################################################

individual_timeseries_output <- function(id) {

  ns <- NS(id)

  tagList(
    uiOutput(ns("sensor_indu")),

    timeseries_output(ns("individual_timeseries_plot"))
  )

}


######################################################################
# Server Module
######################################################################

individual_timeseries_server <- function(id,
                                data_measurements,
                                parameter,
                                overview_component,
                                theme_plots){

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # Get the selected station names
    get_station_select <- reactive({

      # Check if there is any data available
      shiny::validate(
        need(!purrr::is_null(data_measurements()),
             'Geen sensordata beschikbaar.')
      )

      # Get the unique station names
      station_select <- data_measurements() %>%  dplyr:: select(station) %>%  unique() %>% pull()
      return(station_select)
      })


    output$sensor_indu <- renderUI({
      # Check if stations are selected
      if (is.null(get_station_select()) == FALSE){
        # Get the names of the selected stations for the choices of the pickerinput
        stations_select <- get_station_select()

       # Create picker with the station names
          tagList(

            pickerInput(
              ns("sensor_indu"),
              label    = "choose station",
              choices  = stations_select,
              selected = FALSE,
              multiple = FALSE,
              options = pickerOptions(maxOptions = 1)
            )
          )}

    })

    # Observe if the pickerinput has changed
    observeEvent(input$sensor_indu, {
      # Get the name of the station
      indu_station_name <- input$sensor_indu
      # Get the measurements of the station
      measurements <- data_measurements() %>% dplyr::filter(station == indu_station_name)
      # Create timeseries plot
      timeseries_server("individual_timeseries_plot",
                        data_measurements = reactive(measurements),
                        parameter = parameter,
                        overview_component = overview_component,
                        theme_plots = theme_plots)
    })

  })
}
