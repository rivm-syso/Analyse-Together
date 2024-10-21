###############################################
### Show data availablility cache database ###
###############################################

# This module gets the data from the cache database and returns a calender
# plot showing how many sensors have measured in the period.
######################################################################
# Output Module
######################################################################

show_data_avail_cache_output <- function(id) {

  ns <- NS(id)
  tagList(

    p("Databeschikbaarheid"),
    info_sensor_output(ns("data_availability_calender")),
    textOutput(ns("data_avail_or_not"))

  )
}

######################################################################
# Server Module
######################################################################

show_data_avail_cache_server <- function(id,
                                          measurements_con,
                                          stations_name,
                                          start_time,
                                          end_time,
                                          rand_nr
) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # Create list where to observe changes to react on
    tolisten <- reactive({
      list(rand_nr
           )
    })

    # Observe is a new plot is needed
    observeEvent(tolisten(),{

      # Create visualisation of the available data in cache
      # If no data at all
      if(purrr::is_empty(stations_name)){

        # message to the user
        output$data_avail_or_not <- renderText({
          "No data found, please consider to get them."
        })

        # Create empty data_frame to plot
        data_in_tool_empty <- setNames(data.frame(matrix(ncol = 8, nrow = 0)),
                                       c("station", "parameter", "value",
                                         "aggregation", "timestamp",
                                         "date", "sd", "diff"))

        # Plot empty data, plot will give proper message
        info_sensor_server("data_availability_calender",
                           data_measurements = reactive({data_in_tool_empty}))

      }else{ # If there is data at least of this municipality/project
        # Load the data from the cache dbs
        data_in_tool <- get_measurements_cleaned(measurements_con,
                                                 stations_name,
                                                 parameter_input = "pm25_kal",
                                                 start_time,
                                                 end_time)

        # show which data is available, if no data available,
        # message is shown "no data"
        info_sensor_server("data_availability_calender",
                           data_measurements = reactive({data_in_tool}))

        # message to the user
        output$data_avail_or_not <- renderText({
          "Please check if this is the data expected."
        })

      }
    })

  })
}
