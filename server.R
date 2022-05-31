
# Define server logic                                                       ====
shinyServer(function(global, input, output, session) {


  # The dateRangeInput for date range selection
  select_date_range <- date_range_server("select_date_range", communication_stuff)


  communication_stuff <- communication_server("test_comm_output",
                                                measurements,
                                                sensor,
                                                meta,
                                                # TODO Get the selected parameter form the module XXX
                                                selected_parameter = "pm10_kal",
                                                # TODO Get the selected timeperiod from the module XXX
                                                selected_time = select_date_range,
                                                # TODO Get the selected stations form the map
                                                selected_stations = c("SSK_LH003")
                                                )


})
