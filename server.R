
# Define server logic                                                       ====
shinyServer(function(global, input, output, session) {
    communication_stuff <- communication_server("test_comm_output",
                                                measurements,
                                                sensor,
                                                meta,
                                                # TODO Get the selected parameter form the module XXX
                                                selected_parameter = "pm10_kal",
                                                # TODO Get the selected timeperiod from the module XXX
                                                selected_time = list(start_time = as.POSIXct("20220202", format = "%Y%m%d"),
                                                                     end_time = as.POSIXct("20220203", format = "%Y%m%d")),
                                                # TODO Get the selected stations form the map
                                                selected_stations = c("SSK_LH003")
                                                )

})
