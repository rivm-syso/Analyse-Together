
# Define server logic                                                       ====
shinyServer(function(global, input, output, session) {

  
  # The dateRangeInput for date range selection 
  select_date_range <- date_range_server("select_date_range")
  

  # The pickerInput for component selection 
  select_component <- component_selection_server("select_component", comp_choices)
  



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
