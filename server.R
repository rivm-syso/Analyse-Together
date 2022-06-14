
# Define server logic                                                       ====
shinyServer(function(global, input, output, session) {

  # The pickerInput for component selection
  select_component <- component_selection_server("select_component", comp_choices)
<<<<<<< HEAD
  
  # The Map 
  map <- show_map_server("map", sensor)
=======
>>>>>>> develop

  # The dateRangeInput for date range selection
  select_date_range <- date_range_server("select_date_range", communication_stuff)

  communication_stuff <- communication_server("test_comm_output",
                                                measurements,
                                                sensor,
                                                meta, # TODO willen we hier wat mee?
                                                selected_parameter = select_component,
                                                selected_time = select_date_range,
                                                # TODO Get the selected stations form the map
                                                selected_stations = c("SSK_LH003"),
                                                # Options for the colors
                                                col_cat,
                                                col_default,
                                                col_overload,
                                                # Options for the linetype
                                                line_cat,
                                                line_default,
                                                line_overload
                                                )



})
