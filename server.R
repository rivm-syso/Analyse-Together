
# Define server logic                                                       ====
shinyServer(function(global, input, output, session) {

  # The pickerInput for component selection
  select_component <- component_selection_server("select_component", comp_choices)

  # select project/mun
  proj_or_mun_select <- project_or_mun_selection_server("proj_or_mun_select")
  
  # choose proj/mun
  choice_select <- choice_selection_server("choice_select", communication_stuff, mun_choices = mun_choices, proj_choices = proj_choices)
  
  # The Map 
  map <- show_map_server("map", communication_stuff, sensor)

  # The Barplot
  barplot <- barplot_server("barplot_plot", communication_stuff)

  # The dateRangeInput for date range selection
  select_date_range <- date_range_server("select_date_range", communication_stuff)
  
  # The timeseries plot 
  timeseries_plot <- timeseries_server("timeseries_plot", data_measurements_stations = communication_stuff, overview_component)
  # timeseries_plot <- timeseries_server("timeseries_plot", test_measurements, test_loc_col, overview_component)

  # The communication module
  communication_stuff <- communication_server("test_comm_output",
                                                measurements,
                                                sensor,
                                                meta, # TODO willen we hier wat mee?
                                                selected_parameter = select_component,
                                                selected_time = select_date_range,
                                                select_mun_or_proj = proj_or_mun_select,
                                                choose_mun_or_oroj = choice_select,
                                                # TODO Get the selected stations form the map
                                                #selected_stations = c("SSK_LH004"),
                                                selected_stations = map,
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
