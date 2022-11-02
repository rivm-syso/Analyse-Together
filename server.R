
# Define server logic                                                       ====
shinyServer(function(global, input, output, session) {

  # To change the language in the tool
  observeEvent(input$selected_language, {
    # Here is where we update language in session
    shiny.i18n::update_lang(session, input$selected_language)
  })

  # The pickerInput for component selection
  select_component <- component_selection_server("select_component", comp_choices)

  # select project/mun
  proj_or_mun_select <- project_or_mun_selection_server("proj_or_mun_select", select_choices)

  # The dateRangeInput for date range selection
  select_date_range <- date_range_server("select_date_range", communication_stuff)

  # choose proj/mun
  choice_select <- choice_selection_server("choice_select", com_module = communication_stuff, mun_choices = mun_choices, proj_choices = proj_choices)

  # Get metadata
  meta_table <- metadata_server("meta_table", communication_stuff)

  # The Map
  map <- show_map_server("map", communication_stuff, update_data = update_data_button)

  # The bar plot
  barplot <- barplot_server("barplot_plot", communication_stuff, overview_component, theme_plots)

  # The timeseries plot
  timeseries_plot <- timeseries_server(id = "timeseries_plot", data_measurements_stations = communication_stuff, overview_component, theme_plots)

  # The pollutionrose plot
  pollrose_plot <- pollrose_server("pollrose_plot", communication_stuff)

  # The timevariation plot
  timevar_plot_weekly <- timevar_weekly_server("timevar_plot_weekly", communication_stuff)
  timevar_plot_daily <- timevar_daily_server("timevar_plot_daily", communication_stuff)

  # The communication module
  communication_stuff <- communication_server("test_comm_output",
                                              update_data = update_data_button,
                                              download_data_123 = download_api_button,
                                              pool = pool,
                                              measurements_con = measurements_con,
                                              stations_con = stations_con,
                                              meta, # TODO willen we hier wat mee?
                                              selected_parameter = select_component,
                                              selected_time = select_date_range,
                                              default_time = default_time,
                                              select_mun_or_proj = proj_or_mun_select,
                                              choose_mun_or_proj = choice_select,
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

  download_api_button <- download_api_button_server("dl_btn_pushed", proj_or_mun_select , choice_select, select_date_range, pool, que)
  update_data_button <- update_data_button_server("update_data")

  view_que_server("view_que", que)

})
