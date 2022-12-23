# Define server logic                                                       ====
shinyServer(function(global, input, output, session) {

  # To change the language in the tool
  observeEvent(input$selected_language, {
    # Here is where we update language in session
    shiny.i18n::update_lang(session, input$selected_language)
  })

  ############### ReactiveValues #############
  # ReactiveValues to store the data
  # Store the data points (all and filtered)
  data_measurements <- reactiveValues()
  # Store the station locations and plotcolor etc
  data_stations <- reactiveValues()
  # Store other information
  data_other <- reactiveValues()
  # Store messages to communicate with user
  message_data <- reactiveValues()

  ########### Modules ################
  # The pickerInput for component selection
  select_component <- component_selection_server("select_component",
                                                 comp_choices)

  # select project/mun
  proj_or_mun_select <- project_or_mun_selection_server("proj_or_mun_select",
                                                        select_choices)

  # The dateRangeInput for date range selection
  select_date_range <- date_range_server("select_date_range",
                                         list(start_time = default_time$start_time,
                                              end_time = default_time$end_time))
  #communication_stuff$selected_time())

  # choose proj/mun
  choice_select <- choice_selection_server("choice_select",
                                           proj_or_mun_select = proj_or_mun_select,
                                           mun_choices = mun_choices,
                                           proj_choices = proj_choices)

  # Get metadata
  #meta_table <- metadata_server("meta_table", communication_stuff)

  # The Map
  map <- show_map_server("map", data_stations)

  # The bar plot
  barplot <- barplot_server("barplot_plot",
                            data_measurements = data_measurements,
                            data_stations = data_stations,
                            data_other = data_other,
                            overview_component,
                            theme_plots)

  # The timeseries plot
  #timeseries_plot <- timeseries_server(id = "timeseries_plot", com_module = communication_stuff, overview_component, theme_plots)

  # The pollutionrose plot
  #pollrose_plot <- pollrose_server("pollrose_plot", communication_stuff)

  # The timevariation plot
  #timevar_plot_weekly <- timevar_weekly_server("timevar_plot_weekly", communication_stuff)
  #timevar_plot_daily <- timevar_daily_server("timevar_plot_daily", communication_stuff)

  # The communication module
  communication_stuff <- communication_server("test_comm_output",
                                              data_measurements = data_measurements,
                                              data_stations = data_stations,
                                              meta, # TODO willen we hier wat mee?
                                              selected_parameter = select_component,
                                              selected_time = select_date_range,
                                              default_time = default_time,
                                              select_mun_or_proj = proj_or_mun_select,
                                              choose_mun_or_proj = choice_select,
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

  # Download data from external source to database
  download_api_button <- download_api_button_server("dl_btn_pushed",
                                                    proj_or_mun_select ,
                                                    choice_select,
                                                    select_date_range,
                                                    pool,
                                                    que)


  # To give some indication of the data available in dbs
  show_availability_server("show_availability",
                           data_to_show = data_measurements,
                           data_stations = data_stations,
                           time_period = select_date_range)

  single_text_server("text_data_available", text_message = reactive(message_data$data_in_dbs))

  ############ Observers ##############
  # Observe if the get_data_button is clicked ----
  observeEvent(input$get_data_button, {
    # Get the selected choice
    type_choice <- proj_or_mun_select()
    # Name of municipality/project
    name_choice <- choice_select()

    # Check if there is selected project/municipality
    shiny::validate(
      need(!is_empty((type_choice)),"Please, select gemeente of project"),
      need(!is_empty((name_choice)),"Please, select gemeente of project")
    )

    # Get the selected time period
    start_time <- select_date_range$selected_start_date()
    end_time <- select_date_range$selected_end_date()

    # Load the data from the caching database
    # Get the station names in the selected Municipality/project
    stations_name <- get_stations_from_selection(name_choice, type_choice, conn = pool)

    log_trace("{lubridate::now()} Get data from caching database ... ")

    # Get the data measurements of the selected Municipality/project in the period
    data_measurements$data_all <- get_measurements(measurements_con, stations_name, start_time, end_time)

    # Check if there is data in de caching, otherwise stop and give message
    if(nrow(data_measurements$data_all) == 0){
      message_data$data_in_dbs <- c(paste0("No data in ", type_choice, " ", name_choice))
      shiny::validate(need(F,message = paste0("No data in ", type_choice, " ", name_choice)))
    }
    message_data$data_in_dbs <- c(paste0("Data available in ", type_choice, " ", name_choice))

    # Create a pm10_kal and pm25_kl for reference stations
    data_measurements$data_all <- add_ref_kal(data_measurements$data_all)

    # Add uncertainty to the measurements of the sensors
    data_measurements$data_all <- add_uncertainty_sensor(data_measurements$data_all)

    # Get the information from the stations
    data_stations$data <- get_locations(stations_con, stations_name)

    # Take for each sensor 1 location and add the plot-colours etc.
    data_stations$data <- data_stations$data %>%
      dplyr::distinct(station, .keep_all = T) %>%
      # Add some specific info for the tool
      dplyr::mutate(selected = F, col = col_default, linetype = line_default, station_type = "sensor") %>%
      dplyr::mutate(station_type = ifelse(grepl("KNMI", station) == T, "KNMI", ifelse(grepl("NL", station) == T, "LML", station_type))) %>%
      dplyr::mutate(linetype = ifelse(station_type == "LML", line_overload, linetype),
                    size = ifelse(station_type == "LML", 2,1))

    log_trace("{lubridate::now()} Data available in tool. ")

  })


    # Observe filtered data from stations ----
    observe({
      data_filtered <- communication_stuff$selected_measurements()
      data_measurements$data_filtered <- data_filtered
    })

    # Observe filtered data from knmi ----
    observe({
      data_filtered_knmi <- communication_stuff$knmi_measurements()
      data_measurements$data_filtered_knmi <- data_filtered_knmi
    })

    # Observe if the station locations changes (colour) ----
    # TODO something with deselect from te map
    observe({
      data_stations_adjust <- communication_stuff$station_locations()
      data_stations$data <- data_stations_adjust
    })

    # Observe the parameter ----
    observe({
      shiny::validate(need(!is.null(select_component()), "No parameter yet."))
      parameter <- select_component()
      data_other$parameter <- parameter
    })

  ################# overig ##################
  #view_que_server("view_que", que)

})
