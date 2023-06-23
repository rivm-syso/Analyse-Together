# Define server logic                                                       ====
shinyServer(function(global, input, output, session) {

  # To change the language in the tool
  observeEvent(input$selected_language, {
    # Here is where we update language in session
    shiny.i18n::update_lang(session = session, language = input$selected_language)
  })

  ############### ReactiveValues #############
  # ReactiveValues to store the data
  # Store the data points (all and filtered)
  data_measurements <- reactiveValues()
  # Store the station locations and plotcolor etc
  data_stations <- reactiveValues()
  # Store other information
  data_other <- reactiveValues(group_name = group_name_default, group_number = 1)
  # Store messages to communicate with user
  message_data <- reactiveValues()

  ########### Modules ################
  # The pickerInput for component selection ----
  select_component <- component_selection_server("select_component",
                                                 comp_choices)

  # select project/mun ----
  proj_or_mun_select <- project_or_mun_selection_server("proj_or_mun_select",
                                                        select_choices)

  # The dateRangeInput for date range selection ----
  select_date_range <- date_range_server("select_date_range",
                                         list(start_time = default_time$start_time,
                                              end_time = default_time$end_time))

  # choose proj/mun ----
  choice_select <- choice_selection_server("choice_select",
                                           proj_or_mun_select = proj_or_mun_select,
                                           mun_choices = mun_choices,
                                           proj_choices = proj_choices)

  # Get metadata ----
  meta_param_table <- metadata_param_server("meta_param_table",
                                data_measurements = reactive(data_measurements$data_all),
                                data_stations = reactive(data_stations$data),
                                parameter = reactive(data_other$parameter),
                                selected_start_date = reactive(data_other$start_date),
                                selected_end_date = reactive(data_other$end_date),
                                name_munproj = reactive(data_other$name_munproj))

  # The Map ----
  map <- show_map_server("map",
                         data_stations,
                         reactive(data_other$group_name),
                         reactive(data_other$tab_choice),
                         # Options for the colors
                         col_cat,
                         col_default,
                         col_overload,
                         # Options for the linetype
                         line_cat,
                         line_default,
                         line_overload,
                         #Default group name
                         group_name_none
                         )

  # The bar plot ----
  barplot <- barplot_server("barplot_plot",
                            data_measurements = reactive(data_measurements$data_grouped),
                            parameter = reactive(data_other$parameter),
                            overview_component,
                            theme_plots)

  # The timeseries plot ----
  timeseries_plot <- timeseries_server(id = "timeseries_plot",
                                       data_measurements = reactive(data_measurements$data_grouped),
                                       parameter = reactive(data_other$parameter),
                                       overview_component,
                                       theme_plots)
  # The pollutionrose plot ----
  pollrose_plot <- pollrose_server("pollrose_plot",
                                   data_measurements =  reactive(data_measurements$data_grouped),
                                   data_measurements_knmi =  reactive(data_measurements$data_filtered_knmi),
                                   parameter = reactive(data_other$parameter),
                                   overview_component)

  # The timevariation plot ----
  timevar_plot_weekly <- timevar_weekly_server("timevar_plot_weekly",
                                               data_measurements = reactive(data_measurements$data_grouped),
                                               parameter = reactive(data_other$parameter),
                                               overview_component)
  timevar_plot_daily <- timevar_daily_server("timevar_plot_daily",
                                             data_measurements = reactive(data_measurements$data_grouped),
                                             parameter = reactive(data_other$parameter),
                                             overview_component)

  # Individual timeseries plot ----
  indu_timeseries <- individual_timeseries_server("indu_timeseries",
                                                  data_measurements = reactive(data_measurements$data_filtered),
                                                  parameter = reactive(data_other$parameter),
                                                  overview_component = overview_component,
                                                  theme_plots)

  # The communication module ----
  communication_stuff <- communication_server("test_comm_output",
                                              data_measurements = reactive(data_measurements$data_all),
                                              data_stations = reactive(data_stations$data),
                                              meta, # TODO willen we hier wat mee?
                                              selected_parameter = select_component,
                                              selected_start_date = reactive(data_other$start_date),
                                              selected_end_date = reactive(data_other$end_date)
                                              )

  # Download data from external source to database ----
  download_api_button <- download_api_button_server("dl_btn_pushed",
                                                    proj_or_mun = reactive(data_other$proj_or_mun) ,
                                                    name_munproj = reactive(data_other$name_munproj),
                                                    selected_start_date = reactive(data_other$start_date),
                                                    selected_end_date = reactive(data_other$end_date),
                                                    pool = pool)

  # Get the data from the database ----
  get_data_button <- get_data_button_server("get_btn_pushed",
                                            data_measurements = data_measurements,
                                            data_stations = data_stations,
                                            message_data = message_data,
                                            proj_or_mun = reactive(data_other$proj_or_mun) ,
                                            name_munproj = reactive(data_other$name_munproj),
                                            selected_start_date = reactive(data_other$start_date),
                                            selected_end_date = reactive(data_other$end_date),
                                            pool = pool,
                                            measurements_con = measurements_con,
                                            stations_con = stations_con,
                                            # Options for the colors
                                            col_cat,
                                            col_default,
                                            col_overload,
                                            # Options for the linetype
                                            line_cat,
                                            line_default,
                                            line_overload,
                                            # Default group name
                                            group_name_none
                                            )

  # Download to pc user ----
  download_pc_button_server("download_pc",
                            data_measurements = reactive(data_measurements$data_all),
                            data_stations = reactive(data_stations$data_all),
                            name_munproj = reactive(data_other$name_munproj),
                            selected_start_date = reactive(data_other$start_date),
                            selected_end_date = reactive(data_other$end_date))

  # Create a new group ----
  set_new_group_button <- set_group_button_server("set_group_pushed",
                                                  data_other = data_other)

  single_text_server("name_group", reactive(data_other$group_name))

  single_text_server("text_data_available", text_message = reactive(message_data$data_in_dbs))
  single_text_server("text_download_estimation", text_message = reactive(message_data$download_estimation))
  single_text_server("text_check_visualisation", text_message = reactive("Please, check with the visualisations if all expected data is available."))



   ########### Observers ################
   # Observe if you change tab and store the tabname ----
    observeEvent(input$second_order_tabs,{
      data_other$tab_choice <- input$second_order_tabs
    })

    # Observe get data button is pushed ----
    observe({
      # Get the measurements and store them
      data_measumerements_new <- get_data_button$data_measurements()
      data_measurements$data_all <- data_measumerements_new

      # Get the station info - all stations - and store them
      data_stations_new <- get_data_button$data_stations_all()
      data_stations$data_all <- data_stations_new

      # Get the station info - stations with data - and store them
      data_stations_filtered_new <- get_data_button$data_stations_filtered()
      data_stations$data <- data_stations_filtered_new

    })

    # Observe filtered data from stations and groups ----
    observe({
      data_filtered <- communication_stuff$selected_measurements()
      data_measurements$data_filtered <- data_filtered

      data_grouped <- communication_stuff$grouped_measurements()
      data_measurements$data_grouped <- data_grouped
    })

    # Observe filtered data from knmi ----
    observe({
      data_filtered_knmi <- communication_stuff$knmi_measurements()
      data_measurements$data_filtered_knmi <- data_filtered_knmi
    })

    # Observe the parameter ----
    observe({
      shiny::validate(need(!is.null(select_component()), "No parameter yet."))
      parameter <- select_component()
      data_other$parameter <- parameter
    })

    # Observe the time period ----
    observe({
      start_date_set <- select_date_range$selected_start_date()
      end_date_set <- select_date_range$selected_end_date()
      data_other$start_date <- lubridate::as_datetime(start_date_set, tz = "Europe/Amsterdam")
      data_other$end_date <- lubridate::as_datetime(end_date_set, tz = "Europe/Amsterdam")
    })

    # Observe the selection municipality OR project ----
    observe({
      shiny::validate(need(!is.null(proj_or_mun_select()), "No municipality/project yet."))
      proj_mun_or <- proj_or_mun_select()
      data_other$proj_or_mun <- proj_mun_or
    })

    # Observe the municipality/project name ----
    observe({
      shiny::validate(need(!is.null(choice_select()), "No municipality/project yet."))
      name_munproj <- choice_select()
      data_other$name_munproj <- name_munproj
    })

    # Observe if the station locations changes (colour) ----
    observe({
      data_stations_adjust <- map$data_stations()
      data_stations$data <- data_stations_adjust
    })

  # Observe if new group is created ----
  observe({
    new_group_name <- set_new_group_button$group_name()
    new_group_number <- set_new_group_button$group_number()

    data_other$group_name <- new_group_name
    data_other$group_number <- new_group_number
  })


  ################# overig ##################
  #view_que_server("view_que", que)
  # keep queue running
  a <- observe({
      invalidateLater(3e3, NULL) # 10 seconds
      log_trace("server: poll queue")
      que$poll()

  }, suspended = TRUE)

})
