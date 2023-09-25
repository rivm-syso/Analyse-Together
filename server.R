# Define server logic                                                       ====
shinyServer(function(global, input, output, session) {

  # To change the language in the tool
  observeEvent(input$selected_language, {
    # Here is where we update language in session
    shiny.i18n::update_lang(session = session, language = input$selected_language)
  })

  # To keep the app activated in container
  output$currentTime <- renderText({

    shiny::invalidateLater(60000, session)
    format(Sys.time(), "%Y-%m-%d %H:%M")

  })

  ############### ReactiveValues #############
  # ReactiveValues to store the data
  # Store the data points (all and filtered)
  data_measurements <- reactiveValues()
  # Store the station locations and plotcolor etc
  data_stations <- reactiveValues()
  # Store other information
  data_other <- reactiveValues(group_name = group_name_default,
                               group_number = 1,
                               mun_or_proj = default_munproj,
                               name_munproj = default_munproj_name,
                               start_date = default_time[[1]],
                               end_date = default_time[[2]],
                               parameter = default_parameter,
                               cutoff = default_cutoff,
                               plot = default_plot,
                               col_select = default_col_select)
  # Store messages to communicate with user
  message_data <- reactiveValues()

  ########### Modules ################
  # The pickerInput for component selection ----
  select_component <- component_selection_server("select_component",
                                                 data_other = data_other,
                                                 comp_choices,
                                                 default_parameter = default_parameter)

  # The outlier cutoff value
  outlier_cutoff_server("select_cutoff",
                        data_other = data_other,
                        default_cutoff = default_cutoff
                        )

  # the selected plot for the visualisation
  plot_selection_server("select_plot",
                        plot_choices = plot_choices,
                        data_other = data_other,
                        default_plot = default_plot
  )

  # The dateRangeInput for date range selection ----
  select_date_range <- date_range_server("select_date_range",
                                         data_other = data_other,
                                         list(start_time = default_time$start_time,
                                              end_time = default_time$end_time))

  # select project/mun ----
  project_or_mun_selection_server("proj_or_mun_select",
                                  data_other = data_other,
                                  select_choices,
                                  pre_select = default_munproj)

  # choose proj/mun ----
  choice_selection_server("choice_select",
                         data_other = data_other,
                         # proj_or_mun_select = proj_or_mun_select,
                         mun_choices = mun_choices,
                         proj_choices = proj_choices,
                         pre_select = default_munproj_name)

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
                         data_stations = data_stations,
                         reactive(data_other$group_name),
                         reactive(data_other$tab_choice),
                         # Options for the colors
                         col_default,
                         col_select = reactive(data_other$col_select),
                         # Options for the linetype
                         line_cat,
                         line_default,
                         line_overload,
                         #Default group name
                         group_name_none
                         )

  # The plots ----
  show_plot_server("show_plot",
                   pick_plot = reactive(data_other$plot)
                   )

  # The map on the startpage ----
  show_map_no_server("map_start",
                     data_stations = reactive(data_stations$data))

  # Info about the sensor plot on start page ----
  info_sensor <- info_sensor_server("info_sensor",
                                    data_measurements = reactive(data_measurements$data_all))

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

  # The calender plot ----
  calender_plot <- calender_server("calender_plot",
                                   data_measurements =  reactive(data_measurements$data_grouped),
                                   data_measurements_knmi =  reactive(data_measurements$data_filtered_knmi),
                                   parameter = reactive(data_other$parameter),
                                   overview_component)

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
                                              selected_parameter = reactive(data_other$parameter),
                                              selected_start_date = reactive(data_other$start_date),
                                              selected_end_date = reactive(data_other$end_date),
                                              selected_cutoff = reactive(data_other$cutoff)
                                              )

  # Download data from external source to database ----
  download_api_button <- download_api_button_server("dl_btn_pushed",
                                                    proj_or_mun = reactive(data_other$mun_or_proj) ,
                                                    name_munproj = reactive(data_other$name_munproj),
                                                    selected_start_date = reactive(data_other$start_date),
                                                    selected_end_date = reactive(data_other$end_date),
                                                    pool = pool)

  # Get the data from the database ----
  get_data_cache_dbs_start <- get_data_cache_server("get_data_dbs_button_start",
                                            text_button = i18n$t({"title_start"}),
                                              data_measurements = data_measurements,
                                            data_stations = data_stations,
                                            message_data = message_data,
                                            mun_or_proj = reactive(data_other$mun_or_proj) ,
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

  get_data_cache_dbs_select <- get_data_cache_server("get_data_dbs_button_select",
                                              text_button = i18n$t({"btn_get_data"}),
                                              data_measurements = data_measurements,
                                              data_stations = data_stations,
                                              message_data = message_data,
                                              mun_or_proj = reactive(data_other$mun_or_proj) ,
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
                                                  data_stns = reactive(data_stations$data),
                                                  data_other = data_other,
                                                  col_names,
                                                  col_overload)

  single_text_server("name_group", reactive(data_other$group_name))

  single_text_server("text_data_available", text_message = reactive(message_data$data_in_dbs))
  single_text_server("text_download_estimation", text_message = reactive(message_data$download_estimation))
  single_text_server("text_check_visualisation", text_message = reactive("Please, check with the visualisations if all expected data is available."))



   ########### Observers ################
   # Observe if you change tab and store the tabname ----
    observeEvent(input$second_order_tabs,{
      data_other$tab_choice <- input$second_order_tabs
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

    # Observe if the station locations changes (colour) ----
    observe({
      data_stations_adjust <- map$data_stations()
      data_stations$data <- data_stations_adjust
    })


  # Observe to change tabs
  observeEvent(input$to_visualise_tab,{
    updateTabsetPanel(inputId = "second_order_tabs" , selected = "Visualise data")
  })
  observeEvent(input$to_select_tab,{
    updateTabsetPanel(inputId = "second_order_tabs" , selected = "Select data")
  })
  observeEvent(input$to_start_tab,{
    updateTabsetPanel(inputId = "second_order_tabs" , selected = "Start")
  })

  observeEvent(input$to_info_tab,{
    updateNavbarPage(inputId = "navbar", selected = "Information")
  })

  # Observe secret observer button
  observeEvent(input$browser, {
    browser()
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
