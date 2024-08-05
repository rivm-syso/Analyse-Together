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
  # Store other information
  data_other <- reactiveValues(group_name = default_group_name,
                               group_number = 1,
                               mun_or_proj = default_munproj,
                               name_munproj = default_munproj_name,
                               start_date = default_time[[1]],
                               end_date = default_time[[2]],
                               parameter = default_parameter,
                               cutoff = default_cutoff,
                               plot = default_plot,
                               col_select = default_col_select,
                               combi_col_name = setNames(default_col_select,
                                                         default_group_name),
                               indu_station_index = 1,
                               missing_days = 0,
                               to_start_page = 0)

  # Store the data points (all and filtered)
  data_measurements <- reactiveValues(data_all = measurements_all)
  # Store the station locations and plotcolor etc
  data_stations <- reactiveValues(data_all = data_stations_list$data_all,
                                  data = data_stations_list$data)
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
                        default_cutoff = data_other$cutoff
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
                         reactive(data_other$tab_choice_figures),
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

   # The map on the show page ----
  show_map_no_select_server("map_no_select_step3",
                            data_stations = reactive(data_stations$data),
                            change_tab = reactive(data_other$change_tab_figures))
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
                                       theme_plots,
                                       zoom_in = list(
                                         start_slider_zoom = reactive(data_other$start_slider_zoom),
                                         end_slider_zoom = reactive(data_other$end_slider_zoom))
                                       )

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
  indu_timeseries <- individual_timeseries_map_server("indu_timeseries",
                                                      data_stations = data_stations,
                                                      data_measurements = reactive(data_measurements$data_filtered),
                                                      data_measurements_all = reactive(data_measurements$data_all),
                                                      parameter = reactive(data_other$parameter),
                                                      overview_component = overview_component,
                                                      theme_plots,
                                                      change_tab = reactive(data_other$change_tab_figures),
                                                      data_other = data_other,
                                                      default_group_name,
                                                      col_default,
                                                      line_default)

  # Overview timeseries plot ----
  overview_timeseries_server("overview_timeseries",
                             data_stations = reactive(data_stations$data),
                             data_measurements_all = reactive(data_measurements$data_all),
                             parameter = reactive(data_other$parameter),
                             selected_cutoff = reactive(data_other$cutoff),
                             overview_component = overview_component,
                             theme_plots,
                             change_tab = reactive(data_other$change_tab_figures)   )

  # Slider zoom for on the timeseries
  slider_zoom_server("slider_zoom",
                     data_other = data_other,
                     min_date = reactive(data_other$start_date),
                     max_date = reactive(data_other$end_date))

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
                                                    pool = pool,
                                                    pop_up_title = i18n$t("word_patient"),
                                                    pop_up_message = i18n$t("infotext_patient"))

  # Get the data from the database ----
  get_data_cache_dbs_start <- get_data_cache_server("get_data_dbs_button_start",
                                            data_measurements = data_measurements,
                                            data_stations = data_stations,
                                            message_data = message_data,
                                            mun_or_proj = reactive(data_other$mun_or_proj) ,
                                            name_munproj = reactive(data_other$name_munproj),
                                            selected_parameter = reactive(data_other$parameter),
                                            selected_start_date = reactive(data_other$start_date),
                                            selected_end_date = reactive(data_other$end_date),
                                            pool = pool,
                                            measurements_con = measurements_con,
                                            stations_con = stations_con,
                                            data_other = data_other,
                                            # Options for the colors
                                            col_default,
                                            # Options for the linetype
                                            line_default,
                                            line_overload,
                                            # Default group name
                                            group_name_none,
                                            pop_up_title = i18n$t("word_helaas"),
                                            pop_up_message = i18n$t("infotext_patient")
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
  # Switch group ----
  switch_group_server("switch_group",
                      data_other = data_other)

  # Rename group ----
  rename_group_button_server("rename_group",
                             data_stations = data_stations,
                             data_other = data_other)

  # Single text items
  single_text_server("text_data_available",
                     text_message = reactive(message_data$data_in_dbs))
  single_text_server("text_download_estimation",
                     text_message = reactive(message_data$download_estimation))
  single_text_server("text_check_visualisation",
                     text_message = reactive("Please, check with the visualisations if all expected data is available."))
  single_text_server("text_selected_sensors",
                     text_message = reactive(message_data$selected_sensors))

  # Pop up information
  info_button_server("text_overview",
                     i18n$t("infotext_overview_expl"))
  info_button_server("text_step1",
                     i18n$t("infotext_step1_select_expl"))
  info_button_server("text_step2",
                     i18n$t("expl_indu_timeplot_expl"))
  info_button_server("plot_timeseries",
                     i18n$t("expl_timeplot_expl"))

  info_button_server("plot_barplot",
                     i18n$t("expl_barplot_expl"))

  info_button_server("plot_weekly",
                     i18n$t("expl_overviewplot_expl_weekly"))

  info_button_server("plot_daily",
                     i18n$t("expl_overviewplot_expl_daily"))


  info_button_server("plot_windcal",
                     i18n$t("expl_calplot_expl"))

  info_button_server("plot_conc_rose",
                     i18n$t("expl_concplot%_expl"))

  info_button_server("plot_table",
                     i18n$t("expl_meta_table_expl"))

   ########### Observers ################
   # Observe if you change tab and store the tabname ----
    observeEvent(input$second_order_tabs,{
      data_other$tab_choice <- input$second_order_tabs
    })
  # Observe if you change tab and store the tabname ----
  observeEvent(input$tab_figures,{
    data_other$tab_choice_figures <- input$tab_figures
  })
  # Observe if you change tab (visualise/plaatjes section) and store the tabname ----
  observeEvent(input$tab_figures,{
    data_other$change_tab_figures <- input$tab_figures
  })

  # Observe filtered data from stations and groups ----
  observe({
    data_filtered <- communication_stuff$selected_measurements()
    data_measurements$data_filtered <- data_filtered

    data_grouped <- communication_stuff$grouped_measurements()
    data_measurements$data_grouped <- data_grouped

    message_data$selected_sensors <- communication_stuff$message_selected()
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

  # Observe if there is new data selected from the caching, then move to the
  # start-page
  observeEvent(data_other$to_start_page, {
               updateTabsetPanel(inputId = "second_order_tabs" , selected = "Start")
               # If you loaded new data, get the cut off value based on this new data
               data_other$cutoff <- isolate(communication_stuff$cut_off_value())
               })

  # Observe to change tabs
  observeEvent(input$to_visualise_tab,{
    updateTabsetPanel(inputId = "second_order_tabs" ,
                      selected = "Visualise data")
  })
  observeEvent(input$to_select_tab,{
    updateTabsetPanel(inputId = "second_order_tabs" ,
                      selected = "Select data")
  })

  # Observe change of tabs reset to overview
  observeEvent(input$second_order_tabs == "Start",{
    updateTabsetPanel(inputId = "tab_figures" ,
                      selected = "Overview")
  })

  # Observe if user want to go to information tab
  observeEvent(input$link_to_information, {
    updateNavbarPage(inputId = "navbar" ,
                      selected = "Information")
  })

  # Observe User input from URL to start with other data set,
  # If no input is given, start/load the default data set
  observe({
    user_input_url <- parseQueryString(session$clientData$url_search)

    browser()
    # Check if there is a valid query
    if(purrr::is_empty(user_input_url)){
      # Get the default values
      name_choice <- default_munproj_name
      type_choice <- default_munproj
      start_time <- default_time$start_time
      end_time <- default_time$end_time
      parameter_choice <- default_parameter
    }else{

      # Check if all input is valid, otherwise take default values
      type_choice <- ifelse(is.null(user_input_url$proj_mun),
                            default_munproj,
                            user_input_url$proj_mun)

      name_choice <- ifelse(is.null(user_input_url$name),
                            default_munproj_name,
                            user_input_url$name)

      parameter_choice <- ifelse(is.null(user_input_url$parameter),
                            default_parameter,
                            user_input_url$parameter)

      if(is.null(user_input_url$start_date)){
        start_time <- default_time$start_time
      }else{
        start_time <- lubridate::ymd(user_input_url$start_date)
      }

      if(is.null(user_input_url$end_date)){
        end_time <- default_time$end_time
      }else{
        end_time <- lubridate::ymd(user_input_url$end_date)
      }

    }

    # Get the station names in the selected Municipality/project
    stations_name <- get_stations_from_selection(name_choice,
                                                 type_choice,
                                                 conn = pool)

    # Check if there are stations in cache dbs known,
    # then the default data is shown
    shiny::validate(need(!purrr::is_null(stations_name),"No stations found,
                         please use the 'Choose' option from the default tool
                         to load the data."))

    # Get the data measurements of the selected Municipality/project in
    # the period and do some data cleaning
    data_measurements$data_all <- get_measurements_cleaned(measurements_con = measurements_con,
                                                           stations_name = stations_name,
                                                           parameter_input = parameter_choice,
                                                           start_time = start_time,
                                                           end_time = end_time)

    # Get the data of the stations and put colours etc to it
    data_stations_list <- get_stations_cleaned(stations_con,
                                               stations_name,
                                               data_measurements$data_all,
                                               col_default,
                                               line_default,
                                               group_name_none,
                                               line_overload)

    # Put the station data in the reactivevalues
    data_stations$data <- data_stations_list$data
    data_stations$data_all <- data_stations_list$data_all

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
