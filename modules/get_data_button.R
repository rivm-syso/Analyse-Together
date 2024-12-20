###############################################
### Get data button ###
###############################################

# This is a module which get the data from the (cache)database,
# checks if the data is available and gives the user the option to
# download it from the API (add a job in the queu)
######################################################################
# Output Module
######################################################################

get_data_cache_output <- function(id) {

  ns <- NS(id)
  uiOutput(ns("get_dbs_cache"))

}

######################################################################
# Server Module
######################################################################

get_data_cache_server <- function(id,
                                  data_measurements,
                                  data_stations,
                                  message_data,
                                  mun_or_proj,
                                  name_munproj,
                                  selected_parameter,
                                  selected_start_date,
                                  selected_end_date,
                                  pool,
                                  measurements_con,
                                  stations_con,
                                  data_other,
                                  # Options for the colors
                                  col_default,
                                  # Options for the linetype
                                  line_default,
                                  line_overload,
                                  # DEfault group name
                                  group_name_none,
                                  #text for the popup
                                  pop_up_title = "Warning",
                                  pop_up_message = "Please wait a bit."
) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # Ui output with action button and other modules ----
    output$get_dbs_cache <- renderUI({
      tagList(
        actionButton(ns("get_dbs_cache"),
                     i18n$t("btn_get_data"),
                     style="background-color: #ffe9b7",
                     icon = icon("square-check")),
        fluidRow(
          column(info_sensor_output(ns("data_availability_calender_temp")), width = 8),
          column(show_map_no_output(ns("data_available_map")), width = 4),
        ),
        textOutput(ns("data_avail_or_not")),
        uiOutput(ns("btn_do_data"))
      )
    })

    # Initiate status ----
    # Create overview from the input status, which can be used in the
    # different steps
    initiate_status <- reactive({

      # Get the selected choice
      type_choice <- mun_or_proj()
      # Name of municipality/project
      name_choice <- name_munproj()

      # Check if there is selected project/municipality
      shiny::validate(
        need(!is_empty((type_choice)),"Please, select gemeente of project"),
        need(!is_empty((name_choice)),"Please, select gemeente of project")
      )

      # If the jobs are done, then this is changing, so excecute initiate_values
      waiting_number <- data_other$waiting_number

      # Get the selected time period
      start_time <- selected_start_date()
      end_time <- selected_end_date()
      end_time_plot <- end_time - 1

      # Check if there is selected start/end time
      shiny::validate(
        need(!is_empty((start_time)),"Please, select periode"),
        need(!is_empty((end_time)),"Please, select periode")
      )

      # Load the data from the caching database
      # Get the station names in the selected Municipality/project
      stations_name <- get_stations_from_selection(name_choice,
                                                   type_choice,
                                                   conn = pool)

      return(list(type_choice = type_choice,
                  name_choice = name_choice,
                  start_time = start_time,
                  end_time = end_time,
                  stations_name = stations_name
                  ))

    })


    # Load from cache  ----
    # Function to get the data from the cache dbs in the tool
    load_from_cache_temp <- function(measurements_con,
                                stations_name,
                                start_time,
                                end_time,
                                stations_con,
                                data_measurements,
                                col_default,
                                line_default,
                                groep_name_none,
                                line_overload){

      # Get the data measurements of the selected Municipality/project in
      # the period and do some data cleaning
      data_measurements$temp_data_all <-
        get_measurements_cleaned(measurements_con,
                                 stations_name,
                                 parameter_input = selected_parameter(),
                                 start_time,
                                 end_time)


      # Get the data of the stations and put colours etc to it
      data_stations_list <- get_stations_cleaned(stations_con,
                                                 stations_name,
                                                 data_measurements$temp_data_all,
                                                 col_default,
                                                 line_default,
                                                 group_name_none,
                                                 line_overload)

      # Put the station data in the reactivevalues
      data_stations$temp_data <- data_stations_list$data
      data_stations$temp_data_all <- data_stations_list$data_all

      # Set the start end period as use for selection
      data_other$start_date <- start_time
      data_other$end_date <- end_time

      log_info("mod get_data_button: Data temp available in tool. ")

    }
    # Load from cache  ----
    # Function to get the data from the cache dbs in the tool
    load_from_cache <- function(measurements_con,
                                stations_name,
                                start_time,
                                end_time,
                                stations_con,
                                data_measurements,
                                col_default,
                                line_default,
                                groep_name_none,
                                line_overload){

      # Set up notification
      showNotification(i18n$t("expl_waiting_start"),
                       duration = NULL,
                       id = ns("waiting_start"),
                       closeButton = F
      )

      # Get the data measurements of the selected Municipality/project in
      # the period and do some data cleaning
      data_measurements$data_all <- get_measurements_cleaned(measurements_con,
                                                             stations_name,
                                                             parameter_input = selected_parameter(),
                                                             start_time,
                                                             end_time)


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

      # Set the start end period as use for selection
      data_other$start_date <- start_time
      data_other$end_date <- end_time

      # remove notification
      removeNotification(id = ns("waiting_start"))

      log_info("mod get_data_button: Data available in tool. ")

    }

    # do_download_external ----
    # Function to do the download or at least creates the job
    do_download_external <- function(mun_or_proj,
                                     name_munproj,
                                     selected_start_date,
                                     selected_end_date,
                                     pop_up_title,
                                     pop_up_message){
      # Do some checks to the data
      type <- ifelse(is.null(mun_or_proj), NA, mun_or_proj)
      name <- ifelse(is.null(name_munproj), NA, name_munproj)
      tstart <- as_datetime(ifelse(is.null(selected_start_date), NA,
                                   selected_start_date))
      tend <- as_datetime(ifelse(is.null(selected_end_date), NA,
                                 selected_end_date))
      time_start <- selected_start_date %>% as.POSIXct()
      time_end <- selected_end_date %>%  as.POSIXct()
      log_info("mod get button data actions: Download pushed with paremeters Type:
               {type}; name: {name}; time_start: {tstart}; time_end: {tend}")

      if(any(is.na(c(type, name, tstart, tend)))) {
        log_trace("mod get button data actions: Download not started,
                  missing parameters")
      } else {
        log_trace("mod get button data actions: create download queue")

        # Add message pop up to the user
        shinyalert(pop_up_title,
                   pop_up_message,
                   type = "warning",
                   confirmButtonCol = "#ffb612")

        # Download the metadata
        download_sensor_meta(name, type = type)

        # Get the overview of the sensors
        kits <- get_stations_from_selection(name, type = type)
        log_trace("mod get button data actions: Overview kits opgehaald")

        data_reqs <- create_data_request(kits = kits,
                            time_start = time_start,
                            time_end = time_end,
                            conn = pool,
                            max_requests = 100)

        # get the meta data of the cache dbs
        meta_table <- tbl(pool, "meta") %>%
          as.data.frame()

        # overview data requests in queu, including waitng number
        data_reqs_queu <- meta_table %>%
          dplyr::filter(type == "data_req") %>%
          dplyr::select(-c(doc)) %>%
          dplyr::mutate(waiting = row_number())

        # Get job id of interest
        job_id_interest <- tail(data_reqs,1) %>% unlist()
        # number of requests above the one of interest, waiting
        waiting_number <- data_reqs_queu %>%
          dplyr::filter(str_detect(ref, job_id_interest)) %>%
          dplyr::select(waiting) %>%
          pull()

        # Take the last one (multiple lines if >100 stations in job)
        waiting_number <- tail(waiting_number,1)

        #Store job_id
        data_other$job_id <- job_id_interest

        # Store the waiting number
        data_other$waiting_number <- waiting_number

        log_info("mod get button data actions: download request made {job_id_interest},
                  waiting {waiting_number}")

      }
    }


    # Observe if button for use this data is clicked ----
    observeEvent(input$btn_use_data,{

      log_trace("mod get data button: use this button load cache data")

      # Check the input values, if available and get some more info
      input_values <- initiate_status()

      # If no data at all
      if(purrr::is_empty(input_values$stations_name)){
        log_trace("mod show avail: Empty stations_name. No data available in cache ")

      }else{
        log_info("get buttons data actions mod: set temp data to product data")

        data_measurements$data_all <- data_measurements$temp_data_all
        data_stations$data <- data_stations$temp_data
        data_stations$data_all <- data_stations$temp_data_all

        # counter to trigger event to switch to Start-page
        data_other$to_explore_page <-  data_other$to_explore_page + 1
        log_trace("mod get data button:
                  data_other$to_explore_page {data_other$to_explore_page}")
      }

    })

    # Observe if button for external download ----
    observeEvent(input$btn_external_data,{
      # Check the input values, if available and get some more info
      input_values <- isolate(initiate_status())
      log_trace("mod get data button: external data button ... ")

      # Download always from the beginning of the month
      start_time_all <- lubridate::floor_date(input_values$start_time,
                                              unit = "month")
      end_time_all <- lubridate::ceiling_date(input_values$end_time,
                                              unit = "month")

      # Do the download functionalities from external source
      do_download_external(input_values$type_choice,
                           input_values$name_choice,
                           start_time_all,
                           end_time_all,
                           pop_up_title,
                           pop_up_message)

    })

    # Observe if the get_dbs_cache is clicked ----
    observeEvent(input$get_dbs_cache, {

      # Give user feedback
      # Set up notification
      showNotification(i18n$t("expl_waiting_check_cache"),
                       duration = NULL,
                       id = ns("waiting_check_cache"),
                       closeButton = F
      )

      # Check the input values, if available and get some more info
      input_values <- initiate_status()

      log_trace("mod get_data_button; observeEvent get_dbs_cache;
                input {input_values} ")

      # Create visualisation of the available data in cache
      # If no data at all
      if(purrr::is_empty(input_values$stations_name)){
        log_trace("mod show avail: Empty stations_name.
                  No data available in cache ")

        # message to the user
        output$data_avail_or_not <- renderText({
          i18n$t("expl_nodatayet")
        })

        # Create empty data_frame to plot
        data_in_tool_empty <- setNames(data.frame(matrix(ncol = 8, nrow = 0)),
                                       c("station", "parameter", "value",
                                         "aggregation", "timestamp",
                                         "date", "sd", "diff"))

        # Plot empty data, plot will give proper message
        info_sensor_server("data_availability_calender_temp",
                           data_measurements = reactive({data_in_tool_empty}))

        # Create empty stations
        stations_empty <- setNames(data.frame(matrix(ncol = 11, nrow = 0)),
                                   c("station", "lat", "lon",
                                     "selected", "col",
                                     "linetype", "station_type", "group_name",
                                     "label", "stroke", "size"))

        # Add empty map
        show_map_no_server("data_available_map",
                           data_stations = reactive(stations_empty))

      }else{ # If there is data at least of this municipality/project
        log_trace("mod get data btn: load from cache temp ...")

        log_info("mod get data btn: Get data from caching database:
        {input_values$name_choice} ; {input_values$start_time} ; {
                 input_values$end_time}... ")

        # Get the data from the cache and store it in temp reactivevalue
        load_from_cache_temp(measurements_con = measurements_con,
                            stations_name = input_values$stations_name,
                            start_time = input_values$start_time,
                            end_time = input_values$end_time,
                            stations_con = stations_con,
                            data_measurements = data_measurements,
                            col_default = col_default,
                            line_default = line_default,
                            groep_name_none = groep_name_none,
                            line_overload = line_overload)

        # show which data is available, in calender
        info_sensor_server("data_availability_calender_temp",
                           data_measurements =
                             reactive({data_measurements$temp_data_all}))

        # And in a map show the data available
        show_map_no_server("data_available_map",
                           data_stations = reactive(data_stations$temp_data))

        # message to the user
        output$data_avail_or_not <- renderText({
          i18n$t("expl_check_expected")
        })
      }

      # Add actionButtons to load the data
      output$btn_do_data <- renderUI({
          tagList(
            actionButton(ns("btn_use_data"),
                         label = i18n$t("btn_use_data"),
                         icon = icon("play")),
            actionButton(ns("btn_external_data"),
                         label = i18n$t("btn_external_data"),
                         icon = icon("hourglass-start"))
          )
        })

      # remove notification
      removeNotification(id = ns("waiting_check_cache"))

  })
  })
}
