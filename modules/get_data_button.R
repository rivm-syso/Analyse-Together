###############################################
### Get data button ###
###############################################

# This is a module which get the data from the (cache)database,
# checks wheter or not the data is available and when not downloads
# it from the API (add a job in the queu)
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
                                  text_button,
                                  data_measurements,
                                  data_stations,
                                  message_data,
                                  mun_or_proj,
                                  name_munproj,
                                  selected_start_date,
                                  selected_end_date,
                                  pool,
                                  measurements_con,
                                  stations_con,
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

    output$get_dbs_cache <- renderUI({
      tagList(
        actionButton(ns("get_dbs_cache"),
                     text_button,
                     style="background-color: #ffe9b7")

      )
    })

    # Function to do the doenload or at least creates the job ----
    do_download_external <- function(mun_or_proj,
                                     name_munproj,
                                     selected_start_date,
                                     selected_end_date,
                                     pop_up_title,
                                     pop_up_message){
      type <- ifelse(is.null(mun_or_proj()), NA, mun_or_proj())
      name <- ifelse(is.null(name_munproj()), NA, name_munproj())
      tstart <- as_datetime(ifelse(is.null(selected_start_date()), NA, selected_start_date()))
      tend <- as_datetime(ifelse(is.null(selected_end_date()), NA, selected_end_date()))
      time_start <- selected_start_date() %>% as.POSIXct()
      time_end <- selected_end_date() %>%  as.POSIXct()
      log_info("mod download: Download pushed with paremeters Type: {type}; name: {name}; time_start: {tstart}; time_end: {tend}")


      if(any(is.na(c(type, name, tstart, tend)))) {
        log_trace("mod download: Download not started, missing parameters")
      } else {
        log_trace("mod download: create download queue")

        # Add message pop up to the user
        shinyalert(pop_up_title,
                   pop_up_message,
                   type = "warning",
                   confirmButtonCol = "#ffb612")

        # Download the metadata
        download_sensor_meta(name, type = type)

        # Get the overview of the sensors
        kits <- get_stations_from_selection(name, type = type)
        log_trace("mod download: Overview kits opgehaald")

        create_data_request(kits = kits,
                            time_start = time_start,
                            time_end = time_end,
                            conn = pool,
                            max_requests = 100)


      }
    }

    # Observe if the get_dbs_cache is clicked ----
    observeEvent(input$get_dbs_cache, {
      # Get the selected choice
      type_choice <- mun_or_proj()
      # Name of municipality/project
      name_choice <- name_munproj()

      # Check if there is selected project/municipality
      shiny::validate(
        need(!is_empty((type_choice)),"Please, select gemeente of project"),
        need(!is_empty((name_choice)),"Please, select gemeente of project")
      )

      # Get the selected time period
      start_time <- selected_start_date()
      end_time <- selected_end_date()

      # Check if there is selected start/end time
      shiny::validate(
        need(!is_empty((start_time)),"Please, select periode"),
        need(!is_empty((end_time)),"Please, select periode")
      )

      # Set up notification
      showNotification(i18n$t("expl_waiting_start"),
                       duration = NULL,
                       id = ns("waiting_start"),
                       closeButton = F
                       )

      # Load the data from the caching database
      # Get the station names in the selected Municipality/project
      stations_name <- get_stations_from_selection(name_choice,
                                                   type_choice,
                                                   conn = pool)

      log_info("get mod: Get data from caching database: {name_choice} ; {start_time} ; {end_time}... ")

      # Estimate the download time, if sensors exists in selection
      # If there are no sensors in the database known (yet)
      if(is.null(stations_name)){
        log_trace("get mod: download to queu ...")

        message_data$download_estimation <- c(paste0("No information available yet, please press the get data button (right button)."))

        # remove notification
        removeNotification(id = ns("waiting_start"))

        # Start the download and show pop up message
        # Do the download functionalities from external source
        do_download_external(mun_or_proj,
                             name_munproj,
                             selected_start_date,
                             selected_end_date,
                             pop_up_title,
                             pop_up_message)

        log_trace("get mod: download added to queu")

      }else{ # There are sensors known
        # Check if there are measurements for the choosen period
        timeranges_to_download <- sapply(stations_name, function(x){
          ATdatabase::get_download_ranges(x,
                                          selected_start_date() |> as.POSIXct() ,
                                          selected_end_date()|> as.POSIXct() ,
                                          pool)
          })
        timeranges_to_download <- unlist(timeranges_to_download)

        # Not all data for the choosen period is in the dbs, So download needed
        if(T %in% (timeranges_to_download > 0) ){
          log_trace("get mod: download timeranges ...")

          # if there is missing timeranges, then estimate time and do_download_external
          # Shinyalert for estimated time.
          estimate_time <- ceiling((length(stations_name) * 7 + 30)/60)
          message_data$download_estimation <- c(paste0("Estimated load time from external source: ", estimate_time, " minutes."))

          pop_up_message <- paste0(pop_up_message, " Geschatte download tijd: ",
                                   estimate_time, ' minuten.')

          # remove notification
          removeNotification(id = ns("waiting_start"))

          # Start the download and show pop up message
          do_download_external(mun_or_proj,
                               name_munproj,
                               selected_start_date,
                               selected_end_date,
                               pop_up_title,
                               pop_up_message)

          log_trace("get mod: timeranges done")

      }else{ # The data is available in the dbs, So load this in the tool
        # Get the data measurements of the selected Municipality/project in the period
        # and do some data cleaning
        log_info("get mod: adding data to the tool ...")

        data_measurements$data_all <- get_measurements_cleaned(measurements_con,
                                                               stations_name,
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

        # remove notification
        removeNotification(id = ns("waiting_start"))

        # Check if there is data in de caching, otherwise stop and give message
        if(nrow(data_measurements$data_all) == 0){
          message_data$data_in_dbs <- c(paste0("No data in ", type_choice, " ", name_choice))
          shiny::validate(need(F,message = paste0("No data in ", type_choice, " ", name_choice)))
        }

        message_data$data_in_dbs <- c(paste0("Data available in ", type_choice, " ", name_choice))

        # counter to trigger event to switch to Start-page
        message_data$to_start_page <-  message_data$to_start_page + 1

        log_info("get mod: Data available in tool. ")
      }
      }

  })
  })
}
