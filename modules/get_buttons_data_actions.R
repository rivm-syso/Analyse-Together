###############################################
### Buttons to choose get data actions ###
###############################################

# This is a module which create buttons to use or download the data
# and do those actions too
######################################################################
# Output Module
######################################################################

btn_do_data_actions_output <- function(id) {

  ns <- NS(id)
  uiOutput(ns("btn_do_data"))

}

######################################################################
# Server Module
######################################################################

btn_do_data_actions_server <- function(id,
                                       missing_days, # list
                                       measurements_con,
                                       data_measurements,
                                       data_stations,
                                       stations_con,
                                       stations_name,
                                       start_time,
                                       end_time,
                                       mun_or_proj,
                                       name_munproj,
                                       col_default,
                                       line_default,
                                       group_name_none,
                                       line_overload,
                                       pop_up_title,
                                       pop_up_message
) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    observeEvent(missing_days,{

      # Create buttons to choose which data actions to do: use the data available
      # or download the external data
        output$btn_do_data <- renderUI({

        if(missing_days$create_btn_get_data
                   & missing_days$create_btn_use_data){
            tagList(
              actionButton(ns("btn_use_data"),
                              label = "Gebruik beschikbare data"),
              actionButton(ns("btn_external_data"),
                              label = "Ophalen van missende data")
              )
          }else if(missing_days$create_btn_get_data){
            tagList(
              actionButton(ns("btn_external_data"),
                           label = "Ophalen van missende data")
            )
          }else if(missing_days$create_btn_use_data){
            tagList(
              actionButton(ns("btn_use_data"),
                              label = "Gebruik beschikbare data")
            )
          }
        })

    })

    # Function to get the data from the cache dbs in the tool ----
    load_from_cache <- function(){
      log_info("get buttons data actions mod: Get data from caching database:
      {name_munproj} ; {start_time} ; {end_time}... ")

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

      log_info("get buttons data actions mod: Data available in tool. ")

    }

    # Function to do the download or at least creates the job ----
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

        #     # if there is missing timeranges, then estimate time and
        #     # do_download_external
        #     # Shinyalert for estimated time.
        #     estimate_time <- ceiling((length(stations_name) * 7 + 30)/60)
        #     message_data$download_estimation <- c(paste0("Estimated load time
        #                                                  from external source: ",
        #                                                  estimate_time, " minutes."))
        #
        #     pop_up_message <- paste0(pop_up_message, " Geschatte download tijd: ",
        #                              estimate_time, ' minuten.')

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

        create_data_request(kits = kits,
                            time_start = time_start,
                            time_end = time_end,
                            conn = pool,
                            max_requests = 100)

        log_trace("mod get button data actions: download request made")

      }
    }


    # Observe if button for use this data is clicked ----
    observeEvent(input$btn_use_data,{
            log_trace("mod get data button: use this button load cache data")
      load_from_cache()

      # TODO add observer to go back to start page

    })

    # Observe if button for external download ----
    observeEvent(input$btn_external_data,{
      log_trace("mod get data button: external data button ... ")
      # Download always from the beginning of the month
      start_time_all <- lubridate::floor_date(start_time,
                                              unit = "month")
      end_time_all <- lubridate::ceiling_date(end_time,
                                              unit = "month")
      # Do the download functionalities from external source
      do_download_external(mun_or_proj,
                           name_munproj,
                           start_time_all,
                           end_time_all,
                           pop_up_title,
                           pop_up_message)

    })

})
}

