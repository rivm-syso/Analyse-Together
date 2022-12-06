download_api_button_output <- function(id) {
  useShinyalert()
  ns <- NS(id)
  #uiOutput(ns("download_api_button"))
  actionButton(ns("download_api_button"), "Get the data from external source")
}


download_api_button_server <- function(id,
                                       proj_or_mun,
                                       selection,
                                       daterange,
                                       pool,
                                       que) {

    moduleServer(id, function(input, output, session) {

                     ns <- session$ns


                     btn <- eventReactive(input$download_api_button, {T})

                     observeEvent(input$download_api_button, {
                                       type <- ifelse(is.null(proj_or_mun()), NA, proj_or_mun())
                                       name <- ifelse(is.null(selection()), NA, selection())
                                       tstart <- as_datetime(ifelse(is.null(daterange$selected_start_date()), NA, daterange$selected_start_date()))
                                       tend <- as_datetime(ifelse(is.null(daterange$selected_end_date()), NA, daterange$selected_end_date()))
                                       time_start <- daterange$selected_start_date() %>% as.POSIXct()
                                       time_end <- daterange$selected_end_date() %>%  as.POSIXct()
                                       log_trace("mod download: Download pushed with paremeters Type: {type}; name: {name}; time_start: {tstart}; time_end: {tend}")


                                       if(any(is.na(c(type, name, tstart, tend)))) {
                                           log_trace("mod download: Download not started, missing parameters")
                                       } else {
                                           log_trace("mod download: create download queue")

                                         # Add message pop up to the user
                                         shinyalert("Be patient!", "Data will be gathered, this can take some time. Please check the other button if data is available for usage.", type = "info")

                                         # Download the metadata
                                         download_sensor_meta(name, type = type)

                                         # Get the overview of the sensors
                                         kits <- get_stations_from_selection(name, type = type)
                                         log_trace("mod download: Overview kits opgehaald")
                                         # Download the data from the sensors - > via the queue
                                         for(i in seq(1, length(kits))){
                                           qid <- que$push(dl_station, list(kits[i],
                                                                            time_start,
                                                                           time_end),
                                                           id = kits[i])


                                           log_trace("pushed job {qid} to the queue")
                                         }

                                         # start queue
                                         que$poll()

                                       }
                     })
                     return(btn)

})
}
