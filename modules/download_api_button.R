###############################################
#### Module for the download ####
###############################################

# This is module to download the data from api to database
######################################################################
# Output Module ----
######################################################################
download_api_button_output <- function(id) {

  ns <- NS(id)
  actionButton(ns("download_api_button"), i18n$t("btn_external_source"))
}


######################################################################
# Server Module ----
######################################################################

download_api_button_server <- function(id,
                                       proj_or_mun,
                                       name_munproj,
                                       selected_start_date,
                                       selected_end_date,
                                       pool)
  {

    moduleServer(id, function(input, output, session) {

     ns <- session$ns

     btn <- eventReactive(input$download_api_button, {T})

     observeEvent(input$download_api_button, {
         type <- ifelse(is.null(proj_or_mun()), NA, proj_or_mun())
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
           shinyalert("Be patient!", "Data will be gathered, this can take some time. Please check the other button if data is available for usage.", type = "info")

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
     })
     return(btn)
})
}
