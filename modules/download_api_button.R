###############################################
#### Module for the download ####
###############################################

# This is module to download the data from api to database
######################################################################
# Output Module ----
######################################################################
download_api_button_output <- function(id) {

  useShinyalert()
  ns <- NS(id)
  actionButton(ns("download_api_button"), i18n$t("btn_external_source"))
}


######################################################################
# Server Module ----
######################################################################

download_api_button_server <- function(id,
                                       proj_or_mun,
                                       name_munproj,
                                       daterange,
                                       pool)
  {

    moduleServer(id, function(input, output, session) {

     ns <- session$ns

     btn <- eventReactive(input$download_api_button, {T})

     observeEvent(input$download_api_button, {
         type <- ifelse(is.null(proj_or_mun()), NA, proj_or_mun())
         name <- ifelse(is.null(name_munproj()), NA, name_munproj())
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
           dl_req <- data.frame()
           job_id <- sprintf("id%010.0f", round(runif(1, 1, 2^32), digits = 0))
           for(i in seq(1, length(kits))){
               dl_req <- bind_rows(dl_req, data.frame(station = kits[i],
                                                      time_start = time_start,
                                                      time_end = time_end,
                                                      row.names = NULL))

           }
           if(!doc_exists(type = "data_req", ref = job_id, conn = pool)) {
             log_trace("data request {job_id} stored")
               # this observeEvent is
               # called multiple times,
               # and multiple writes to
               # the db as result
               add_doc(type = "data_req", ref = job_id, doc = dl_req,
                       conn = pool, overwrite = FALSE)
           }

         }
     })
     return(btn)
})
}
