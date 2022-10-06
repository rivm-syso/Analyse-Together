download_api_button_output <- function(id) {

  ns <- NS(id)
  uiOutput(ns("download_api_button"))
}


download_api_button_server <- function(id, proj_or_mun, selection, daterange) {

    moduleServer(id, function(input, output, session) {

                     ns <- session$ns

                     output$download_api_button <- renderUI({
                         actionButton(ns("download_api_button"), "start download")

                     })


                     observeEvent(input$download_api_button, {
                                       type <- ifelse(is.null(proj_or_mun()), NA, proj_or_mun())
                                       name <- ifelse(is.null(selection()), NA, selection())
                                       tstart <- as_datetime(ifelse(is.null(daterange$selected_start_date()), NA, daterange$selected_start_date()))
                                       tend <- as_datetime(ifelse(is.null(daterange$selected_end_date()), NA, daterange$selected_end_date()))
                                       log_trace("Download pushed with paremeters Type: {type}; name: {name}; time_start: {tstart}; time_end: {tend}")

                                       if(any(is.na(c(type, name, tstart, tend)))) {
                                           log_trace("Download not started, missing parameters") 
                                       } else {
                                           log_trace("Download should start, not implemented yet")

                                           # !!!!!!!!!!
                                           # Add download code here
                                           # !!!!!!!!!!!!!!!!!!
                                       }
                     })


                     return(TRUE)

})
}
