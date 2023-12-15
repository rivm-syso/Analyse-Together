###############################################
### Get data button ###
###############################################

# This is a module which get the data from the (cache)database
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
                                  group_name_none
) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    output$get_dbs_cache <- renderUI({
      tagList(
        actionButton(ns("get_dbs_cache"), text_button, style="background-color: #ffe9b7")

      )
    })

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
      # Load the data from the caching database
      # Get the station names in the selected Municipality/project
      stations_name <- get_stations_from_selection(name_choice, type_choice, conn = pool)

      log_info("get mod: Get data from caching database: {name_choice} ; {start_time} ; {end_time}... ")
      message_data$to_start_page <- c(paste0("To start page if:  ", type_choice, " ", name_choice," " , start_time, end_time, "has changed."))

      # Estimate the download time, if sensors exists in selection
      if(is.null(stations_name)){
        message_data$download_estimation <- c(paste0("No information available yet, please press the get data button (right button)."))
      }else{
        estimate_time <- ceiling((length(stations_name) * 7 + 30)/60)
        message_data$download_estimation <- c(paste0("Estimated load time from external source: ", estimate_time, " minutes."))
      }

      # Get the data measurements of the selected Municipality/project in the period
      # and do some data cleaning
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

      # Check if there is data in de caching, otherwise stop and give message
      if(nrow(data_measurements$data_all) == 0){
        message_data$data_in_dbs <- c(paste0("No data in ", type_choice, " ", name_choice))
        shiny::validate(need(F,message = paste0("No data in ", type_choice, " ", name_choice)))
      }

      message_data$data_in_dbs <- c(paste0("Data available in ", type_choice, " ", name_choice))

      log_info("get mod: Data available in tool. ")

    })

  })
}
