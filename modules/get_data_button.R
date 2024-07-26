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

    output$get_dbs_cache <- renderUI({
      tagList(
        actionButton(ns("get_dbs_cache"),
                     text_button,
                     style="background-color: #ffe9b7"),
        show_data_cache_output(ns("show_data_cache")),
        btn_do_data_actions_output(ns("btns_data_actions"))

      )
    })

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

      # Get the selected time period
      start_time <- selected_start_date()
      end_time <- selected_end_date() - 1

      # Check if there is selected start/end time
      shiny::validate(
        need(!is_empty((start_time)),"Please, select periode"),
        need(!is_empty((end_time)),"Please, select periode")
      )


      # Create dataframe for plotting availability in cache
      data_to_plot <- data.frame(date = seq(start_time,
                                            end_time,
                                            by='1 day')
      )

      # Load the data from the caching database
      # Get the station names in the selected Municipality/project
      stations_name <- get_stations_from_selection(name_choice,
                                                   type_choice,
                                                   conn = pool)

      # Get the missing time ranges from cache database
      timeranges_to_download <- sapply(stations_name, function(x){
        ATdatabase::get_download_ranges(x,
                                        start_time %>% as.POSIXct(),
                                        end_time %>% as.POSIXct(),
                                        pool)
      })
      timeranges_to_download <- unlist(timeranges_to_download)

      return(list(type_choice = type_choice,
                  name_choice = name_choice,
                  start_time = start_time,
                  end_time = end_time,
                  data_to_plot = data_to_plot,
                  stations_name = stations_name,
                  timeranges_to_download = timeranges_to_download
                  ))

    })

    # Observe what is expected after checking the cache database ----
    observeEvent(data_other$missing_days, {

      # Check if there is new data
      shiny::validate(need(class(data_other$missing_days) == "list" ,
                           "start of tool"))

      # Check the input values, if available and get some more info
      input_values <- isolate(initiate_status())

      # Create the action buttons and actions for obtaining the data
      btn_do_data_actions_server("btns_data_actions",
                                 data_measurements = data_measurements,
                                 data_stations = data_stations,
                                 missing_days = data_other$missing_days,
                                 measurements_con = measurements_con,
                                 stations_con = stations_con,
                                 stations_name = input_values$stations_name,
                                 start_time = input_values$start_time,
                                 end_time =  input_values$end_time,
                                 mun_or_proj = input_values$type_choice,
                                 name_munproj = input_values$name_choice,
                                 col_default = col_default,
                                 line_default = line_default,
                                 group_name_none = group_name_default,
                                 line_overload = line_overload,
                                 pop_up_title = pop_up_title,
                                 pop_up_message = pop_up_message
      )

    })

    # Observe if the get_dbs_cache is clicked ----
    observeEvent(input$get_dbs_cache, {

      # Check the input values, if available and get some more info
      input_values <- isolate(initiate_status())

      # Show visualisation of data availability in cache dbs
      # get results of which steps to continue
      show_data_cache_server("show_data_cache",
                             title_plot = input_values$name_choice,
                             data_other = data_other,
                             data_to_plot = input_values$data_to_plot,
                             stations_name = input_values$stations_name,
                             timeranges_to_download =
                               input_values$timeranges_to_download)

  })
  })
}
