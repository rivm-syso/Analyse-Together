###############################################
### Show data availablility cache database ###
###############################################

# This is a module which shows in a shinyalert, the data available in the
# cache data base, and returns feedback wheter or not to download
######################################################################
# Output Module
######################################################################

show_data_cache_output <- function(id) {

  ns <- NS(id)
  uiOutput(ns("show_data_cache"))

}

######################################################################
# Server Module
######################################################################

show_data_cache_server <- function(id,
                                   title_plot,
                                   data_to_plot,
                                   data_other,
                                   timeranges_to_download,
                                   stations_name
) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    output$show_data_cache <- renderUI({
     tagList(
           plotOutput(ns("plot_cache")),
           textOutput(ns("missing_days"))
    )
    })

    # Create list where to observe changes to react on
    tolisten <- reactive({
      list(data_to_plot,
           timeranges_to_download)
    })

    # Observe is a new plot is needed
    observeEvent(tolisten(),{

      # Initiate the 'flags' to create some buttons to laod or download the data
      create_btn_get_data <- F
      create_btn_use_data <- F

      # Create visualisation of the available data in cache
      # If no data at all, set available to FALSE
      if(purrr::is_empty(stations_name)){
        data_to_plot <- data_to_plot %>% dplyr::mutate(available = F)
        create_btn_get_data <- T
      }else{ # If there is data set to TRUE
        data_to_plot <- data_to_plot %>% dplyr::mutate(available = T)
        create_btn_use_data <- T
        # Check which periods aren't available
        if(T %in% (timeranges_to_download > 0)){

          create_btn_get_data <- T

          # Assumption is that all stations have the same missing period
          start_unavailable <- lubridate::as_datetime(timeranges_to_download[1],
                                                      tz = "Europe/Amsterdam") %>%
            lubridate::as_date()
          end_unavailable <- lubridate::as_datetime(timeranges_to_download[2],
                                                    tz = "Europe/Amsterdam") %>%
            lubridate::as_date()

          seq_unavailable <- seq((start_unavailable),
                                 (end_unavailable),
                                 by = "1 day")

          # Set all the days to available and create plot
          data_to_plot <- data_to_plot %>%
            dplyr::mutate(available = ifelse(date %in% seq_unavailable, F, T)
            )
        }
      }

      # plot output with the available data
      output$plot_cache <- renderPlot({
        openair::calendarPlot(data_to_plot,
                              pollutant = "available",
                              cols = c( "#d95f02","#1b9e77"),
                              breaks = c(-0.2,0.8,1.2),
                              labels = c( "Afwezig", "Aanwezig"),
                              key.position = "top",
                              key.header = title_plot)
      })

      # calculate the data missing
      missing_days <- data_to_plot %>%
        dplyr::filter(available == F) %>%
        count()

      # return number of missing data
      data_other$missing_days <- list(missing_days = missing_days,
                                      create_btn_get_data = create_btn_get_data,
                                      create_btn_use_data = create_btn_use_data,
                                      counter = lubridate::now())

      # text output with number of missing data days
      output$missing_days <- renderText({

        paste0("Er missen nog: ", missing_days, " dagen in de gekozen tijdsperiode.")
      })

    })


  })
}
