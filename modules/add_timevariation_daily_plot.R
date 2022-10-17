###############################################
### TimeVariation-plot Module ###
###############################################

# Creates a timevariation-plot with the chosen stations, parameters and time range

###################################################################
### Output Module ####
#################################################################
timevar_daily_output <- function(id) {

  ns <- NS(id)
  plotOutput(ns("timevar_plot_daily"))

}


######################################################################
# Server Module
######################################################################
#
#

timevar_daily_server <- function(id, data_measurements_stations) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # Determine parameter that needs to be plotted
    # Get selected measurements from communication module
    data_measurements <- reactive({
      data_measurements <- data_measurements_stations$selected_measurements()
      return(data_measurements)
    })

    # Get selected stations from communication module
    data_stations <- reactive({
      data_stations <- data_measurements_stations$station_locations() %>% select(c(station, col)) %>% dplyr::distinct(station, .keep_all = T)
      return(data_stations)
    })

    output$timevar_plot_daily <- renderPlot({

      # Determine parameter that needs to be plotted
      parameter_sel <- data_measurements()$parameter

      # Find the corresponding label
      parameter_label <- str_replace(toupper(parameter_sel[1]), '_', ' - ')

      data_timevar <- data_measurements() %>% filter(parameter == parameter_sel)
      data_timevar <- merge(data_timevar, data_stations(), by = 'station')

      # Make a plot
      data_timevar$value <- as.numeric(data_timevar$value)

      if (length(parameter_sel>0)){
        a <- timeVariation(data_timevar,
                      pollutant = "value", normalise = FALSE, group = "station",
                      alpha = 0.1, cols = data_timevar$col, local.tz="Europe/Amsterdam",
                      ylim = c(0,NA),
                      ylab = paste0(unique(data_timevar$parameter)),
                      xlab = "Hour of the day",
                      start.day = 1,
                      par.settings=list(fontsize=list(text=15)),
                      key = T
                      )
        a[[1]][2]
        }
      else{
          verbatimTextOutput("Selecteer een sensor.")
      }

    })
    
  })

}
