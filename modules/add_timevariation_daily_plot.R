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
        min_meas <- plyr::round_any(min(a$data$hour$Upper, na.rm = T), 5, f = floor)
        max_meas <- plyr::round_any(max(a$data$hour$Lower, na.rm = T), 5, f = ceiling)
        steps <- plyr::round_any(max_meas / 15, 6, f = ceiling) # to create interactive y-breaks
        n_stat_in_plot <- length(unique(a$data$hour$variable))
        
        b <- ggplot(data = a$data$hour, aes(x = hour, y = Mean, group = variable)) +
              geom_line(aes(color = variable)) +
              geom_ribbon(aes(y = Mean, ymin = Lower, ymax = Upper, fill = variable), alpha = 0.2) +
              scale_color_manual(values = c(paste0(data_timevar$col)),
                                  breaks = c(paste0(data_timevar$station))) +
              scale_fill_manual(values=c(paste0(data_timevar$col)),
                                 breaks = c(paste0(data_timevar$station)), guide = 'none') +
              scale_y_continuous(breaks = seq(min_meas-steps,max_meas+steps, by = steps), minor_breaks = seq(min_meas-(steps/2),max_meas+(steps/2), by = steps/2), limits = c(min_meas-(steps/2), max_meas+(steps/2))) +
              scale_x_continuous(breaks = seq(0,23,2), minor_breaks = seq(0,23,1)) + 
              labs(x = "Hour of the day", y = expression(paste("Concentration (", mu, "g/",m^3,")")), title=paste0('Daily pattern for: ', parameter_label)) +
              expand_limits(y=0) +
              theme_plots +
              theme(legend.text = element_text(size = paste0(16-log(n_stat_in_plot)*2)),
                    axis.text.x = element_text(color = "black", size = 16, angle = 0, hjust = 0.5, vjust = 0))  +
              guides(colour = guide_legend(title = "Sensor / Station", override.aes = list(size=3)))
        b
        }
      else{
          verbatimTextOutput("Selecteer een sensor.")
      }

    })
    
  })

}
