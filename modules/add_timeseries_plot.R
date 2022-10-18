###############################################
### Time Series Plot Module ###
###############################################

# Creates a time series plot with the chosen stations, parameters and time range

###################################################################
### Output Module ####
#################################################################

timeseries_output <- function(id) {

  ns <- NS(id)

  plotOutput(ns("timeseries_plot"), hover = hoverOpts(id ="plot_hover"))

}


######################################################################
# Server Module
######################################################################

timeseries_server <- function(id,
                              data_measurements_stations,
                              overview_component,
                              theme_plots) {

  moduleServer(id, function(input, output, session) {

                 ns <- session$ns

                 # Get selected measurements from communication module
                 data_measurements <- reactive({
                   data_measurements <- data_measurements_stations$selected_measurements()
                   return(data_measurements)
                   })

                 # Get selected stations from communication module
                 data_stations <- reactive({
                   data_stations <- data_measurements_stations$station_locations()
                   return(data_stations)
                   })

                 # Create time plot with ggplot
                 output$timeseries_plot <- renderPlot({

                   # Determine parameter that needs to be plotted
                   parameter <- data_measurements()$parameter

                   # Find the corresponding label
                   parameter_label <- filter(overview_component, component == parameter[1])['label']
                   if (nrow(parameter_label) < 1){
                     parameter_label <- " "
                   }
                   # Ad colour and linetype to the data_measurements
                   data_timeseries <- data_measurements() %>% left_join(select(data_stations(), c(station, col, linetype, size, station_type)), by = "station")

                   # Calculate standard deviation
                   data_timeseries <- data_timeseries %>% group_by(station) %>% mutate(sd = sd(value)) %>% ungroup()
                   n_days_in_plot <- round(as.numeric(max(data_timeseries$date) - min(data_timeseries$date)))
                   n_stat_in_plot <- length(unique(data_timeseries$station))
                   min_meas <- plyr::round_any(min(data_timeseries$value), 5, f = floor)
                   max_meas <- plyr::round_any(max(data_timeseries$value), 5, f = ceiling)
                   steps <- plyr::round_any(max_meas / 15, 10, f = ceiling) # to create interactive y-breaks

                   # Make a plot
                   if (length(parameter>0)){
                       try(ggplot(data = data_timeseries, aes(x = date, y = value, group = station)) +
                         geom_line(aes(color = station, linetype=station_type)) +
                         geom_ribbon(aes(y = value, ymin = value - sd, ymax = value + sd, fill = station), alpha = .2) +
                         scale_color_manual(values = c(paste0(data_timeseries$col)),
                                            breaks = c(paste0(data_timeseries$station))) +
                         scale_fill_manual(values=c(paste0(data_timeseries$col)),
                                           breaks = c(paste0(data_timeseries$station))) +
                         scale_size_manual(values = c(paste0(data_timeseries$station_type)),
                                           breaks = c(paste0(data_timeseries$size)), guide = 'none') +
                         scale_x_datetime(date_breaks = paste0(as.character(round(n_days_in_plot/7))," day"), date_minor_breaks = "1 day") +
                         scale_y_continuous(breaks = seq(min_meas-steps,max_meas+steps, by = steps), minor_breaks = seq(min_meas-(steps/2),max_meas+(steps/2), by = steps/2), limits = c(min_meas-(steps/2), max_meas+(steps/2))) +
                         labs(x = "Date", y = expression(paste("Concentration (", mu, "g/",m^3,")")), title=paste0('Timeseries for: ', parameter_label)) +
                         expand_limits(y=0) +
                         theme_plots +
                         theme(legend.text = element_text(size = paste0(16-log(n_stat_in_plot)*2)))  +
                         guides(colour = guide_legend(override.aes = list(size=2)),
                                linetype = guide_legend(override.aes = list(size = 1)))
                       )
                     }

                 })

               })

}