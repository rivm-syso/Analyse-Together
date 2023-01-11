###############################################
### Time Series Plot Module ###
###############################################

# Creates a time series plot with the chosen stations, parameters and time range

###################################################################
### Output Module ####
#################################################################

timeseries_output <- function(id) {

  ns <- NS(id)

  plotOutput(ns("timeseries_plot"))

}


######################################################################
# Server Module
######################################################################

timeseries_server <- function(id,
                              data_measurements,
                              data_stations,
                              data_other,
                              overview_component,
                              theme_plots){

  moduleServer(id, function(input, output, session) {

         ns <- session$ns

         # Create time plot with ggplot
         output$timeseries_plot <- renderPlot({
           
           # Get the data to plot
           data_plot <- data_measurements$data_filtered

         # Check if there is data to plot
         shiny::validate(
           need(!is_empty(data_plot) | !dim(data_plot)[1] == 0,
                'Geen sensordata beschikbaar.')
           )

         # Get the colours for the stations
         data_stations <- data_stations$data %>%
           dplyr::select(c(station, col, linetype, size, station_type)) %>%
           dplyr::distinct(station, .keep_all = T) %>%
           dplyr::filter(!grepl("KNMI", station))

         # Determine parameter for the label in the plot
         parameter <- data_other$parameter

         # Find the corresponding label
         parameter_label <- overview_component %>%
           dplyr::filter(component == parameter) %>%
           dplyr::pull(label)

         # Add colour and linetype to the data_measurements
         data_timeseries <- data_plot %>%
           dplyr::left_join(data_stations, by = "station") %>%
           dplyr::mutate(ribbon_min = value - sd,
                         ribbon_max = value + sd) %>%
           dplyr::mutate(ribbon_min = ifelse(ribbon_min < 0, 0, ribbon_min),
                         ribbon_max = ifelse(ribbon_max < 0, 0, ribbon_max))

         # Calculate stats for the axis
         n_days_in_plot <- round(as.numeric(max(data_timeseries$date) - min(data_timeseries$date)))
         n_stat_in_plot <- length(unique(data_timeseries$station))

         min_meas <- plyr::round_any(min(data_timeseries$value-data_timeseries$sd, na.rm = T), 5, f = floor)
         max_meas <- plyr::round_any(max(data_timeseries$value+data_timeseries$sd, na.rm = T), 5, f = ceiling)
         steps <- plyr::round_any(max_meas / 15, 10, f = ceiling) # to create interactive y-breaks

         # Make a plot ====
         try(ggplot(data = data_timeseries, aes(x = date, y = value, group = station)) +
               geom_line(aes(color = station, linetype=station_type)) +
               geom_ribbon(aes(y = value, ymin = ribbon_min, ymax = ribbon_max, fill = station), alpha = .2) +
               scale_color_manual(values = c(paste0(data_timeseries$col)),
                                  breaks = c(paste0(data_timeseries$station))) +
               scale_fill_manual(values=c(paste0(data_timeseries$col)),
                                 breaks = c(paste0(data_timeseries$station))) +
               scale_size_manual(values = c(paste0(data_timeseries$station_type)),
                                 breaks = c(paste0(data_timeseries$size)), guide = 'none') +
               scale_x_datetime(date_breaks = paste0(as.character(dplyr::case_when(n_days_in_plot < 8 ~ 1, T ~ n_days_in_plot/7))," day"), date_minor_breaks = "1 day") +
               scale_y_continuous(breaks = seq(min_meas - steps, max_meas + steps, by = steps), minor_breaks = seq(min_meas - (steps/2), max_meas + (steps/2), by = steps/2), limits = c(0, max_meas + (steps/2))) +
               labs(x = "Date", y = expression(paste("Concentration (", mu, "g/",m^3,")")), title=paste0('Timeseries for: ', parameter_label)) +
               theme_plots +
               theme(legend.text = element_text(size = paste0(16-log(n_stat_in_plot)*2)))  +
               guides(colour = guide_legend(override.aes = list(size=2)),
                      linetype = guide_legend(override.aes = list(size = 1)))
         )

     })

   })

}
