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
                              parameter,
                              overview_component,
                              theme_plots){

  moduleServer(id, function(input, output, session) {

         ns <- session$ns

         # Create time plot with ggplot
         output$timeseries_plot <- renderPlot({
         # Get the data to plot
         data_plot <- data_measurements()

         # Check if there is data to plot
         shiny::validate(
           need(!is_empty(data_plot) | !dim(data_plot)[1] == 0,
                'Geen sensordata beschikbaar.')
           )

          # Determine parameter for the label in the plot
         parameter <- parameter()

         # Find the corresponding label
         parameter_label <- overview_component %>%
           dplyr::filter(component == parameter) %>%
           dplyr::pull(label)

         # Add max min values for the ribbon
         data_timeseries <- data_plot %>%
           dplyr::mutate(ribbon_min = value - sd,
                         ribbon_max = value + sd) %>%
           dplyr::mutate(ribbon_min = ifelse(ribbon_min < 0, 0, ribbon_min),
                         ribbon_max = ifelse(ribbon_max < 0, 0, ribbon_max))

         # Calculate stats for the axis
         n_days_in_plot <- round(as.numeric(max(data_timeseries$date) - min(data_timeseries$date)))
         date_breaks_in_plot <- paste0(as.character(dplyr::case_when(n_days_in_plot < 8 ~ 1, T ~ n_days_in_plot/7))," day")
         n_stat_in_plot <- length(unique(data_timeseries$label))

         min_meas <- plyr::round_any(min(data_timeseries$value-data_timeseries$sd, na.rm = T), 5, f = floor)
         max_meas <- plyr::round_any(max(data_timeseries$value+data_timeseries$sd, na.rm = T), 5, f = ceiling)
         steps <- plyr::round_any(max_meas / 15, 10, f = ceiling) # to create interactive y-breaks

         # Get the combination of label and color and linetype for the legend etc
         names_col <- data_timeseries %>% dplyr::select(label, col) %>% unique()
         names_col_plot <- setNames(names_col$col, names_col$label)
         names_col_plot <- names_col_plot[order(names(names_col_plot))]

         names_linetype <- data_timeseries %>% dplyr::select(label, linetype) %>% unique()
         names_linetype_plot <- setNames(names_linetype$linetype, names_linetype$label)
         names_linetype_plot <- names_linetype_plot[order(names(names_linetype_plot))]

         # Make a plot ====
         try(ggplot(data = data_timeseries, aes(x = date, y = value)) +
               geom_line(aes(color = label, linetype = label)) +
               geom_ribbon(aes(y = value, ymin = ribbon_min,
                               ymax = ribbon_max, fill = label), alpha = .2) +
               scale_color_manual(values = names_col_plot,
                                  labels = names(names_col_plot)) +
               scale_fill_manual(values = names_col_plot,
                                 labels = names(names_col_plot)) +
               scale_linetype_manual(values =  names_linetype_plot,
                                     labels = names(names_linetype_plot)) +
               scale_x_datetime(date_breaks = date_breaks_in_plot,
                                date_minor_breaks = "1 day") +
               scale_y_continuous(breaks = seq(min_meas - steps, max_meas + steps, by = steps),
                                  minor_breaks = seq(min_meas - (steps/2), max_meas + (steps/2),
                                                     by = steps/2),
                                  limits = c(0, max_meas + (steps/2))) +
               labs(x = "Date", y = expression(paste("Concentration (", mu, "g/",m^3,")")),
                    title = paste0('Timeseries for: ', parameter_label)) +
               theme_plots +
               theme(legend.text=element_text(size = paste0(16-log(n_stat_in_plot)*2)),
                     legend.position="top",
                     legend.title=element_blank())
         )

     })

   })

}
