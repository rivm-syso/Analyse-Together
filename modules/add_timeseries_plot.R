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
                              theme_plots,
                              zoom_in = NA){

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
         if(!is.list(zoom_in)){ # Check if the functionality of the zoom is used
           max_time <- max(data_timeseries$date)
           min_time <- min(data_timeseries$date)}
         else{ # Use the min/max of the zoom input
           max_time <- zoom_in$end_slider_zoom()
           min_time <- zoom_in$start_slider_zoom()
           # Extra check, somehow the reactive wil be available a moment later
           shiny::validate(need(!is.null(max_time), "Please, try other figure."))
         }

         n_days_in_plot <- round(as.numeric(max_time - min_time))
         date_breaks_in_plot <- paste0(as.character(dplyr::case_when(n_days_in_plot < 8 ~ 1, T ~ n_days_in_plot/7))," day")
         n_stat_in_plot <- length(unique(data_timeseries$label))

         min_meas <- plyr::round_any(min(data_timeseries$value-data_timeseries$sd, na.rm = T), 5, f = floor)
         max_meas <- plyr::round_any(max(data_timeseries$value+data_timeseries$sd, na.rm = T), 5, f = ceiling)
         steps <- plyr::round_any(max_meas / 15, 10, f = ceiling) # to create interactive y-breaks

         # Create background colors, to give indication of the value of the pm
         background_colours_df <- data.frame(
           col = c("#238aff", "#fff244","#ffbb4b","#f95a19"),
           label = c("blauw", "geel","oranje","rood"),
           y_max = c(12, 30, 60, 1000),
           y_min = c(0, 12, 30, 60))

         # Get the combination of label and color and linetype for the legend etc
         # for the lines:
         names_col_line <- data_timeseries %>%
           dplyr::select(label, col) %>%
           unique()
         names_col_line_plot <- setNames(names_col_line$col, names_col_line$label)
         names_col_line_plot <- names_col_line_plot[order(names(names_col_line_plot))]

         names_linetype <- data_timeseries %>% dplyr::select(label, linetype) %>% unique()
         names_linetype_plot <- setNames(names_linetype$linetype, names_linetype$label)
         names_linetype_plot <- names_linetype_plot[order(names(names_linetype_plot))]

         # For the ribbon and rectangle: (needed seperate to exclude from legend)
         names_col <- data_timeseries %>%
           dplyr::select(label, col) %>%
           unique() %>%
           dplyr::bind_rows(., background_colours_df %>% dplyr::select(label, col))
         names_col_plot <- setNames(names_col$col, names_col$label)
         names_col_plot <- names_col_plot[order(names(names_col_plot))]

         # Make a plot ====
         try(ggplot(data = data_timeseries) +
               geom_rect(data = background_colours_df,
                         aes(xmin = min_time, xmax = max_time,
                         ymin = y_min, ymax = y_max, fill = label),
                         alpha = 0.1, show.legend = F) +
               geom_ribbon(aes(x = date, y = value, ymin = ribbon_min,
                               ymax = ribbon_max, fill = label),
                           alpha = .2, show.legend = F) +
               geom_line(aes(x = date, y = value, color = label, linetype = label)) +
               scale_color_manual(values = names_col_line_plot,
                                  labels = names(names_col_line_plot)) +
               scale_fill_manual(values = names_col_plot,
                                 labels = names(names_col_plot)) +
               scale_linetype_manual(values =  names_linetype_plot,
                                     labels = names(names_linetype_plot)) +
               scale_x_datetime(date_breaks = date_breaks_in_plot,
                                date_minor_breaks = "1 day",
                                date_labels = "%d/%b/%y") +
               coord_cartesian(ylim = c(0, max_meas  + (steps/2)),
                               xlim = c(min_time, max_time)) +
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
