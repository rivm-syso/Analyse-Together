###############################################
### TimeVariation-plot Module: weekly ###
###############################################

# Creates a timevariation-plot with the chosen stations, parameters and time range
# Weekly version!
###################################################################
### Output Module ####
#################################################################

timevar_weekly_output <- function(id) {

  ns <- NS(id)
  plotOutput(ns("timevar_plot_weekly"))

}


######################################################################
# Server Module
######################################################################

timevar_weekly_server <- function(id,
                                  data_measurements,
                                  parameter,
                                  overview_component) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    output$timevar_plot_weekly <- renderPlot({
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


      # Make a plot
      plot_all <- data_plot %>% dplyr::mutate(daynumber = weekdays(date)) %>%
                                dplyr::group_by(label, daynumber) %>%
                                dplyr::mutate(mean_day = mean(value, na.rm = T))
      
      # Obtain info for the axis
      min_meas <- plyr::round_any(min(plot_all$mean_day, na.rm = T), 5, f = floor)
      max_meas <- plyr::round_any(max(plot_all$mean_day, na.rm = T), 5, f = ceiling)
      steps <- plyr::round_any(max_meas / 15, 6, f = ceiling) # to create interactive y-breaks
      n_stat_in_plot <- length(unique(plot_all$col))
      
      plot_part <- ggplot(data = plot_all) +
        geom_line(aes(x = daynumber, y = mean_day, group = label, color = label), lwd = 1) +
        scale_color_manual(values = plot_all$col,
                           breaks = plot_all$label) +
        scale_y_continuous(breaks = seq(min_meas-steps,max_meas+steps, by = steps),
                           minor_breaks = seq(min_meas-(steps/2),max_meas+(steps/2),
                                              by = steps/2),
                           limits = c(min_meas-(steps/2), max_meas+(steps/2))) +
       labs(x = "Day of the week", y = expression(paste("Concentration (", mu, "g/",m^3,")")),
             title=paste0('Daily pattern for: ', parameter_label)) +
        expand_limits(y=0) +
        theme_plots +
        theme(legend.text = element_text(size = paste0(16-log(n_stat_in_plot)*2)),
              axis.text.x = element_text(color = "black", size = 16, angle = 0,
                                         hjust = 0.5, vjust = 0))  +
        guides(colour = guide_legend(title = "Group / Station",
                                     override.aes = list(size=3)))
      
      plot_part

    })

  })

}
