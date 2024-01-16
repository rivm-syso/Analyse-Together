###############################################
### Bar-plot Module ###
###############################################

# Creates a bar-plot with the chosen stations, parameters and time range

###################################################################
### Output Module ####
#################################################################

barplot_output <- function(id) {

  ns <- NS(id)

  plotOutput(ns("barplot_plot"))

}


######################################################################
# Server Module ####
######################################################################

barplot_server <- function(id,
                           data_measurements,
                           parameter,
                           overview_component,
                           theme_plots) {

  moduleServer(id, function(input, output, session) {

        ns <- session$ns

        output$barplot_plot <- renderPlot({

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

          # Calculate the mean and standard deviation
          data_barplot <- data_plot %>%
            dplyr::group_by(parameter, label) %>%
            dplyr::mutate(gemiddelde = round(mean(value, na.rm=TRUE), 2),
                          standaarddev = sd(value, na.rm = T),
                          n_obs = n()) %>%
            distinct(label, .keep_all = TRUE)

          # Make a plot
          try(ggplot(data = data_barplot, aes(y=gemiddelde, x=label)) +
            geom_bar(stat="identity", fill=paste0(data_barplot$col), color = 'black',
                     size = data_barplot$size/2) +
            labs(x = element_blank(), y = expression(paste("Concentration (", mu, "g/",m^3,")")),
                 title=paste0('Barplot for: ', parameter_label,
                              "  ",  min(data_plot$date) %>% format("%d/%b/%Y"),
                              " - ",  max(data_plot$date) %>% format("%d/%b/%Y")
                              )) +
            expand_limits(y=0) + # Make sure no negative values are shown
            theme_plots
        )

    })

  })

}
