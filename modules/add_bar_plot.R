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
                           data_stations,
                           data_other,
                           overview_component,
                           theme_plots) {

  moduleServer(id, function(input, output, session) {

        ns <- session$ns

        output$barplot_plot <- renderPlot({

          # Get the data to plot
          data_plot <- data_measurements$data_filtered

          # Check if there is data to plot
          shiny::validate(
            need(!is_empty(data_plot) | !dim(data_plot)[1] == 0,
                 'Geen sensordata beschikbaar.')
          )

          # Get the colours for the stations
          data_stations <- data_stations$data %>%
            dplyr::select(c(station, col, linetype, size)) %>%
            dplyr::distinct(station, .keep_all = T) %>%
            dplyr::filter(!grepl("KNMI", station))

          # Determine parameter for the label in the plot
          parameter <- data_other$parameter

          # Find the corresponding label
          parameter_label <- overview_component %>%
            dplyr::filter(component == parameter) %>%
            dplyr::pull(label)

          # Calculate the mean and standard deviation
          data_barplot <- data_plot %>%
            dplyr::group_by(parameter, station) %>%
            dplyr::mutate(gemiddelde = round(mean(value, na.rm=TRUE), 2),
                          standaarddev = sd(value, na.rm = T),
                          n_obs = n()) %>%
            distinct(station, .keep_all = TRUE)

          data_barplot <- data_barplot %>%
            dplyr::left_join(data_stations, by = "station")

          # Make a plot
          try(ggplot(data = data_barplot, aes(y=gemiddelde, x=station)) +
            geom_bar(stat="identity", fill=paste0(data_barplot$col), color = 'black', size = data_barplot$size/2) +
            geom_errorbar(aes(ymin=gemiddelde-standaarddev, ymax=gemiddelde+standaarddev), size = data_barplot$size/2, width=.2,
                          position=position_dodge(.9), color='black') +
            geom_text(aes(y = gemiddelde-gemiddelde+2, label = n_obs), colour = 'white', size = 9-2*log(length(unique(data_barplot$station)))) +
            labs(x = element_blank(), y = expression(paste("Concentration (", mu, "g/",m^3,")")), title=paste0('Barplot for: ', parameter_label)) +
            expand_limits(y=0) + # Make sure no negative values are shown
            theme_plots
        )

    })

  })

}
