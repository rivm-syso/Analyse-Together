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
# Server Module
######################################################################
#
#

barplot_server <- function(id, data_measurements_stations, overview_component) {
  
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
      data_stations <- data_measurements_stations$station_locations() %>% select(c(station, col, linetype, size)) %>% dplyr::distinct(station, .keep_all = T)
      return(data_stations)
    })
    
    output$barplot_plot <- renderPlot({
      
      # Determine parameter that needs to be plotted
      parameter_sel <- data_measurements()$parameter
      
      # Find the corresponding label
      parameter_label <- filter(overview_component, component == parameter_sel[1])['label']
      if (nrow(parameter_label) < 1){
        parameter_label <- " "
      }
      
      data_barplot <- data_measurements() %>% filter(parameter == parameter_sel)
      data_barplot <- data_barplot %>% group_by(parameter, station) %>% mutate(gemiddelde = round(mean(value, na.rm=TRUE), 2),
                                                                               standaarddev = sd(value, na.rm = T),
                                                                               n_obs = n()) %>% 
                                                                        distinct(station, .keep_all = TRUE)
      
      data_barplot <- merge(data_barplot, data_stations(), by = 'station')
      
      theme_plots <- theme_bw(base_size = 18) + 
        theme(strip.text.x = element_text(size = 14, colour = "black"),
              axis.text.y = element_text(face = "bold",color = "black", size = 16),
              axis.text.x = element_text(color = "black", size = 15),
              axis.title = element_text(color = "black", size = 16),
              text = element_text(family = 'serif'),
              title = element_text(color = "black", size = 16),
              legend.title = element_text(size = 16),
              legend.text = element_text(size = 16),
              legend.key.height = unit(0.5, 'cm'),
              legend.key.width = unit(1, 'cm'),
              panel.border = element_rect(colour = "black", fill=NA, size=1))
      
      # Make a plot
      if (length(parameter_sel>0)){
        try(ggplot(data = data_barplot, aes(y=gemiddelde, x=station)) +
          geom_bar(stat="identity", fill=paste0(data_barplot$col), color = 'black', size = data_barplot$size/2) +
          geom_errorbar(aes(ymin=gemiddelde-standaarddev, ymax=gemiddelde+standaarddev), size = data_barplot$size/2, width=.2,
                        position=position_dodge(.9), color='black') +
          geom_text(aes(y = gemiddelde-gemiddelde+1, label = n_obs), colour = 'white', size = 5-0.2*(length(unique(data_barplot$station)))) +
          labs(x = element_blank(), y = expression(paste("Concentration (", mu, "g/",m^3,")")), title=paste0('Barplot for: ', parameter_label)) +
          expand_limits(y=0) + # Make sure no negative values are shown
          theme_plots +
          theme(axis.text.x=element_text(angle = 45, hjust = 1, vjust = 1))
      
      )
      }
    })
    
  })
  
  
}





