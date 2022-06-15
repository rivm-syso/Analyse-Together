###############################################
### Bar-plot Module ###
###############################################

# Creates a bar-plot with the chosen stations, parameters and time range

###################################################################
### Output Module ####
#################################################################
barplot_output <- function(id) {
  
  ns <- NS(id)
  
  plotlyOutput(ns("barplot_plot"))
  
}


######################################################################
# Server Module
######################################################################
#
#

barplot_server <- function(id, data_measurements) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # Determine parameter that needs to be plotted
    parameter <- data_measurements$parameter
    # Find the corresponding label
    #parameter_label <- filter(overview_component, component == parameter)['label']
    
    # Check if any stations are selected
    # if(data_stations$selected){
    #   validate("Please select a sensor or an air quality monitoring station.")
    # }
    
    # Ad colour and linetype to the data_measurements
    data_barplot <- data_measurements %>% filter(parameter == "pm25")#%>% left_join(select(data_stations, c(station, col, linetype)), by = "station")
    data_barplot <- data_barplot %>% group_by(parameter, station) %>% mutate(gemiddelde = round(mean(value, na.rm=TRUE), 2)) %>% 
      distinct(station, .keep_all = TRUE)
      
    output$barplot_plot <- renderPlotly({
      
      # Make a plot
      barplot_plot <- ggplot(data = data_barplot, aes(y=gemiddelde, x=station, color=station)) +
        geom_bar(stat="identity", fill="white") +
        labs(x = "Sensor or station number", y = paste0("Concentratie ", parameter), title=paste0('Parameter: ', parameter)) +
        expand_limits(y=0) + # Make sure no negative values are shown
        theme_bw() +
        theme(axis.text.x=element_text(angle = -90, hjust = 0))
      
      ggplotly(barplot_plot, dynamicTicks = T)
      
      
    })
    
  })
  
  
}





