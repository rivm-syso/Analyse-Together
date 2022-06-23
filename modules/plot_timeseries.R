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

timeseries_server <- function(id, data_measurements_stations, overview_component) {
  
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
                   
                   # Ad colour and linetype to the data_measurements
                   data_timeseries <- data_measurements() %>% left_join(select(data_stations(), c(station, col, linetype)), by = "station")
                   
                   # Calculate standard deviation
                   data_timeseries <- data_timeseries %>% group_by(station) %>% mutate(sd = sd(value)) %>% ungroup()

                   # Make a plot
                   ggplot(data = data_timeseries, aes(x = date, y = value, group = station)) +
                     geom_line(aes(linetype = station, col = station)) +
                     geom_ribbon(aes(y = value, ymin = value - sd, ymax = value + sd, fill = station), alpha = .2) +
                     labs(x = "Date", y = expression(paste("Concentration (", mu, "g/",m^3,")")), title=paste0('Component: ', parameter_label)) +
                     expand_limits(y=0) +
                     theme_bw()
                   
                 })
                 
               })
  
  
}

