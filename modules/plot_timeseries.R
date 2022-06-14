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
#
#

timeseries_server <- function(id, data_measurements, data_stations) {
  
  moduleServer(id, function(input, output, session) {
                 
                 ns <- session$ns
                 
                 # Determine parameter that needs to be plotted
                 parameter <- data_measurements$parameter
                 # Find the corresponding label
                 # parameter_label <- filter(overview_component, component == parameter)['label']
                 
                 # Check if any stations are selected
                 selected_sensor <- (TRUE %in% test_loc_col$selected)
                 if(selected_sensor==FALSE){
                   validate("Please select a sensor or an air quality monitoring station.")
                 }
                 
                 # Ad colour and linetype to the data_measurements
                 data_timeseries <- data_measurements %>% left_join(select(data_stations, c(station, col, linetype)), by = "station")
                 
                 output$timeseries_plot <- renderPlot({
                   
                   # Make a plot
                   ggplot(data = data_timeseries, aes(x = date, y = value, group = station)) +
                     geom_line(aes(linetype = station, col = station)) +
                     geom_ribbon(aes(y = value, ymin = value - sd, ymax = value + sd, fill = station), alpha = .2) +
                     labs(x = "Date", y = expression(paste("Concentratie (", mu, "g/",m^3,")")), title=paste0('Parameter: ', parameter)) +
                     expand_limits(y=0) + 
                     theme_bw()
                   
                 })
                 
               })
  
  
}


######################################################################
# TEMPORARY TEST CODE
######################################################################

# overview_component <- data.frame('component' = c(" ","pm10","pm10_kal","pm25","pm25_kal"), 'label'=c(" ","PM10","PM10 - gekalibreerd","PM2.5" ,"PM2.5 - gekalibreerd" ))
test_loc_col <- readRDS("data/test_loc_col.RDS")
test_loc_col <- test_loc_col %>% add_column(linetype = "solid")
test_measurements <- readRDS("data/test_measurements.RDS")
test_measurements <- test_measurements %>% add_column(sd = 5)

ui <- fluidPage(
  timeseries_output("timeseries_test")
)

server <- function(input, output, session) {
  timeseries_server("timeseries_test", test_measurements, test_loc_col)
}

shinyApp(ui, server)


  