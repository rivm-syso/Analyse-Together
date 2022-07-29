###############################################
### Polar Annulus-plot Module ###
###############################################

# Creates a polar annulus-plot with the chosen stations, parameters and time range

###################################################################
### Output Module ####
#################################################################
polarann_output <- function(id) {
  
  ns <- NS(id)
  
  plotOutput(ns("polarann_plot"))
  
}


######################################################################
# Server Module
######################################################################
#
#

polarann_server <- function(id, data_measurements_stations) {
  
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
      data_stations <- data_measurements_stations$station_locations() %>% select(c(station, col)) %>% dplyr::distinct(station, .keep_all = T)
      return(data_stations)
    })
    
    output$polarann_plot <- renderPlot({
      
      # Determine parameter that needs to be plotted
      parameter_sel <- data_measurements()$parameter
      
      # Find the corresponding label
      parameter_label <- str_replace(toupper(parameter_sel[1]), '_', ' - ')
      
      data_polarann <- data_measurements() %>% filter(parameter == parameter_sel)
      data_polarann <- merge(data_polarann, data_stations(), by = 'station')
      
      # Make a plot
      data_polarann$value <- as.numeric(data_polarann$value)
      data_polarann$ws <- sample(seq(0, 10, length.out=nrow(data_polarann)))
      data_polarann$wd <- sample(seq(0, 359, length.out=nrow(data_polarann)))
      
      if (length(parameter_sel>0)){
        polarAnnulus(data_polarann, period = "hour",
                      pollutant = "value", local.tz="Europe/Amsterdam"
                      ) 
        }
      else{
          verbatimTextOutput("Selecteer een sensor.")
      }
        
    })
    
  })
  
  
}





