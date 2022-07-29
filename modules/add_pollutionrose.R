###############################################
### PollutionRose-plot Module ###
###############################################

# Creates a bar-plot with the chosen stations, parameters and time range

###################################################################
### Output Module ####
#################################################################
pollrose_output <- function(id) {
  
  ns <- NS(id)
  
  plotOutput(ns("pollrose_plot"))
  
}


######################################################################
# Server Module
######################################################################
#
#

pollrose_server <- function(id, data_measurements_stations) {
  
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
    
    output$pollrose_plot <- renderPlot({
      
      # Determine parameter that needs to be plotted
      parameter_sel <- data_measurements()$parameter
      
      # Find the corresponding label
      parameter_label <- str_replace(toupper(parameter_sel[1]), '_', ' - ')
      
      data_pollrose <- data_measurements() %>% filter(parameter == parameter_sel)
      data_pollrose <- merge(data_pollrose, data_stations(), by = 'station')
      
      theme_plots <- theme_bw(base_size = 18) + 
        theme(strip.text.x = element_text(size = 14, colour = "black"),
              axis.text.y = element_text(face = "bold",color = "black", size = 16),
              axis.text.x = element_text(color = "black", size = 12),
              axis.title = element_text(color = "black", size = 16),
              text = element_text(family = 'serif'),
              title = element_text(face = "bold",color = "black", size = 16),
              legend.title = element_text(size = 16),
              legend.text = element_text(size = 16),
              legend.key.height = unit(0.5, 'cm'),
              legend.key.width = unit(1, 'cm'),
              panel.border = element_rect(colour = "black", fill=NA, size=1))
      
      # Make a plot
      data_pollrose$ws <- sample(seq(0, 10, length.out=nrow(data_pollrose)))
      data_pollrose$wd <- sample(seq(0, 359, length.out=nrow(data_pollrose)))
      data_pollrose$value <- as.numeric(data_pollrose$value)
      
      if (length(parameter_sel>0)){
          pollutionRose(data_pollrose,
                        pollutant = "value", 
                        wd = "wd", 
                        ws = "ws", 
                        type = 'station', 
                        local.tz="Europe/Amsterdam", 
                        cols = "Oranges", 
                        statistic = 'prop.mean', 
                        breaks=c(0,10,25,50,100,200),
                        par.settings=list(fontsize=list(text=15)),
                        key = list(header = paste0(unique(parameter_sel)),
                                   footer = '',
                                   labels = c('0 to 10', '10 to 25', 
                                              '25 to 50','50 to 100', '100 or more')),
                        between = list(x=0.5, y = 0.5))
      }
      else{
          verbatimTextOutput("Selecteer een sensor.")
      }
        
    })
    
  })
  
  
}





