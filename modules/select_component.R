###############################################
### pickerInput - select component ###
###############################################

# This is a component selection module
######################################################################
# Output Module
######################################################################

component_selection_output <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    pickerInput(
      ns("component"),
      label    = "Select component",
      choices  = c("PM10", "PM10 - calibrated", "PM2.5", "PM2.5 - calibrated"),
      selected = "PM10",
      width    = 150
      )
    
  )
  
}


######################################################################
# Server Module
######################################################################

component_selection_server <- function(id) {
  
  moduleServer(id, 
               
               return(selected_component = reactive({input$component}))
               
               )
}
