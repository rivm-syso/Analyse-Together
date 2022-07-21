###############################################
### pickerInput - select component ###
###############################################

# This is a municipality selection module
######################################################################
# Output Module
######################################################################

municipality_selection_output <- function(id) {
  
  ns <- NS(id)
  
  uiOutput(ns("mun_select"))
  
}


######################################################################
# Server Module
######################################################################

municipality_selection_server <- function(id, mun_choices) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    output$mun_select <- renderUI({
      
      # Create the component picker with a list of possible choices
      tagList(
        
        pickerInput(
          ns("mun_select"),
          selected = '',
          label    = "Select Municipality",
          choices  = mun_choices
        )
      )
    })
    
    # Return the chosen component
    return(selected_municipality = reactive({input$mun_select}))
    
  })
  
}

