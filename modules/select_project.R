###############################################
### pickerInput - select component ###
###############################################

# This is a project selection module
######################################################################
# Output Module
######################################################################

project_selection_output <- function(id) {
  
  ns <- NS(id)
  
  uiOutput(ns("proj_select"))
  
}


######################################################################
# Server Module
######################################################################

project_selection_server <- function(id, proj_choices) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    output$proj_select <- renderUI({
      
      # Create the component picker with a list of possible choices
      tagList(
        
        pickerInput(
          ns("proj_select"),
          label    = "Select Project",
          choices  = proj_choices
        )
      )
    })
    
    # Return the chosen component
    return(selected_project = reactive({input$proj_select}))
    
  })
  
}

