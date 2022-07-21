###############################################
### pickerInput - select component ###
###############################################

# This is a project selection module
######################################################################
# Output Module
######################################################################

project_or_mun_selection_output <- function(id) {
  
  ns <- NS(id)
  
  uiOutput(ns("proj_or_mun_select"))
  
}


######################################################################
# Server Module
######################################################################

project_or_mun_selection_server <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    output$proj_or_mun_select <- renderUI({
      
      # Create the component picker with a list of possible choices
      tagList(
        
        pickerInput(
          ns("proj_or_mun_select"),
          label    = "Selection based on:",
          choices  = c("Project", "Gemeente"),
          selected = F
        )
      )
    })
    
    # Return the chosen component
    return(selected_project = reactive({input$proj_or_mun_select}))
    
  })
  
}

