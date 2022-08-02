###############################################
### pickerInput - select component ###
###############################################

# This is a municipality selection module
######################################################################
# Output Module
######################################################################

choice_selection_output <- function(id) {
  
  ns <- NS(id)
  
  uiOutput(ns("choice_select"))
  
}


######################################################################
# Server Module
######################################################################

choice_selection_server <- function(id, com_module, mun_choices, proj_choices) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    get_choice_select <- reactive({
      choice_select <- com_module$choice_select()
      return(choice_select)})
    

    output$choice_select <- renderUI({
      
      if (is.null(get_choice_select()) == FALSE){
          
          if (get_choice_select() == "Municipality"){
              # Create the component picker with a list of possible choices
              tagList(
                
                pickerInput(
                  ns("choice_select"),
                  selected = '',
                  label    = "Choose one option:",
                  choices  = mun_choices
                )
              )}
          else{
                tagList(
            
                pickerInput(
                  ns("choice_select"),
                  selected = '',
                  label    = "Choose one option:",
                  choices  = proj_choices
                ))}}
    })
    
    # Return the chosen component
    return(selected_choice = reactive({input$choice_select}))
    
  })
  
}
