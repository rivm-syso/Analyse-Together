###############################################
### dateRangeInput - select start and end date ###
###############################################

# This is a date range selection module
######################################################################
# Output Module
######################################################################

date_range_output <- function(id, start_date, end_date) {
  
  ns <- NS(id)
  
  tagList(
    
    dateRangeInput(
      ns("date_range"), 
      label = "Select date range", 
      start = start_date, 
      end = end_date, 
      min = start_date,
      max = end_date, 
      format = "dd-mm-yyyy", 
      separator = " - "
    )
    
  )
  
}


######################################################################
# Server Module
######################################################################

date_range_server <- function(id) {
  
  moduleServer(id, 
               
               return(list(
                 selected_start_date = reactive({input$date_range[1]}), 
                 selected_end_date = reactive({input$date_range[2]})
                 
               )))
}
