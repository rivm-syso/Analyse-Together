
# Define server logic                                                       ====
shinyServer(function(global, input, output, session) {
  
  # The dateRangeInput for date range selection 
  select_date_range <- date_range_server("select_date_range")
  
  # The pickerInput for component selection 
  select_component <- component_selection_server("select_component")
  
})
