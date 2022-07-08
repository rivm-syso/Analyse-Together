###############################################
### dateRangeInput - select start and end date ###
###############################################

# This is a date range selection module
######################################################################
# Output Module
######################################################################

date_range_output <- function(id) {

  ns <- NS(id)
  uiOutput(ns("date_range"))


}


######################################################################
# Server Module
######################################################################

date_range_server <- function(id,
                              comm_date #module
                              ) {

  moduleServer(id, function(input, output, session) {

               ns <- session$ns

               # Get the min and max of the dataset
               get_date_total <- reactive({
                 date_total <- comm_date$start_end_total()
                 return(date_total)

               })

               output$date_range <- renderUI({
                 # Get the boundaries of the datepicker
                 date_total <- get_date_total()
                 log_trace("mod date_range: date total = {date_total[[1]]} - {date_total[[2]]}")

                 # Create the datepicker
                 tagList(

                   dateRangeInput(
                     ns("date_range"),
                     label = "Select date range",
                     start = date_total$start_time,
                     end = date_total$end_time,
                     min = date_total$start_time,
                     max = date_total$end_time,
                     format = "dd-mm-yyyy",
                     separator = " - "
                   )

                 )
               })

               observe({ #loggin
                   x1 <- input$date_range[1]
                   x2 <- input$date_range[2]
                   log_trace("mod data_range: date selected {x1} - {x2}")
               })


               res  <- list(
                 selected_start_date = reactive({input$date_range[1]}),
                 selected_end_date = reactive({input$date_range[2]})
                 )

               return(res)
               })
}
