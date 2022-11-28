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
                              list_start_end
                              ) {

  moduleServer(id, function(input, output, session) {

              ns <- session$ns

              output$date_range <- renderUI({

                 # Get the boundaries of the datepicker
                 date_total <- list_start_end

                 log_trace("mod date_range: date total = {date_total[[1]]} - {date_total[[2]]}")

                 # Create the datepicker
                 tagList(

                   dateRangeInput(
                     ns("date_range"),
                     label = i18n$t("sel_date"),
                     start = date_total$start_time,
                     end = date_total$end_time,
                     max = date_total$end_time,
                     format = "dd-mm-yyyy",
                     separator = " - "
                   )

              )})

               observe({
                   x1 <- input$date_range[1]
                   x2 <- input$date_range[2]
                   log_trace("mod data_range: date selected {x1} - {x2}")
               })

               return(list(
                 selected_start_date = reactive({input$date_range[1]}),
                 selected_end_date = reactive({input$date_range[2]}))
               )
      })
}
