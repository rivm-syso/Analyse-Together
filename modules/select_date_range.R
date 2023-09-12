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
                              data_other,
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
                     format = "dd-mm-yyyy",
                     separator = " - "
                   )

              )})


               observeEvent(input$date_range,{

                 data_other$start_date <- input$date_range[1]
                 data_other$end_date <- input$date_range[2]

               })

      })
}
