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
                     width = "500px",
                     format = "dd MM yyyy",
                     separator = " - ",
                     startview = "year",
                     language = data_other$lang
                   ),

                   # Hidden text appended to the start/end inputs' accessible
                   # name, so screen readers announce which field is which
                   # (both share the same visible label otherwise).
                   tags$span(id = ns("date_range_start_label"), class = "sr-only", i18n$t("sel_date_start")),
                   tags$span(id = ns("date_range_end_label"), class = "sr-only", i18n$t("sel_date_end")),
                   tags$script(HTML(sprintf(
                     "(function() {
                        var inputs = document.querySelectorAll('#%s .input-daterange input');
                        if (inputs.length === 2) {
                          [['%s'], ['%s']].forEach(function(ids, i) {
                            var current = inputs[i].getAttribute('aria-labelledby') || '';
                            inputs[i].setAttribute('aria-labelledby', (current + ' ' + ids[0]).trim());
                          });
                        }
                      })();",
                     ns("date_range"),
                     ns("date_range_start_label"),
                     ns("date_range_end_label")
                   )))

              )})


               observeEvent(input$date_range,{
                 data_other$start_date_choose <- input$date_range[1]
                 data_other$end_date_choose <- input$date_range[2]

               })

      })
}
