###############################################
### Slider time zoom Module ###
###############################################

# Functionalitiy to zoom in in a plot using this slider

###################################################################
### Output Module ####
#################################################################

slider_zoom_output <- function(id) {

  ns <- NS(id)

  uiOutput(ns("slider_zoom"))

}


######################################################################
# Server Module
######################################################################

slider_zoom_server <- function(id,
                               data_other,
                               min_date,
                               max_date,
                               tzone_man = "Europe/Amsterdam"
){

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    output$slider_zoom <- renderUI({

      # Create the slider
      tagList(

        sliderInput(
          ns("slider_zoom"),
          label = "",
          min = min_date() - days(1),
          max = max_date() + days(1),
          value = c(min_date(), max_date()),
          timeFormat = "%d/%b/%y",
          timezone = tzone_man
        )

      )})

    # Check if the slider is used and save to gobal reactivevalues
    observeEvent(input$slider_zoom,{
      # NB for the plotting a posixct is expected
      data_other$start_slider_zoom <- input$slider_zoom[1] %>%
        as.POSIXct() %>%
        force_tz(tzone = tzone_man )
      data_other$end_slider_zoom <- input$slider_zoom[2] %>%
        as.POSIXct() %>%
        force_tz(tzone = tzone_man )


    })

  })
}
