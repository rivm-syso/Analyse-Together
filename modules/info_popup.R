###############################################
### Information group button ###
###############################################

# This is a module show pop-up with some information/explanation
######################################################################
# Output Module
######################################################################

info_button_output <- function(id) {

  ns <- NS(id)
  tagList(
    actionButton(ns("info_button"),
                 label = "",
                 icon = icon("info")
    )
  )
}

######################################################################
# Server Module
######################################################################

info_button_server <- function(id,
                               infotext) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # When the button is pushed then show text
    observeEvent(input$info_button,{

      # Show pop up with text/explanation
      shinyalert(
        text = infotext,
        confirmButtonText = "OK",
        confirmButtonCol = "#ffb612",
        closeOnClickOutside = TRUE
      )


    })


  })
}
