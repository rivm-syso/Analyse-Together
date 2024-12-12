###############################################
### information select manual button ###
###############################################

# This is a module show pop-up with some information about how to select
# sensors on the map
######################################################################
# Output Module
######################################################################

info_select_map_button_output <- function(id) {

  ns <- NS(id)
  tagList(
    actionButton(ns("info_selectmap_button"),
                 label = i18n$t("word_select_map"),
                 icon = icon("square"),
                 style="background-color: #ffe9b7",
                 width = "200px"
    )
  )
}

######################################################################
# Server Module
######################################################################

info_select_map_button_server <- function(id,
                               infotext) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # When the button is pushed then show text
    observeEvent(input$info_selectmap_button,{

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
