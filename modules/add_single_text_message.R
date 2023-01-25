###############################################
### Single text message ###
###############################################

# This is a module to a single text message if data is available
######################################################################
# Output Module
######################################################################

single_text_output <- function(id) {

  ns <- NS(id)
  tagList(
    textOutput(ns("message"))
  )
}

######################################################################
# Server Module
######################################################################

single_text_server <- function(id,
                              text_message
) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    output$message <- renderText({
      # text_message$data_in_dbs
      text_message()
    })
  })
}
