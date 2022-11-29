update_data_button_output <- function(id) {

  ns <- NS(id)
  tagList(
  actionButton(ns("update_data_button"), "Update data", style="background-color: #ffe9b7")
  )
}


update_data_button_server <- function(id) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    btn <- eventReactive(input$update_data_button, {T})
    return(btn)


  })
}
