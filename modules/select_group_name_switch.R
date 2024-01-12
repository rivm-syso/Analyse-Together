###############################################
### Switch group name ###
###############################################

# This is a module to switch group name
######################################################################
# Output Module
######################################################################

switch_group_output <- function(id) {

  ns <- NS(id)
  uiOutput(ns("switch_menu"))
}

######################################################################
# Server Module
######################################################################

switch_group_server <- function(id,
                                data_other) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    output$switch_menu <- renderUI({
      tagList(
        pickerInput(
          ns("switch_menu"),
          choices = data_other$combi_col_name
        )
      )
    })

    # When the button is pushed then set new groupname
    observeEvent(input$switch_menu,{
      # Change the current group assignment
      data_other$group_name <- names(which(data_other$combi_col_name == input$switch_menu))
      colour_switch <- input$switch_menu
      data_other$col_select <- colour_switch
    })


  })
}
