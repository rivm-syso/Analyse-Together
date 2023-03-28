###############################################
### Set new group button ###
###############################################

# This is a module which set a new group name
######################################################################
# Output Module
######################################################################

set_group_button_output <- function(id) {

  ns <- NS(id)
  tagList(
    actionButton(ns("set_new_group"), i18n$t("btn_create_group"), style="background-color: #ffe9b7")
  )
}

######################################################################
# Server Module
######################################################################

set_group_button_server <- function(id,
                                   data_other) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # When the button is pushed then set new groupname
    observeEvent(input$set_new_group,{
      # Get number of the previous group and add 1
      previous_number <- data_other$group_number
      new_number <- previous_number + 1
      data_other$group_number <- new_number

      # Create the new group, with a default name and the new number
      data_other$group_name <- paste0("group_", new_number)
    })

  return(list(
    group_number = reactive({data_other$group_number}),
    group_name = reactive({data_other$group_name})
  ))
  })
}
