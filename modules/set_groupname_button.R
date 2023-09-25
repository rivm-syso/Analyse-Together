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
                                    data_stns,
                                    data_other,
                                    col_names,
                                    col_overload) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # When the button is pushed then set new groupname
    observeEvent(input$set_new_group,{
      col_label_picked <- pick_color(data_stns(), col_names, col_overload)
      data_other$col_select <- col_label_picked$col_picked
      data_other$group_name <- col_label_picked$col_label
    })


  })
}
