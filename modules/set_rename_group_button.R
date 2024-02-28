###############################################
### Rename group button ###
###############################################

# This is a module to rename the group
######################################################################
# Output Module
######################################################################

rename_group_button_output <- function(id) {

  ns <- NS(id)
  tagList(
    actionButton(ns("rename_group"),
                 i18n$t("btn_rename_group"),
                 style="background-color: #ffe9b7",
                 width = "200px",
                 icon = icon("pen-to-square")
                 )
  )
}

######################################################################
# Server Module
######################################################################

rename_group_button_server <- function(id,
                                       data_stations,
                                        data_other) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # Function for inside the pop-up, to change the reactiveValues
    # with the new name
     set_new_name <- function(value) {
      # Get the name and colour to be changed
      old_groupname <- data_other$group_name
      old_colour <- data_other$col_select

      # Change the new group name to the corresponding variables
      data_other$group_name <- value
      # Remove old name
      data_other$combi_col_name <-
      data_other$combi_col_name[! data_other$combi_col_name %in%
                                  old_colour]
      # Add new name
      data_other$combi_col_name <- setNames(old_colour, value) %>%
      append(data_other$combi_col_name)

      # Change the new group name to the stations
      data_stations$data <- data_stations$data %>%
      dplyr::mutate(group_name = ifelse(group_name == old_groupname,
                                        value,
                                        group_name))

      # Show new shinyalert (pop-up) to mention new name
      shinyalert(paste("Naam succesvol gewijzigd naar: ", value))

      }

    # When the button is pushed then set new groupname
    observeEvent(input$rename_group,{

      # Get the came to be changed
      old_groupname <- data_other$group_name

      # Show pop up to change the name (user input)
      shinyalert(
        text = paste0("Geef nieuwe naam voor groep: ", old_groupname),
        type = "input",
        callbackR = set_new_name
        )


    })


  })
}
