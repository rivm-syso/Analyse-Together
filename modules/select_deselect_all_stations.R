###############################################
### Deselect all stations button ###
###############################################

# This is a module which deselects all stations
######################################################################
# Output Module
######################################################################

deselect_all_button_output <- function(id) {

  ns <- NS(id)
  tagList(
    actionButton(ns("deselect_all"),
                 i18n$t("btn_deselect_all"),
                 style="background-color: #ffe9b7",
                 width = "200px",
                 icon = icon("object-ungroup"))
  )
}

######################################################################
# Server Module
######################################################################

deselect_all_button_server <- function(id,
                                     group_name_none,
                                     data_stations,
                                     col_default,
                                     line_default
                                     ) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # When the button is pushed then select all stations (incl. knmi + ref)
    observeEvent(input$deselect_all,{
      # Get all the stations names from the stations with data
      id_select <- data_stations$data %>% dplyr::pull(station) %>% unique()

      # Set them to selected
      data_stations$data <- change_state_to_deselected(data_stations$data,
                                                       id_select,
                                                       group_name_none ,
                                                       col_default,
                                                       line_default
                                                      )

    })


  })
}
